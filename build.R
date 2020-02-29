#!/usr/bin/env Rscript

# Copyright (C) 2018 Liang Zhang - All Rights Reserved
# @author Liang Zhang <psychelzh@outlook.com>
# @description This script is used to build collective reports for customers.

# get the configuration parameters used in report generation ----
library(optparse)
if (!interactive()) {
  # parse command line argument if not in interactive mode
  # specify our desired options in a list
  # by default OptionParser will add an help option equivalent to
  # make_option(c("-h", "--help"), action="store_true", default=FALSE,
  #               help="Show this help message and exit")
  option_list <- list(
    make_option(
      c("-c", "--customer-id"),
      help = paste(
        "Required. Specify the customer identifier for reporting.",
        "This signifies because it will be used to identify datasets and descriptions."
      )
    ),
    make_option(
      c("-u", "--report-unit"), default = "default",
      help = paste(
        "Optional. Specify the report unit, i.e., one of the properties of users.",
        "Default value is set as 'default'."
      )
    ),
    make_option(
      c("-t", "--report-type"), default = "default",
      help = paste(
        "Optional. Specify the report type, which is used to select which",
        "archetype Rmarkdown to use.",
        "Default value is 'default'."
      )
    ),
    make_option(
      c("-s", "--report-slices"),
      help = paste(
        "Optional. Specify the name(s) of organization(s), which is useful",
        "when you want to generate reports only for those of interest.",
        "No default value is set, which means generate reports for all."
      )
    ),
    make_option(
      c("-T", "--debug-test"), action = "store_true", default = FALSE,
      help = paste(
        "Optional. Used when in testing mode.",
        "When set, the program will choose one report unit randomly."
      )
    )
  )
  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  params <- parse_args(OptionParser(option_list = option_list), convert_hyphens_to_underscores = TRUE)
} else {
  # read configurations from yaml config file if in interactive mode (this is used for quick preview or debug)
  params <- config::get(file = "params.yml")
}

# command line arguments checking ----
if (is.null(params$customer_id)) {
  stop("Fatal error! You must specify the identifier of the customer.")
}

# load packages ----
library(tidyverse)
library(ggthemes)
library(extrafont)
library(lubridate)
library(cowplot)

# environmental settings ----
# package options
old_opts <- options(
  "yaml.eval.expr" = TRUE,
  "knitr.kable.NA" = "",
  "reports.archytype" = "archetypes",
  "reports.mysql.querydir" = "assets/sql"
)
# set default configuration as what the command line argument specifies
Sys.setenv(R_CONFIG_ACTIVE = params$customer_id)
# import font if not found
text_family <- config::get("text.family")
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}
# common variables used in reporting
customer_name <- config::get("customer.name")
title_config <- config::get("report.title")
if (hasName(title_config, params$report_unit)) {
  title <- title_config[[params$report_unit]]
} else {
  title <- title_config[["default"]]
  warning(
    str_glue("Will use default title ('{title}') for report unit '{params$report_unit}'."),
    immediate. = TRUE
  )
}
subtitle_config <- config::get("report.subtitle")
if (hasName(subtitle_config, params$report_unit)) {
  subtitle <- subtitle_config[[params$report_unit]]
} else {
  subtitle <- subtitle_config[["default"]]
  warning(
    str_glue("Will use default subtitle ('{subtitle}') for report type '{params$report_unit}'."),
    immediate. = TRUE
  )
}

# dataset preparations ----
# load configurations
norms <- read_tsv("assets/temp_dataset/norms.tsv")
config_school <- read_csv("assets/temp_dataset/config_school.csv")
ability_class <- read_tsv("assets/temp_dataset/ability_class.txt") %>%
  select(ab_type = AbilityType, ab_sum = Criteria)
# load dataset
raw_scores <- jsonlite::read_json("assets/temp_dataset/raw_scores.json", simplifyVector = TRUE)
user_info <- jsonlite::read_json("assets/temp_dataset/user_info.json", simplifyVector = TRUE)  %>%
  unite("full_class", grade, class, sep = "", remove = FALSE) %>%
  filter(
    !str_detect(user_name, "\\d"),
    !str_detect(full_class, "测试|体验"),
    !is.na(user_sex)
  ) %>%
  full_join(config_school, by = "school") %>%
  mutate(
    user_sex = factor(user_sex, c("男", "女")),
    school_type = factor(school_type, c("试点校", "对照校"))
  )
# calculate scores
item_scores <- user_info %>%
  inner_join(raw_scores, by = "user_id") %>%
  filter(
    ! school %in% c(
      '成都市青白江区外国语小学',
      '成都市双流区实验小学',
      '成都市青白江区实验小学',
      '成都经济技术开发区实验小学',
      '成都市温江区鹏程小学',
      '成都市玉林小学',
      '成都市新都区旃檀小学',
      '成都教科院附属学校'
    ) | ! course_id == "71b33c52-fbc1-40e5-9750-13194d1b0acc"
  ) %>%
  mutate(
    age = (user_dob %--% game_time) / dyears(),
    age_int = round(age)
  ) %>%
  left_join(norms, by = c("game_name", "age_int", "user_sex")) %>%
  mutate(
    score = (raw_score - AVG) / SD * 15 + 100,
    score_censored = case_when(
      score > 150 ~ 150,
      score < 50 ~ 50,
      TRUE ~ score
    )
  ) %>%
  filter(!is.na(score_censored))
ability_scores <- item_scores %>%
  group_by(user_id, game_id) %>%
  mutate(occur = row_number(desc(game_time))) %>%
  # keep the latest score only
  filter(occur == 1) %>%
  group_by(user_id) %>%
  mutate(assess_time = min(game_time)) %>%
  group_by(user_id, assess_time, ab_code, ab_name) %>%
  summarise(score = round(mean(score_censored))) %>%
  ungroup()

# build the three parts of the report ----
# there might be many reports based on the report unit
name_units <- switch(
  params$report_unit,
  default = "全体报告",
  unique(pull(dataset, params$report_unit))
)
# customize settings for debug and deploy
output_dir <- "targets"
if (params$debug_test) {
  name_units <- sample(name_units, 1)
  warning(
    str_glue("Debugging. The chosen report unit is {name_units}."),
    immediate. = TRUE
  )
  output_dir <- "test"
}
if (!is.null(params$report_slices) &&
    !identical(params$report_slices, "default")) {
  report_slices_existed <- params$report_slice %in% name_units
  if (!all(report_slices_existed)) {
    warning(
      str_glue("Some of the choosen name units are not found,
               using the found ones only."),
      immediate. = TRUE
    )
  }
  name_units <- params$report_slice[report_slices_existed]
}
# rendering report for each unit
for (name_unit in name_units) {
  # report rendering
  bookdown::render_book(
    "index.Rmd",
    output_file = str_glue("{customer_name}-{name_unit}.docx"),
    output_dir = output_dir,
    clean_envir = FALSE
  )
}

# restore options ----
options(old_opts)
