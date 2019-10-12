# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
library(tidyverse)
library(DBI)
library(ggthemes)
library(extrafont)
library(lubridate)
library(optparse)
library(iquizoor)
library(dataprocr)

# get the configuration parameters used in report generation ----
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
# load dataset configurations
config_game <- jsonlite::read_json("config_game.json", simplifyVector = TRUE)
organization_names <- str_c(
  "\"", config::get("customer.organizations"), "\"",
  collapse = ","
)
# load dataset and configurations
database <- dbConnect(
  odbc::odbc(), "iquizoo-v3",
  database = "iquizoo_user_db"
)
raw_data <- dbGetQuery(
  database,
  read_file(
    file.path(
      getOption("reports.mysql.querydir"),
      "scores.sql"
    )
  ) %>%
    str_glue()
)
users <- dbGetQuery(
  database,
  read_file(
    file.path(
      getOption("reports.mysql.querydir"),
      "users.sql"
    )
  ) %>%
    str_glue()
)
# calculate game scores
scores_item <- raw_data %>%
  left_join(config_game, by = "game_name") %>%
  mutate(game_data = map(game_data, jsonlite::fromJSON)) %>%
  mutate(
    score = pmap_dbl(
      .,
      function(game_data, fun, game_duration, ...) {
        get(fun)(game_data, game_duration / 60 / 1000)
      }
    )
  ) %>%
  # only keep the latest score
  group_by(user_id, game_name, ability) %>%
  summarise(
    score = score[which.max(game_time)],
    game_time = max(game_time)
  ) %>%
  group_by(game_name) %>%
  mutate(std_score_orig = scale(score) * 15 + 100) %>%
  ungroup() %>%
  mutate(
    std_score = case_when(
      is.infinite(std_score_orig) ~ NA_real_,
      std_score_orig > 150 ~ 150,
      std_score_orig < 50 ~ 50,
      TRUE ~ std_score_orig
    )
  ) %>%
  left_join(users, by = "user_id") %>%
  group_by(user_id) %>%
  mutate(assess_time = min(game_time)) %>%
  ungroup()
scores_subability <- scores_item %>%
  group_by(user_id, assess_time, ability) %>%
  summarise(score = mean(std_score, na.rm = TRUE)) %>%
  ungroup()
scores_total <- scores_subability %>%
  group_by(user_id, assess_time) %>%
  summarise(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  add_column(ability = "blai")
scores <- bind_rows(scores_subability, scores_total) %>%
  mutate(score = round(score)) %>%
  spread(ability, score)
dataset <- users %>%
  left_join(scores, by = "user_id") %>%
  unite(full_class, grade, class, sep = "", remove = FALSE) %>%
  filter(full_class != "特别测试")

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
