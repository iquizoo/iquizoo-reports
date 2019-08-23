# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
library(tidyverse)
library(extrafont)
library(lubridate)
library(optparse)
library(dbplyr)
library(ggthemes)
library(DBI)
library(iquizoor)
library(dataprocr)
library(log4r)

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
  message(str_glue("Will use default title ('{title}') for report unit '{params$report_unit}'."))
}
subtitle_config <- config::get("report.subtitle")
if (hasName(subtitle_config, params$report_unit)) {
  subtitle <- subtitle_config[[params$report_unit]]
} else {
  subtitle <- subtitle_config[["default"]]
  message(str_glue("Will use default subtitle ('{subtitle}') for report type '{params$report_unit}'."))
}

# dataset preparations ----
# connect to database and download data
iquizoo_db <- dbConnect(
  odbc::odbc(), "iquizoo-old",
  database = "eval_core"
)
game_data <- dbGetQuery(
  iquizoo_db,
  getOption("reports.mysql.querydir") %>%
    file.path("scores_bbp.sql") %>%
    read_file()
)
users <- dbGetQuery(
  iquizoo_db,
  getOption("reports.mysql.querydir") %>%
    file.path("users_bbp.sql") %>%
    read_file()
) %>%
  spread(prop_key, prop_value) %>%
  filter(!str_detect(desc, "备用")) %>%
  select(user_id, name, gender, birthDay, school, grade, cls)
ability <- dbReadTable(iquizoo_db, "ability") %>%
  select(id, code, pid)
dbDisconnect(iquizoo_db)
# calculate ability scores
scores_ability <- game_data %>%
  left_join(ability, by = c("game_ability" = "id")) %>%
  mutate(
    game_score = case_when(
      game_score < 50 ~ 50,
      game_score > 150 ~ 150,
      TRUE ~ game_score
    )
  ) %>%
  group_by(user_id) %>%
  mutate(assess_date = median(game_time)) %>%
  group_by(user_id, assess_date, game_ability, pid) %>%
  summarise(score = mean(game_score)) %>%
  group_by(user_id, assess_date, pid) %>%
  summarise(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(ability, by = c("pid" = "id")) %>%
  select(user_id, assess_date, code, score) %>%
  group_by(code) %>%
  mutate(score = scale(score) * 15 + 100) %>%
  ungroup()
scores_total <- scores_ability %>%
  group_by(user_id, assess_date) %>%
  summarise(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  add_column(code = "bci")
scores <- bind_rows(scores_ability, scores_total) %>%
  mutate(score = round(score)) %>%
  spread(code, score)
dataset <- users %>%
  left_join(scores, by = "user_id") %>%
  mutate(
    grade = recode(
      grade,
      一 = "1", 二 = "2", 三 = "3",
      四 = "4", 五 = "5", 六 = "6",
      七 = "7", 八 = "8", 九 = "9"
    )
  ) %>%
  filter(!is.na(grade))

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
  message(str_glue("Debugging. The chosen report unit is {name_units}."))
  output_dir <- "test"
}
logger <- logger(appenders = file_appender(file.path("logs", "info.log")))
# rendering report for each unit
for (name_unit in name_units) {
  dataset_unit <- dataset %>%
    filter(school == name_unit)
  if (all(is.na(dataset_unit$assess_date))) {
    info(logger, str_glue("未找到数据：{name_unit}"))
    next
  }
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
