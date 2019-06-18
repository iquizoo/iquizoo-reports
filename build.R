# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(iquizoor))
suppressPackageStartupMessages(library(dataprocr))

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
# connect to database and download data
customer_projectids <- config::get("customer.projectid", config = params$customer_id)
customer_projectids <- paste0("\"", customer_projectids, "\"", collapse = ",")
iquizoo_report_db <- dbConnect(
  odbc::odbc(), "iquizoo-v3",
  database = "iquizoo_report_db"
)
iquizoo_user_db <- dbConnect(
  odbc::odbc(), "iquizoo-v3",
  database = "iquizoo_user_db"
)
game_data <- dbGetQuery(
  iquizoo_report_db,
  getOption("reports.mysql.querydir") %>%
    file.path("scores.sql") %>%
    read_file() %>%
    str_glue()
)
users <- dbGetQuery(
  iquizoo_user_db,
  getOption("reports.mysql.querydir") %>%
    file.path("users.sql") %>%
    read_file() %>%
    str_glue()
)
dbDisconnect(iquizoo_report_db)
dbDisconnect(iquizoo_user_db)
# calculate scores
data_configs <- jsonlite::read_json("config.json", simplifyVector = TRUE)
norms <- read_tsv("assets/extra/norms.tsv") %>%
  filter(n > 10, gender == "全部") %>%
  select(-n, -gender) %>%
  group_by(game_name) %>%
  nest() %>%
  mutate(
    data = map(
      data,
      ~ .x %>%
        arrange(age) %>%
        bind_rows(.x[nrow(.x), ]) %>%
        mutate(age = replace(age, n(), 15))
    )
  ) %>%
  unnest(data)
user_prop_used <- c("user_id", "user_name", "gender", "school", "grade", "class")
subability_scores <- users %>%
  left_join(game_data, by = "user_id") %>%
  filter(!str_detect(user_name, "测试")) %>%
  left_join(data_configs, by = "game_name") %>%
  mutate(
    game_data = map(
      game_data,
      ~ if (is.na(.x) || .x == "") {
        NULL
      } else {
        jsonlite::fromJSON(.x)
      }
    ),
    score = map2_dbl(
      game_data, fun,
      ~ if (is.null(.x)) {
        NA_real_
      } else {
        get(.y)(.x)
      }
    ),
    age = round((dob %--% part_time) / dyears(1))
  ) %>%
  left_join(norms, by = c("game_name", "age")) %>%
  mutate(std_score = (score - avg) / std * 15 + 100) %>%
  complete(ability, nesting(!!!syms(c(names(users))))) %>%
  filter(!is.na(ability)) %>%
  group_by(!!!syms(user_prop_used)) %>%
  mutate(part_time = min(part_time)) %>%
  complete(ability, nesting(!!!syms(names(users)))) %>%
  filter(!is.na(ability)) %>%
  group_by(!!!syms(c(user_prop_used, "part_time", "ability"))) %>%
  summarise(
    score = round(mean(std_score, na.rm = TRUE))
  ) %>%
  group_by(ability) %>%
  mutate(
    score = if_else(
      score %in% boxplot.stats(score)$out,
      NA_real_, score
    )
  ) %>%
  ungroup()
total_scores <- subability_scores %>%
  group_by(!!!syms(c(user_prop_used, "part_time"))) %>%
  summarise(score = round(mean(score, na.rm = TRUE))) %>%
  add_column(ability = "blai") %>%
  ungroup()
dataset <- bind_rows(subability_scores, total_scores) %>%
  spread(ability, score)

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
