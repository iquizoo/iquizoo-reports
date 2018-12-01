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
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(iquizoor))

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
db_config <- config::get("database", config = params$customer_id)
customer_projectids <- config::get("customer.projectid", config = params$customer_id) %>%
  paste(collapse = ",")
iquizoo_db <- dbConnect(
  MySQL(),
  host = db_config$host,
  user = db_config$user,
  password = db_config$password,
  dbname = "eval_core"
)
dbExecute(iquizoo_db, "SET character_set_results = gbk;")
users <- dbGetQuery(
  iquizoo_db,
  getOption("reports.mysql.querydir") %>%
    file.path("users.sql") %>%
    read_file() %>%
    str_glue()
) %>%
  spread(prop_key, prop_value)
scores <- dbGetQuery(
  iquizoo_db,
  getOption("reports.mysql.querydir") %>%
    file.path("scores.sql") %>%
    read_file() %>%
    str_glue()
)
abilities <- dbReadTable(iquizoo_db, "ability")
dbDisconnect(iquizoo_db)
# combine score results and user information to form report dataset
dataset <- users %>%
  left_join(scores) %>%
  # complete cases of ability ids
  complete(
    abId,
    nesting(!!!syms(c(names(users), "examId", "createDate", "bci")))
  ) %>%
  filter(!is.na(abId)) %>%
  left_join(select(abilities, id, code), by = c("abId" = "id")) %>%
  select(-abId) %>%
  spread(code, score) %>%
  # user name with alpha/number is invalid
  filter(!str_detect(name, "(\\d|[a-zA-Z])+|无效|未用")) %>%
  # remove invalid duplicate users (keep the newest)
  group_by(userId) %>%
  mutate(occurrence = row_number(desc(createDate))) %>%
  filter(is.na(occurrence) | occurrence == 1) %>%
  select(-occurrence) %>%
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
