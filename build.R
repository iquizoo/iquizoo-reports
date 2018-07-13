# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))

# get the configuration parameters used in report generation ----
if (!interactive()) {
  # parse command line argument if not in interactive mode
  # specify our desired options in a list
  # by default OptionParser will add an help option equivalent to
  # make_option(c("-h", "--help"), action="store_true", default=FALSE,
  #               help="Show this help message and exit")
  option_list <- list(
    make_option(
      c("-t", "--report-type"),
      help = "Required. Specify the report type, could be 'one', 'school' or 'district'."
    ),
    make_option(
      c("-c", "--customer-id"),
      help = paste(
        "Required. Specify the customer identifier for reporting.",
        "This signifies because it will be used to identify datasets and descriptions.",
        "The customer `id``, `type` and `name` should be able to be found in the configuration file `config.yml`."
      )
    ),
    make_option(
      c("-d", "--date-manual"),
      help = paste(
        "Optional. Whether the dates in the report (report data and test date) are set manually?",
        "Use \"report\", \"test\" or \"all\" to manually set one or two dates.",
        "Or do not set it to if all the dates should be set automatically.",
        "When set, the corresponding report-date or test-date are required."
      )
    ),
    make_option(c("--report-date"), help = "Required conditionally. The report date."),
    make_option(c("--test-date"), help = "Required conditionally. The test date.")
  )
  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  params <- parse_args(OptionParser(option_list = option_list), convert_hyphens_to_underscores = TRUE)
} else {
  # read configurations from yaml config file if in interactive mode (this is used for quick preview or debug)
  params <- read_yaml("params.yml")
}

# environmental settings ----
# source user script, which will be placed in script path
# TODO: write a package
source(
  file.path(config::get("include.path")$script, "utils.R"),
  encoding = config::get("encoding")
)
# import font if not found
text_family <- config::get("text.family")
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}
# get the content of all the configuration files
# report intro: context template
context_tmpl <- get_config("context", params$customer_id, params$report_type, ext = "Rmd")
# report body: body template
body_tmpl <- get_config("body", params$customer_id, params$report_type, ext = "Rmd")
# report ending: suggestion template
suggestion_tmpl <- get_config("suggestion", params$customer_id, params$report_type, ext = "Rmd")
# descriptions, or the content builder
descriptions <- get_config("descriptions", params$customer_id, params$report_type)
# ability information preparation
ability_info <- as_tibble(descriptions$ability) %>%
  mutate(ability = "general")
component_info <- descriptions$components %>%
  map(as_tibble) %>%
  bind_rows(.id = "ability")
# generate report sequence of abilities (names and nameids)
ability_names_id <- character()
for (ability_general in ability_info$nameid) {
  ability_names_id <- c(
    ability_names_id, ability_general,
    component_info %>%
      filter(ability == ability_general) %>%
      pull(nameid)
  )
}
# generate markdown for ability info
ability_md <- rbind(ability_info, component_info) %>%
  mutate(
    md = render_title_content(
      title = name, content = description,
      hlevel = hlevel, style = style
    )
  )

# dataset preparations ----
# connect to database and download data
iquizoo_db <- dbConnect(
  SQLite(),
  dbname = file.path(
    config::get("include.path")$database,
    "iquizoo.sqlite"
  )
)
scores_origin <- tbl(iquizoo_db, params$region) %>%
  left_join(tbl(iquizoo_db, "users")) %>%
  left_join(tbl(iquizoo_db, "abilities")) %>%
  filter(!is.na(score)) %>%
  collect()
dbDisconnect(iquizoo_db)
# count number of school, grade and users
n_school <- n_distinct(scores_origin$school)
n_grade <- n_distinct(scores_origin$grade)
n_user <- n_distinct(scores_origin$userId)
# reconfigure `school_name` based on the dataset
if (is.null(params$school_name)) {
  school_names <- unique(scores_origin$school)
} else {
  school_names <- params$school_name
}
if (params$type == "school") {
  # validate shcool names
  if (!all(school_names %in% scores_origin$school)) {
    stop("School not found!")
  }
}

# build the three parts of the report ----
switch(
  params$type,
  school = {
    for (school_name in school_names) {
      # filter out scores for current school
      scores_school <- scores_origin %>%
        filter(school == school_name)
      if (n_school > 1) {
        # school are compared with district
        scores_combined <- list(
          本区 = scores_origin,
          本校 = scores_school,
          各班 = scores_school
        ) %>%
          bind_rows(.id = "region") %>%
          mutate(cls = if_else(region != "各班", region, cls)) %>%
          mutate(region = factor(region, c("各班", "本校", "本区")))
      } else {
        # no need to compare school and district
        scores_combined <- list(
          本校 = scores_school,
          各班 = scores_school
        ) %>%
          bind_rows(.id = "region") %>%
          mutate(cls = if_else(region != "各班", region, cls)) %>%
          mutate(region = factor(region, c("各班", "本校")))
      }
      # set dates
      attach(set_date(params, test_date = median(as.Date(scores_school$firstPartTime))))
      render_report(output_file = glue("{school_name}.docx"), clean_envir = FALSE)
    }
  },
  district = {
    # use schoolCovert not school
    scores_origin <- scores_origin %>%
      rename(schoolOvert = school, school = schoolCovert)
    # combine data from whole district, each school
    scores_combined <- list(
      本区 = scores_origin,
      各校 = scores_origin
    ) %>%
      bind_rows(.id = "region") %>%
      mutate(school = if_else(region != "各校", region, school)) %>%
      mutate(region = factor(region, c("本区", "各校")))
    # set dates
    attach(set_date(params, test_date = median(scores_origin$firstPartTime)))
    render_report(output_file = glue("{region}.docx"), clean_envir = FALSE)
  },
  one = {
    # set dates
    attach(set_date(params, test_date = median(scores_origin$firstPartTime)))
    render_report(output_file = glue("{region}统一.docx"), clean_envir = FALSE)
  },
  stop("Unsupported report type! Please specify as school/district only.")
)
