# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(extrafont))
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
      c("-c", "--customer-id"),
      help = paste(
        "Required. Specify the customer identifier for reporting.",
        "This signifies because it will be used to identify datasets and descriptions.",
        "The corresponding customer `type` and `name` should be found in the configuration file `config.yml`."
      )
    ),
    make_option(
      c("-t", "--report-type"),
      help = paste(
        "Optional. Used when the customer(s) need different versions of reports.",
        "Default value is not set, i.e. `NULL`, and the report type will be the same as customer type.",
        "Change it to report different types of report, e.g., a `region` report for `school` customer."
      )
    ),
    make_option(
      c("-d", "--date-manual"), default = "none",
      help = paste(
        "Optional. Whether the dates in the report (report data and test date) are set manually?",
        "Use \"none\" (default), \"report\", \"test\" or \"all\" to manually set one or two dates.",
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
  params <- config::get(file = "params.yml")
}

# check command line arguments ----
if (is.null(params$customer_id)) {
  stop("Fatal error! You must specify the identifier of the customer.")
}
if (!params$date_manual %in% c("none", "report", "test", "all")) {
  warning("The \"--date-manual\" has unexpected value. Will set it to \"none\".")
  params$date_manual <- "none"
}
# check date arguments
date_manual_report <- params$date_manual %in% c("report", "all")
date_manual_test <- params$date_manual %in% c("test", "all")
if (date_manual_report && is.null(params$report_date)) {
  stop("Fatal error! You must specify the report date in manual report date mode.")
}
if (date_manual_test && is.null(params$test_date)) {
  stop("Fatal error! You must specify the test date in manual test date mode.")
}

# environmental settings ----
customer_id <- params$customer_id
# source user script, which will be placed in script path
# TODO: write a package
source(file.path("scripts", "utils.R"), encoding = "UTF-8")
# import font if not found
text_family <- config::get("text.family", config = customer_id)
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}
# get the content of all the configuration files
# report intro: context template
context_tmpl <- get_config(
  "context", params$customer_id, type = params$report_type, ext = "Rmd"
)
# report body: body template
body_tmpl <- get_config(
  "body", params$customer_id, type = params$report_type, ext = "Rmd"
)
# report ending: suggestion template
suggestion_tmpl <- get_config(
  "suggestion", params$customer_id, type = params$report_type, ext = "Rmd"
)
# descriptions, or the content builder
descriptions <- get_config(
  "descriptions", params$customer_id, type = params$report_type
)
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
  SQLite(), dbname = file.path("assets", "db", "iquizoo.sqlite")
)
scores_origin <- tbl(iquizoo_db, "report_ability_scores") %>%
  collect()
dbDisconnect(iquizoo_db)
# filter out corresponding data based on the configurations
customer_type <- config::get("type", config = customer_id)
customer_name <- config::get("name", config = customer_id)
# get the region name to extract data for report
name_region <- scores_origin %>%
  filter(str_detect(!!!syms(customer_type), customer_name)) %>%
  pull(region) %>%
  unique()
if (length(name_region) > 1) {
  warning(
    "Multiple region names found, will try to use the first one.\n",
    "This might cause unexpected results, so please have a careful check!"
  )
  name_region <- name_region[1]
}
# extract data for the whole region
scores_region <- scores_origin %>%
  filter(region == name_region) %>%
  # to avoid temporary variable names, calculate levels here
  mutate(
    level = cut(
      score,
      breaks = config::get("score.level")$breaks,
      labels = config::get("score.level")$labels
    )
  )

# build the three parts of the report ----
if (is.null(params$report_type)) {
  report_type <- config::get("type", config = customer_id)
} else {
  report_type <- params$report_type
}
name_units <- switch(
  report_type,
  region = "全区报告",
  unique(scores_region$school)
)
for (name_unit in name_units) {
  scores_unit <- switch(
    report_type,
    region = scores_region,
    scores_region %>%
      filter(school == name_unit)
  )
  # set report date and test date
  if (date_manual_report) {
    report_date <- params$report_date
  } else {
    report_date <- Sys.time()
  }
  report_date_string <- str_glue("{year(report_date)}年{month(report_date)}月{day(report_date)}日")
  if (date_manual_test) {
    test_date <- params$test_date
  } else {
    test_date <- median(as_datetime(scores_unit$firstPartTime))
  }
  test_date_string <- str_glue("{year(test_date)}年{month(test_date)}月")
  # report rendering
  render_report(output_file = str_glue("{name_region}-{name_unit}.docx"))
}
