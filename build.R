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
      c("-T", "--debug-test"), action = "store_true", default = FALSE,
      help = "Optional. Used when in testing mode. When set, the program will choose one report unit randomly."
    )
  )
  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  params <- parse_args(OptionParser(option_list = option_list), convert_hyphens_to_underscores = TRUE)
} else {
  # read configurations from yaml config file if in interactive mode (this is used for quick preview or debug)
  params <- config::get(file = "params.yml")
}

# environmental settings ----
# package options
old_opts <- options(
  "yaml.eval.expr" = TRUE,
  "knitr.kable.NA" = "",
  "reports.archytype" = "archetypes",
  "reports.mysql.querydir" = "assets/sql"
)
# import font if not found
text_family <- config::get("text.family", config = params$customer_id)
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}

# check command line arguments and extract customer info ----
if (is.null(params$customer_id)) {
  stop("Fatal error! You must specify the identifier of the customer.")
}
customer_type <- config::get("customer.type", config = params$customer_id)
customer_name <- config::get("customer.name", config = params$customer_id)
project_ids <- config::get("project.id", config = params$customer_id) %>%
  paste(collapse = ",")

# dataset preparations ----
# connect to database and download data
db_config <- config::get("database", config = params$customer_id)
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
    nesting(!!!syms(c(names(users), "examId", "createDate", "bci_score")))
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
if (is.null(params$report_type)) {
  report_type <- config::get("customer.type", config = params$customer_id)
} else {
  report_type <- params$report_type
}
name_units <- switch(
  report_type,
  region = "全区报告",
  unique(scores_region$school)
)
if (params$debug_test) {
  name_units <- sample(name_units, 1)
  warning(str_glue("Debugging. The chosen report unit is {name_units}."), immediate. = TRUE)
}
for (name_unit in name_units) {
  # get the scores of current report unit
  scores_unit <- switch(
    report_type,
    region = scores_region,
    scores_region %>%
      filter(school == name_unit)
  )
  # default index file name is also used as default template file name
  default_index <- "index.Rmd"
  # the customer specific index template
  index_tmpl <- file.path(
    getOption("reports.archytype"),
    get_tmpl_name("index", params$customer_id, params$report_type)
  )
  if (!file.exists(index_tmpl)) {
    # if the customer specific index template does not exist, use default
    index_tmpl <- file.path(
      getOption("reports.archytype"), default_index
    )
  }
  # copy index template to base and rename it as the same as the default name
  file.copy(index_tmpl, default_index)
  # render main parts of reports
  report_parts <- config::get("report.parts", config = params$customer_id)
  for (report_part in report_parts) {
    report_part_tmpl_file <- file.path(
      getOption("reports.archytype"),
      get_tmpl_name(
        report_part, params$customer_id, params$report_type
      )
    )
    report_part_tmpl <- read_file(report_part_tmpl_file)
    if (report_part != "body") {
      report_part_md <- render_part_normal(report_part_tmpl)
    } else {
      heading <- config::get("bodyheading", config = params$customer_id)
      ab_ids <- config::get("ability", config = params$customer_id)$id
      report_part_md <- render_part_body(heading, report_part_tmpl, ab_ids)
    }
    write_lines(report_part_md, paste0(report_part, ".Rmd"))
  }
  rmd_files <- c(default_index, paste0(report_parts, ".Rmd"))
  # report rendering
  bookdown::render_book(
    default_index,
    output_file = str_glue("{name_region}-{name_unit}.docx"),
    clean_envir = FALSE
  )
  # remove generated report parts files
  unlink(rmd_files)
}

# restore options ----
options(old_opts)
