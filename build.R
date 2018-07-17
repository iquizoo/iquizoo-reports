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
suppressPackageStartupMessages(library(RSQLite))
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
    )
  )
  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  params <- parse_args(OptionParser(option_list = option_list), convert_hyphens_to_underscores = TRUE)
} else {
  # read configurations from yaml config file if in interactive mode (this is used for quick preview or debug)
  params <- config::get(file = "params.yml")
}

# set package options ----
old_opts <- options(
  "yaml.eval.expr" = TRUE,
  "knitr.kable.NA" = "",
  "reports.archytype" = "archetypes"
)

# check command line arguments ----
if (is.null(params$customer_id)) {
  stop("Fatal error! You must specify the identifier of the customer.")
}
if (!params$date_manual %in% c("none", "report", "test", "all")) {
  warning("The \"--date-manual\" has unexpected value. Will set it to \"none\".")
  params$date_manual <- "none"
}

# dataset preparations ----
# connect to database and download data
iquizoo_db <- dbConnect(
  SQLite(), dbname = file.path("assets", "db", "iquizoo.sqlite")
)
scores_origin <- tbl(iquizoo_db, "report_ability_scores") %>%
  collect()
dbDisconnect(iquizoo_db)
# filter out corresponding data based on the configurations
customer_type <- config::get("type", config = params$customer_id)
customer_name <- config::get("name", config = params$customer_id)
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
    firstPartTime = as_datetime(firstPartTime),
    level = cut(
      score,
      breaks = config::get("score.level")$breaks,
      labels = config::get("score.level")$labels
    )
  )

# build the three parts of the report ----
if (is.null(params$report_type)) {
  report_type <- config::get("type", config = params$customer_id)
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
  # get metadata from `index.Rmd` and render `index.Rmd` to base
  index_file <- file.path(
    getOption("reports.archytype"),
    get_tmpl_name("index", params$customer_id, params$report_type)
  )
  if (!file.exists(index_file)) {
    # if the customer specific index template does not exist, use default
    index_file <- file.path(
      getOption("reports.archytype"), "index.Rmd"
    )
  }
  report_metadata <- rmarkdown::yaml_front_matter(
    index_file, encoding = "UTF-8"
  )
  file.copy(index_file, "index.Rmd")
  # render parts of reports
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
      heading <- report_metadata$bodyheading
      ab_ids <- report_metadata$ability$id
      report_part_md <- render_part_body(heading, report_part_tmpl, ab_ids)
    }
    write_lines(report_part_md, paste0(report_part, ".Rmd"))
  }
  # report rendering
  bookdown::render_book(
    "index.Rmd",
    output_file = str_glue("{name_region}-{name_unit}.docx"),
    clean_envir = FALSE
  )
  # remove generated files
  unlink("index.Rmd")
  for (report_part in report_parts) unlink(paste0(report_part, ".Rmd"))
}

# restore options ----
options(old_opts)
