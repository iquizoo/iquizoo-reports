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

# options used in reports configurations ----
options(
  # knitr: do not display NA
  knitr.kable.NA = "",
  # general
  report.encoding = "UTF-8",
  # use Android Sans font, more info at
  # https://www.freechinesefont.com/simplified-traditional-droid-sans-fallback/
  report.text.family = "Droid Sans Fallback",
  # include paths
  report.include.path = c(
    script = "scripts",
    config = "config",
    database = file.path("assets", "db"),
    template = file.path("assets", "template")
  )
)

# get the configuration parameters used in report generation ----
if (!interactive()) {
  # parse command line argument if not in interactive mode
  # specify our desired options in a list
  # by default OptionParser will add an help option equivalent to
  # make_option(c("-h", "--help"), action="store_true", default=FALSE,
  #               help="Show this help message and exit")
  option_list <- list(
    make_option(
      c("-t", "--type"), default = "one",
      help = "Specify the report type, could be 'one' (default), 'school' or 'district'."
    ),
    make_option(
      c("-a", "--all"), action = "store_true", default = FALSE,
      help = paste(
        "For now, only works when 'type' is 'school',",
        "if set, will build reports for all schools."
      )
    ),
    make_option(
      c("-r", "--regionid"),
      help = paste(
        "Specify the region identifier for reporting.",
        "This signifies because it will be used to identify dataset and descriptions."
      )
    )
  )
  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  params <- parse_args(OptionParser(option_list = option_list))
} else {
  # read configurations from yaml config file if in interactive mode
  params <- read_yaml(
    file.path(getOption("report.include.path")["config"], "params.yml"),
    fileEncoding = getOption("report.encoding")
  )
}

# environmental settings ----
# import font if not found
text_family <- getOption("report.text.family")
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}
# source user script, which will be placed in script path, which could be enhanced
source(
  file.path(getOption("report.include.path")["script"], "utils.R"),
  encoding = getOption("report.encoding")
)
# descriptions used in content building
descr_src <- file.path(
  getOption("report.include.path")["config"],
  # description file name rule: 'descriptions.{regionid}.{type}.yml'
  paste("descriptions", params$regionid, params$type, ext = "yml", sep = ".")
)
if (!file.exists(descr_src)) stop("Critical error! No description file found!")
descriptions <- read_yaml(descr_src, fileEncoding = getOption("report.encoding"))
# set report date: needs enhancement
if (params$report_date_auto) {
  report_date <- Sys.time()
} else {
  report_date <- params$report_date
}
report_date_string <- glue("{year(report_date)}年{month(report_date)}月{day(report_date)}日")

# datasets preparations ----
# load ability scores
scores_origin <- read_excel(
  file.path(
    getOption("report.include.path")["database"],
    paste0(params$regionid, ".xlsx")
  )
)
# count number of school, grade and users
n_school <- n_distinct(scores_origin$school)
n_grade <- n_distinct(scores_origin$grade)
n_user <- n_distinct(scores_origin$userId)
# reconfigure `school_name` based on the dataset
if (params$type == "school") {
  if (params$school_name_auto || params$all) {
    school_names <- unique(scores_origin$school)
  } else {
    school_names <- params$school_name
  }
  # validate shcool names
  if (!all(school_names %in% scores_origin$school)) {
    stop("School not found!")
  }
}
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

# build report according to the build type ----
body_filename <- "body.Rmd"
body_title <- "详细报告"
body_src <- file.path(
  getOption("report.include.path")["config"],
  # body Rmd file name rule: 'body.{regionid}.{type}.Rmd'
  paste("body", params$regionid, params$type, ext = "Rmd", sep = ".")
)
switch(
  params$type,
  school = {
    for (school_name in school_names) {
      # data preparations ----
      # filter out scores for current school
      scores_school <- scores_origin %>%
        filter(school == school_name)
      # combine data from whole district, this school and each class
      scores_combined <- list(
        本区 = scores_origin,
        本校 = scores_school,
        各班 = scores_school
      ) %>%
        bind_rows(.id = "region") %>%
        mutate(cls = if_else(region != "各班", region, cls)) %>%
        mutate(region = factor(region, c("各班", "本校", "本区")))
      # set test date
      if (params$test_date_auto) {
        test_date <- median(scores_school$createTime)
      } else {
        test_date <- params$test_date
      }
      test_date_string <- glue("{year(test_date)}年{month(test_date)}月")
      # set test region
      if (params$region_auto) {
        # do not set this as TRUE, because no region info is set now
        region <- unique(scores_school$region)
      } else {
        region <- params$region
      }
      # render body content as 'body.Rmd' ----
      body_content_vector <- character()
      for (ability_name in names(ability_names)) {
        # use one template of single ability to generate the 'body.Rmd'
        body_content_vector[ability_name] <- read_file(
          file.path(getOption("report.tmpl.path"), "body.glue.Rmd")
        ) %>%
          glue(.open = "<<", .close = ">>")
      }
      body_content <- paste(body_content_vector, collapse = "\n\n")
      write_lines(render_title_content(body_title, body_content), body_filename)

      # render report for current school ----
      bookdown::render_book("index.Rmd", output_file = glue("{school_name}.docx"), clean_envir = FALSE)
      # clean generated body content
      unlink(body_filename)
    }
  },
  district = {
    # data preparations ----
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
    # set test date
    if (params$test_date_auto) {
      test_date <- median(scores_origin$createTime)
    } else {
      test_date <- params$test_date
    }
    test_date_string <- glue("{year(test_date)}年{month(test_date)}月")
    # set test region
    if (params$region_auto) {
      # do not set this as TRUE, because no region info is set now
      region <- unique(scores_origin$region)
    } else {
      region <- params$region
    }
    # render body content as 'body.Rmd' ----
    body_content_vector <- character()
    for (ability_name in names) {
      # use one template of single ability to generate the 'body.Rmd'
      body_content_vector[ability_name] <- read_file(
        file.path(getOption("report.tmpl.path"), "body.glue.district.Rmd")
      ) %>%
        glue(.open = "<<", .close = ">>")
    }
    body_content <- paste(body_content_vector, collapse = "\n\n")
    write_lines(render_title_content(body_title, body_content), body_filename)

    # render report for current school ----
    bookdown::render_book("index.Rmd", output_file = glue("{region}.docx"), clean_envir = FALSE)
    # clean generated body content
    unlink(body_filename)
  },
  one = {
    # combine data from whole district, each school
    # scores_combined <- list(
    #   本区 = scores_origin,
    #   各校 = scores_origin
    # ) %>%
    #   bind_rows(.id = "region") %>%
    #   mutate(school = if_else(region != "各校", region, school)) %>%
    #   mutate(region = factor(region, c("本区", "各校")))
    # set test date
    if (params$test_date_auto) {
      test_date <- median(scores_origin$createTime)
    } else {
      test_date <- params$test_date
    }
    test_date_string <- glue("{year(test_date)}年{month(test_date)}月")
    # set test region
    if (params$region_auto) {
      # do not set this as TRUE, because no region info is set now
      region <- unique(scores_origin$region)
    } else {
      region <- params$region
    }
  },
  stop("Unsupported report type! Please specify as school/district only.")
)

# render body content as 'body.Rmd' ----
body_content_vector <- character()
for (ability_name_id in ability_names_id) {
  # use one template of single ability to generate the 'body.Rmd'
  body_content_vector[ability_name_id] <- read_file(body_src) %>%
    glue(.open = "<<", .close = ">>")
}
body_content <- paste(body_content_vector, collapse = "\n\n")
write_lines(render_title_content(body_title, body_content), body_filename)

# render report for current school ----
bookdown::render_book("index.Rmd", output_file = glue("{region}.docx"), clean_envir = FALSE)
# clean generated body content
unlink(body_filename)
