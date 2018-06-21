# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# load packages ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(optparse))

# parse command line argument if not in interactive mode
if (!interactive()) {
  # specify our desired options in a list
  # by default OptionParser will add an help option equivalent to
  # make_option(c("-h", "--help"), action="store_true", default=FALSE,
  #               help="Show this help message and exit")
  option_list <- list(
    make_option(
      c("-T", "--type"), default = "school",
      help = "Specify the report type, could be 'school' (default) or 'district'."
    ),
    make_option(
      c("-a", "--all"), action = "store_true", default = FALSE,
      help = "For now, only works when 'type' is 'school', if set, will build reports for all schools."
    )
  )

  # get command line options, if help option encountered print help and exit,
  # otherwise if options not found on command line then set defaults,
  opts <- parse_args(OptionParser(option_list = option_list))
} else {
  opts <- list(type = "school", all = FALSE)
}

# environmental settings ----
# options used in reports configurations
options(
  # knitr: do not display NA
  knitr.kable.NA = "",
  # general
  report.encoding = "UTF-8",
  # use Android Sans font, more info at
  # https://www.freechinesefont.com/simplified-traditional-droid-sans-fallback/
  report.text.family = "Droid Sans Fallback",
  # configurations
  report.config.path = "config",
  report.config.param.base = "params.yml",
  report.config.descr.school = "descriptions.yml",
  report.config.descr.district = "descriptions.district.yml",
  # scripts
  report.script.path = "scripts",
  report.script.files = "utils.R",
  # database
  report.db.path = file.path("assets", "db"),
  # templates
  report.tmpl.path = file.path("assets", "template"),
  # ability names settings
  report.test.type = setNames(
    c("基础学习能力", "基础数学能力"),
    c("blai", "math")
  ),
  report.test.subtype = list(
    blai = setNames(
      c("注意力", "记忆力", "反应力", "自控力", "思维力"),
      c("attn", "mmry", "rctn", "ctrl", "thnk")
    ),
    math = setNames(
      c("数字加工", "数学推理", "空间几何", "数量加工", "数学计算"),
      c("dgtl", "rsng", "gmtr", "qnty", "cmpt")
    )
  )
)
# import font if not found
text_family <- getOption("report.text.family")
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}
# set the oreder of ability report
ability_name_order <- character()
for (testType in names(getOption("report.test.type"))) {
  ability_name_order <- c(
    ability_name_order,
    getOption("report.test.type")[testType],
    getOption("report.test.subtype")[[testType]]
  )
}
# get encoding option
file_encoding <- getOption("report.encoding")
# get script/config file names from options/settings
script_utils_src <- file.path(
  getOption("report.script.path"),
  getOption("report.script.files")
)
params_src <- file.path(
  getOption("report.config.path"),
  getOption("report.config.param.base")
)
descr_src <- switch(
  opts$type,
  school = file.path(
    getOption("report.config.path"), getOption("report.config.descr.school")
  ),
  district = file.path(
    getOption("report.config.path"), getOption("report.config.descr.district")
  ),
  file.path(
    getOption("report.config.path"), getOption("report.config.descr.district")
  )
)
# source user script
source(script_utils_src, encoding = file_encoding)
# parameterized dynamic reporting configurations
params <- read_yaml(params_src, fileEncoding = file_encoding)
# descriptions used in content building
descriptions <- read_yaml(descr_src, fileEncoding = file_encoding)
# set report date
if (params$report_date_auto) {
  report_date <- Sys.time()
} else {
  report_date <- params$report_date
}
report_date_string <- glue("{year(report_date)}年{month(report_date)}月{day(report_date)}日")

# datasets preparations ----
# load ability scores
scores_district <- read_csv(file.path(getOption("report.db.path"), params$data_filename))
# reconfigure `school_name` based on the dataset
if (opts$type == "school") {
  if (params$school_name_auto || opts$all) {
    school_names <- unique(scores_district$school)
  } else {
    school_names <- params$school_name
  }
  # validate shcool names
  if (!all(school_names %in% scores_district$school)) {
    stop("School not found!")
  }
}
# reconfigure `ability_name` based on the dataset
if (params$ability_name_auto) {
  ability_names <- ability_name_order[ability_name_order %in% ability_name_order]
} else {
  ability_names <- ability_name_order[ability_name_order %in% params$ability_name]
}
# ability information preparation
ability_info <- as_tibble(descriptions$ability) %>%
  mutate(
    # set heading as level 2 for ability, and level 3 for subability
    hlevl = if_else(name %in% getOption("report.test.type"), 2, 3),
    # set style as 'numbered' for ability, and 'normal' for subability
    style = if_else(name %in% getOption("report.test.type"), "标题2-编号", ""),
    md = render_title_content(
      title = name, content = description,
      hlevel = hlevl, style = style
    )
  )

# build report according to the build type ----
body_filename <- "body.Rmd"
body_title <- "详细报告"
switch(
  opts$type,
  school = {
    for (school_name in school_names) {
      # data preparations ----
      # filter out scores for current school
      scores_school <- scores_district %>%
        filter(school == school_name)
      # combine data from whole district, this school and each class
      scores_combined <- list(
        本区 = scores_district,
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
    scores_district <- scores_district %>%
      rename(schoolOvert = school, school = schoolCovert)
    # combine data from whole district, each school
    scores_combined <- list(
      本区 = scores_district,
      各校 = scores_district
    ) %>%
      bind_rows(.id = "region") %>%
      mutate(school = if_else(region != "各校", region, school)) %>%
      mutate(region = factor(region, c("本区", "各校")))
    # set test date
    if (params$test_date_auto) {
      test_date <- median(scores_district$createTime)
    } else {
      test_date <- params$test_date
    }
    test_date_string <- glue("{year(test_date)}年{month(test_date)}月")
    # set test region
    if (params$region_auto) {
      # do not set this as TRUE, because no region info is set now
      region <- unique(scores_district$region)
    } else {
      region <- params$region
    }
    # render body content as 'body.Rmd' ----
    body_content_vector <- character()
    for (ability_name in names(ability_names)) {
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
  stop("Unsupported report type! Please specify as school/district only.")
)
