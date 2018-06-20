# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script is used to generate chapters for book building.

# environmental settings ----
# file encoding: set to 'UTF-8'
kFileEncoding <- "UTF-8"
# configurations
kConfigPath <- file.path("config")
kConfigParamBase <- "params.yml"
kConfigParamTravis <- "params.travis.yml"
kConfigDescription <- "descriptions.yml"
# scripts
kScriptPath <- file.path("scripts")
kScriptUtils <- "utils.R"
kScriptChapter <- ""
# database
kDbPath <- file.path("assets", "db")
# templates
kTemplatePath <- file.path("assets", "template")
# ability names settings
kTestType <- setNames(
  c("基础学习能力", "基础数学能力"),
  c("blai", "math")
)
kSubTestType <- list(
  blai = setNames(
    c("注意力", "记忆力", "反应力", "自控力", "思维力"),
    c("attn", "mmry", "rctn", "ctrl", "thnk")
  ),
  math = setNames(
    c("数字加工", "数学推理", "空间几何", "数量加工", "数学计算"),
    c("dgtl", "rsng", "gmtr", "qnty", "cmpt")
  )
)
# set the oreder of ability report
ability_name_order <- character()
for (testType in names(kTestType)) {
  ability_name_order <- c(
    ability_name_order,
    kTestType[testType], kSubTestType[[testType]]
  )
}

# load packages and user scripts ----
library(tidyverse)
library(extrafont)
library(yaml)
library(glue)
library(lubridate)
# load user utilities
source(file.path(kScriptPath, kScriptUtils), encoding = kFileEncoding)

# setting for Chinese font ----
# use Android Sans font, more info at
# https://www.freechinesefont.com/simplified-traditional-droid-sans-fallback/
text_family <- "Droid Sans Fallback"
# import font if not found
if (!text_family %in% fonts()) {
  font_import(prompt = FALSE, pattern = "DroidSansFallback")
}

# knitr options ----
# do not display NA
options(knitr.kable.NA = "")

# loading configurations ----
# parameterized dynamic reporting configurations
params <- read_yaml(
  file.path(kConfigPath, kConfigParamBase),
  fileEncoding = kFileEncoding
)
if (file.exists(file.path(kConfigPath, kConfigParamTravis))) {
  params_travis <- read_yaml(
    file.path(kConfigPath, kConfigParamTravis),
    fileEncoding = kFileEncoding
  )
  # replace base params with travis params settings
  for (param_travis in names(params_travis)) {
    params[[param_travis]] <- params_travis[[param_travis]]
  }
}
# descriptions used in content building
descriptions <- read_yaml(
  file.path(kConfigPath, kConfigDescription),
  fileEncoding = kFileEncoding
)

# datasets preparations ----
# load ability scores
scores_district <- read_csv(file.path(kDbPath, params$data_filename))
# reconfigure `school_name` based on the dataset
if (params$school_name_auto) {
  school_names <- unique(scores_district$school)
} else {
  school_names <- params$school_name
}
# validate shcool names
if (!all(school_names %in% scores_district$school)) {
  stop("School not found!")
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
    hlevl = if_else(name %in% kTestType, 2, 3),
    # set style as 'numbered' for ability, and 'normal' for subability
    style = if_else(name %in% kTestType, "标题2-编号", ""),
    md = render_title_content(
      title = name, content = description,
      hlevel = hlevl, style = style
    )
  )
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
  # format test date
  test_date_string <- glue("{year(test_date)}年{month(test_date)}月")
  # set test region
  if (params$region_auto) {
    # do not set this as TRUE, because no region info is set now
    region <- unique(scores_school$region)
  } else {
    region <- params$region
  }

  # render body content as 'body.Rmd' ----
  body_filename <- "body.Rmd"
  body_title <- "详细报告"
  body_content_vector <- character()
  for (ability_name in names(ability_names)) {
    # use one template of single ability to generate the 'body.Rmd'
    body_content_vector[ability_name] <- read_file(file.path(kTemplatePath, "body.glue.Rmd")) %>%
      glue(.open = "<<", .close = ">>")
  }
  body_content <- paste(body_content_vector, collapse = "\n\n")
  write_lines(render_title_content(body_title, body_content), body_filename)

  # render report for current school ----
  bookdown::render_book("index.Rmd", output_file = glue("{school_name}.docx"), clean_envir = FALSE)
  # clean generated body content
  unlink(body_filename)
}
