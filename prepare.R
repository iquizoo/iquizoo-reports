# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

#' This script is used to calculate ability scores for Daxing cognitive measure
#' project, and the results are directly used in reports.
#'
#' Accepts one argument from command line as location of data.

# load packages and settings ----
library(tidyverse)
library(yaml)
library(readxl)
library(writexl)
library(rvest)
library(glue)

#' Main function used to prepare datasets
#'
#' Now the main function is a generic function to do preparation works for
#' datasets from any region. The most important work here is to do score
#' correction for some tasks. Configurations are load from a "config.yml"
#' file from the dataset directory.
#'
#' @param loc Location of data, i.e., the specific data folder
#' @return Returns 0 if succeeded.
main <- function(loc) {
  data_dir <- file.path("datasets", loc)
  info_dir <- file.path("_info")
  # load configurations
  configs <- read_yaml(file.path(data_dir, "config.yml"), fileEncoding = "UTF-8")
  # set the directory where the result data go
  res_dir <- configs$goal$res_path
  # use norm to correct data or not, if not, will use standardized score only
  norm_correct <- configs$score_correction$norm
  if (norm_correct) {
    mod_tests_sp <- configs$score_correction$special
    mod_tests_com <- configs$score_correction$common
  }
  # TABLE: ability name informations
  abilities <- tribble(
    ~abId, ~abParent, ~abName,        ~abNameEn,
    1,     0,        "基础学习能力", "blai",
    2,     0,        "基础数学能力", "math",
    11,    1,        "注意力",       "attention",
    12,    1,        "记忆力",       "memory",
    13,    1,        "反应力",       "reaction",
    14,    1,        "自控力",       "control",
    15,    1,        "思维力",       "thinking",
    16,    2,        "数字加工",     "digit",
    17,    2,        "数学推理",     "reasoning",
    18,    2,        "空间几何",     "geometry",
    19,    2,        "数量加工",     "quantity",
    20,    2,        "数学计算",     "computation"
  )
  key_vars <- list(
    user = c(primary_key = "userId", "name", "sex", "school", "grade", "cls"),
    exercise = c(primary_key = "excerciseId", "taskName", "taskIDName"),
    score = c(
      primary_key = "scoreId",
      foreign_key_user = "userId",
      foreign_key_exercise = "excerciseId",
      "createTime", "stdScore"
    )
  )
  breaks <- qnorm(c(0, 0.3, 0.7, 0.9, 1)) * 15 + 100
  labels <- LETTERS[4:1]

  # clean data / correct data ----
  # load dataset
  data_origin <- read_excel(
    file.path(data_dir, configs$data_file), guess_max = 1048576 # maximal number of rows
  )
  # separate original data into three relational tables
  # TABLE: users' information
  users <- data_origin %>%
    select(one_of(key_vars[["user"]])) %>%
    unique()
  # TABLE: exercises'/tasks' information
  exercises <- data_origin %>%
    select(one_of(key_vars[["exercise"]])) %>%
    unique()

  # load ability map tables
  abilities_info <- read_excel(file.path(info_dir, "abilities.xlsx"))
  exercises_info <- read_excel(file.path(info_dir, "exerciseInfo.xlsx"))
  # TABLE: ability map between exercises and abilities
  ability_map <- exercises_info %>%
    left_join(abilities_info, by = c("ability_blai" = "subname")) %>%
    mutate(excerciseId = parse_double(ID)) %>%
    select(-ability_blai) %>% # no need to use subabilities now
    mutate(
      math = ifelse(ability_math == "null", NA_character_, ability_math),
      blai = ifelse(abname == "null", NA_character_, abname)
    ) %>%
    select(excerciseId, blai, math) %>%
    gather(type, abNameOld, blai, math) %>%
    filter(!is.na(abNameOld)) %>%
    mutate(abId = with(abilities, abId[match(abNameOld, abName)])) %>%
    select(-abNameOld, -type) %>%
    add_column(mapId = 1:nrow(.), .before = 1)
  # data correction
  if (norm_correct) {
    # read exercise code information
    task_codes <- read_html(file.path(info_dir, "exercise.html")) %>%
      html_node("table") %>%
      html_table(header = TRUE) %>%
      rename(excerciseId = excerciseid)
    # common norms
    data_norms_common <- read_excel(
      file.path(info_dir, "norm_convert_release.xlsx"), skip = 1
    ) %>%
      mutate(code = parse_integer(交互题CODE)) %>%
      select(-交互题CODE) %>%
      rename(
        title.com = 描述说明,
        avg.com = 平均数,
        std.com = 标准差
      ) %>%
      filter(!is.na(code))
    # special norms
    data_norms_special <- read_excel(
      file.path(info_dir, "norm_convert_release_special.xlsx"), skip = 1
    ) %>%
      mutate(excerciseId = parse_double(交互题CODE)) %>%
      select(-交互题CODE) %>%
      rename(
        title.sp = 描述说明,
        avg.sp = 平均数,
        std.sp = 标准差
      ) %>%
      filter(!is.na(excerciseId))
    # correct data by norms
    scores_corrected <- data_origin %>%
      left_join(task_codes, by = "excerciseId") %>%
      left_join(data_norms_common, by = "code") %>%
      left_join(data_norms_special, by = "excerciseId") %>%
      mutate(
        stdScore = case_when(
          excerciseId %in% mod_tests_sp ~
            (asin(sqrt(index)) - avg.sp) / std.sp * 15 + 100,
          # note: use 'magic number' here for simplicity
          excerciseId == mod_tests_com ~
            (asin(sqrt(index)) - avg.com) / std.com * 15 - 14.4 + 100,
          TRUE ~ standardScore
        )
      )
  } else {
    scores_corrected <- data_origin %>%
      group_by(excerciseId, grade) %>%
      mutate(stdScore = scale(index) * 15 + 100)
  }
  # read extra datasets to merge
  if (!is.null(configs$data_extra_file)) {
    data_extra <- read_excel(file.path(data_dir, configs$data_extra_file))
    scores_corrected <- scores_corrected %>%
      left_join(data_extra)
    # TODO correct to use normal form of database
    key_vars$user <- union(key_vars$user, "schoolCovert")
  }
  # data cleanse: remove duplicates and outliers based on boxplot rule
  # TABLE: scores of all users on all tasks/exercises
  scores <- scores_corrected %>%
    # remove duplicates
    group_by(userId, excerciseId) %>%
    mutate(occurrence = row_number(desc(stdScore))) %>%
    filter(occurrence == 1) %>%
    select(-occurrence) %>%
    # remain the earliest createTime only for each user
    group_by(userId) %>%
    mutate(createTime = createTime[1]) %>%
    # remove outliers based on boxplot rule
    group_by(excerciseId) %>%
    mutate(
      stdScore = ifelse(stdScore %in% boxplot.stats(stdScore)$out, NA, stdScore)
    ) %>%
    ungroup() %>%
    unique() %>%
    add_column(scoreId = 1:nrow(.), .before = 1) %>%
    select(one_of(key_vars[["score"]]))

  # calculate ability scores and levels ----
  # preallocate as a list
  ability_scores_list <- list()
  for (ability_type in names(ability_components)) {
    # calculate components scores by averaging
    components_scores <- scores %>%
      rename(ability = !! sym(glue("ability_{ability_type}"))) %>%
      group_by(!!! syms(key_vars)) %>%
      summarise(
        score = mean(stdScore, na.rm = TRUE),
        level = cut(score, breaks, labels)
      ) %>%
      ungroup() %>%
      filter(ability %in% ability_components[[ability_type]])
    # calculate total score through component scores
    total_scores <- components_scores %>%
      mutate(ability = ability_type_cn[ability_type]) %>%
      group_by(!!! syms(key_vars)) %>%
      summarise(
        score = mean(score, na.rm = TRUE),
        level = cut(score, breaks, labels)
      ) %>%
      ungroup()
    ability_scores_list[[ability_type]] <- rbind(components_scores, total_scores)
  }
  # combine into one tabular data
  ability_scores <- ability_scores_list %>%
    reduce(rbind) %>%
    mutate(cls = glue("{cls}班"))

  # merge relavant data sets ----

  # side effects: output all ability scores after clensing
  write_xlsx(ability_scores, file.path(res_dir, glue("{loc}.xlsx")))
}

if (interactive()) {
  main(readline(prompt = "input the region of your dataset: "))
} else {
  main(commandArgs(trailingOnly = TRUE)[1])
}
