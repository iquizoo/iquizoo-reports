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
  # environmental settings ----
  data_dir <- file.path("datasets", loc)
  info_dir <- file.path("_info")
  # load configurations
  configs <- read_yaml(file.path(data_dir, "config.yml"), fileEncoding = "UTF-8")
  # set the directory where the result data go
  res_dir <- configs$goal$res_path
  if (!dir.exists(res_dir)) dir.create(res_dir)
  # score correction method
  correct_method <- configs$score_correction$method
  # norm_correct <- configs$score_correction$norm
  # if (norm_correct) {
  #   mod_tests_sp <- configs$score_correction$special
  #   mod_tests_com <- configs$score_correction$common
  # }
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
    user = c(primary_key = "userId", "name", "sex", "school", "grade", "cls", "firstPartTime"),
    exercise = c(primary_key = "excerciseId", "taskName", "taskIDName"),
    score = c(
      primary_key = "scoreId",
      foreign_key_user = "userId",
      foreign_key_exercise = "excerciseId",
      "createTime", "stdScore"
    ),
    abscore = c(
      primary_key = "abscoreId",
      foreign_key_user = "userId",
      foreign_key_ability = "abId",
      "score", "level"
    )
  )
  breaks <- qnorm(c(0, 0.3, 0.7, 0.9, 1)) * 15 + 100
  labels <- LETTERS[4:1]

  # load ability map tables ----
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
    filter(!is.na(abId)) %>%
    select(-abNameOld, -type) %>%
    add_column(mapId = 1:nrow(.), .before = 1)

  # clean data / correct data ----
  # load dataset
  data_origin <- read_excel(
    file.path(data_dir, configs$data_file), guess_max = 1048576 # maximal number of rows
  )
  # data correction
  scores_corrected <- switch(
    correct_method,
    none = data_origin %>%
      mutate(stdScore = standardScore),
    iquizoo = {
      # read exercise code information
      task_codes <- read_html(file.path(info_dir, "exercise.html")) %>%
        html_node("table") %>%
        html_table(header = TRUE) %>%
        rename(excerciseId = excerciseid)
      # common norms
      norms_common_global <- read_excel(
        file.path(info_dir, "norm_common_global.xlsx"), skip = 1
      ) %>%
        mutate(code = parse_integer(交互题CODE)) %>%
        select(-交互题CODE) %>%
        rename(
          title.com = 描述说明,
          avg.com = 平均数,
          std.com = 标准差
        ) %>%
        filter(!is.na(code))
      # construct header
      agewise_header <- read_excel(
        file.path(info_dir, "norm_agewise.xlsx"), skip = 2,
        col_names = FALSE, n_max = 2
      )
      # fill missing header
      miss_header <- is.na(agewise_header[1, ])
      agewise_header[1, miss_header] <- agewise_header[1, which(miss_header) - 1]
      agewise_header_chr <- paste(agewise_header[1, ], agewise_header[2, ], sep = "_") %>%
        str_replace("_NA", "")
      norms_common_agewise <- read_excel(
        file.path(info_dir, "norm_agewise.xlsx"), skip = 4,
        col_names = agewise_header_chr
      )
      # special norms
      norms_special_global <- read_excel(
        file.path(info_dir, "norm_special_global.xlsx"), skip = 1
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
      data_origin %>%
        left_join(task_codes, by = "excerciseId") %>%
        left_join(norms_common_global, by = "code") %>%
        left_join(norms_special_global, by = "excerciseId") %>%
        mutate(
          stdScore = case_when(
            excerciseId %in% configs$score_correction$args$special ~
              (asin(sqrt(index)) - avg.sp) / std.sp * 15 + 100,
            # note: use 'magic number' here for simplicity
            excerciseId %in% configs$score_correction$args$common ~
              (asin(sqrt(index)) - avg.com) / std.com * 15 - 14.4 + 100,
            TRUE ~ standardScore
          )
        )
    },
    scale = data_origin %>%
      group_by(excerciseId, grade) %>%
      mutate(stdScore = scale(index) * 15 + 100)
  )
  # correction for user information
  if (!is.null(configs$user_correction)) {
    user_correction_varnames <- names(configs$user_correction$replace)
    for (user_correction_varname in user_correction_varnames) {
      scores_corrected[[user_correction_varname]] <-
        configs$user_correction$replace[[user_correction_varname]]
    }
  }
  # this is a special correction, currently only used for 'erxiao' dataset
  if (!is.null(configs$user_correction$special)) {
    scores_corrected <- switch(
      configs$user_correction$special,
      jcAccount = scores_corrected %>%
        mutate(
          cls = parse_double(str_sub(jcAccount, 6, 7))
        )
    )
  }
  # data cleanse: remove duplicates and outliers based on boxplot rule
  scores_clean <- scores_corrected %>%
    # remove duplicates
    group_by(userId, excerciseId) %>%
    mutate(occurrence = row_number(desc(stdScore))) %>%
    filter(occurrence == 1) %>%
    select(-occurrence) %>%
    # remain the earliest createTime only for each user
    group_by(userId) %>%
    mutate(firstPartTime = createTime[1]) %>%
    # remove outliers based on boxplot rule
    group_by(excerciseId) %>%
    mutate(
      stdScore = ifelse(stdScore %in% boxplot.stats(stdScore)$out, NA, stdScore)
    ) %>%
    ungroup() %>%
    unique()

  # separate original data into three relational tables ----
  # TABLE: exercises'/tasks' information
  exercises <- scores_clean %>%
    select(one_of(key_vars[["exercise"]])) %>%
    unique()
  # TABLE: users' information
  users <- scores_clean %>%
    select(one_of(key_vars[["user"]])) %>%
    unique() %>%
    mutate(
      sex = factor(sex, levels = c("male", "female"), labels = c("男", "女")),
      cls = glue("{cls}班")
    )
  # TABLE: scores of all users on all tasks/exercises
  scores <- scores_clean %>%
    add_column(scoreId = 1:nrow(.), .before = 1) %>%
    select(one_of(key_vars[["score"]]))

  # calculate ability scores and levels ----
  components_scores <- scores %>%
    left_join(exercises) %>%
    left_join(ability_map) %>%
    left_join(abilities) %>%
    group_by(userId, abId, abParent) %>%
    summarise(
      score = mean(stdScore, na.rm = TRUE),
      level = cut(score, breaks, labels)
    ) %>%
    ungroup()
  total_scores <- components_scores %>%
    group_by(userId, abParent) %>%
    summarise(
      score = mean(score, na.rm = TRUE),
      level = cut(score, breaks, labels)
    ) %>%
    ungroup() %>%
    add_column(abId = .$abParent, .before = "abParent")
  # TABLE: scores on each ability for all users
  ability_scores <- rbind(components_scores, total_scores) %>%
    add_column(abscoreId = 1:nrow(.), .before = 1)

  # read extra datasets ----
  if (!is.null(configs$data_extra)) {
    # TABLE: extra database
    assign(
      configs$data_extra$name,
      read_excel(file.path(data_dir, configs$data_extra$file))
    )
  }

  # side effects: save all useful information as Excel files ----
  # future plan will be to store them to the local/remote MySQL database
  output_tables <- configs$goal$tables
  outputs <- list()
  for (output_table in output_tables) {
    outputs[[output_table]] <- eval(as.symbol(output_table))
  }
  write_xlsx(outputs, file.path(res_dir, glue("{loc}.xlsx")))
}

if (interactive()) {
  main(readline(prompt = "input the region of your dataset: "))
} else {
  main(commandArgs(trailingOnly = TRUE)[1])
}
