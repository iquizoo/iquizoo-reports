# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

#' This script is used to calculate ability scores for Daxing cognitive measure
#' project, and the results are directly used in reports.
#'
#' Accepts one argument from command line as location of data.

# load packages and settings ----
library(tidyverse)
library(dbplyr)
library(yaml)
library(readxl)
library(writexl)
library(rvest)
library(glue)
library(DBI)
library(RSQLite)

# open database connection by SQLite ----
iquizoo_db <- dbConnect(SQLite(), dbname = "iquizoo.db")

# metadata preparation ----
info_dir <- file.path("_info")
if (!db_has_table(iquizoo_db, "abilities")) {
  abilities <- read_excel(file.path(info_dir, "abilities.xlsx"))
  dbWriteTable(iquizoo_db, name = "abilities", value = abilities)
}
# add exercises information if not found
if (!db_has_table(iquizoo_db, "exercises")) {
  exercises <- read_excel(file.path(info_dir, "exerciseInfo.xlsx")) %>%
    mutate(excerciseId = parse_double(ID)) %>%
    gather(type, abName, ability_blai, ability_math) %>%
    filter(abName != "null") %>%
    left_join(abilities, copy = TRUE) %>%
    filter(!is.na(abId)) %>%
    unique() %>%
    select(excerciseId, name, title, abId)
  dbWriteTable(iquizoo_db, name = "exercises", value = exercises)
}

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
  # ensure database is disconnected after processing
  on.exit(dbDisconnect(iquizoo_db))
  # environmental settings ----
  data_dir <- file.path("datasets", loc)
  # load configurations
  configs <- read_yaml(file.path(data_dir, "config.yml"), fileEncoding = "UTF-8")
  # future work will be to create table with SQL queries
  key_vars <- list(
    user = c("userId", "name", "sex", "school", "grade", "cls", "firstPartTime"),
    score = c("userId", "excerciseId", "createTime", "stdScore"),
    abscore = c("abscoreId", "userId", "abId", "score", "level")
  )
  breaks <- qnorm(c(0, 0.3, 0.7, 0.9, 1)) * 15 + 100
  labels <- LETTERS[4:1]

  # clean data / correct data ----
  # load dataset
  data_origin <- switch(
    tools::file_ext(configs$data_file),
    xls = ,
    xlsx = read_excel(
      file.path(data_dir, configs$data_file), guess_max = 1048576 # maximal number of rows
    ),
    json = jsonlite::read_json(
      file.path(data_dir, configs$data_file), simplifyVector = TRUE
    ) %>%
      as.tbl() %>%
      mutate(createTime = lubridate::as_datetime(createTime))
  )
  # data correction
  scores_corrected <- with(
    configs$score_correction$pre,
    switch(
      type,
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
              excerciseId %in% args$special ~
                (asin(sqrt(index)) - avg.sp) / std.sp * 15 + 100,
              # note: use 'magic number' here for simplicity
              excerciseId %in% args$common ~
                (asin(sqrt(index)) - avg.com) / std.com * 15 - 14.4 + 100,
              TRUE ~ standardScore
            )
          )
      },
      scale = data_origin %>%
        group_by(excerciseId, grade) %>%
        mutate(stdScore = scale(index) * 15 + 100)
    )
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

  # separate original data into relational tables ----
  # TABLE: user information
  users <- scores_clean %>%
    select(one_of(key_vars[["user"]])) %>%
    unique() %>%
    mutate(firstPartTime = as.character(firstPartTime))
  if (is.numeric(scores_clean$cls)) {
    # get the maximal class number to determine the 0's to be added
    digits_cls <- floor(log10(max(scores_clean$cls))) + 1
    users <- users %>%
      mutate(cls = sprintf(paste0("%0", digits_cls, "d班"), cls))
  }
  if (!all(is.na(scores_clean$sex))) {
    users <- users %>%
      mutate(sex = recode("female" = "女", "male" = "男"))
  } else {
    users <- users %>%
      mutate(sex = NA_character_)
  }

  # TABLE: scores of all users on all tasks/exercises
  scores <- scores_clean %>%
    select(one_of(key_vars[["score"]])) %>%
    unique() %>%
    mutate(createTime = as.character(createTime))

  # calculate ability scores and levels ----
  components_scores <- scores %>%
    left_join(tbl(iquizoo_db, "exercises"), copy = TRUE) %>%
    left_join(tbl(iquizoo_db, "abilities"), copy = TRUE) %>%
    group_by(userId, abId, abParent) %>%
    summarise(score = mean(stdScore, na.rm = TRUE)) %>%
    ungroup()
  total_scores <- components_scores %>%
    group_by(userId, abParent) %>%
    summarise(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    add_column(abId = .$abParent, .before = "abParent")
  # TABLE: scores on each ability for all users
  ability_scores_candidate <- rbind(components_scores, total_scores)

  # correct ability scores directly ----
  ability_scores <- with(
    configs$score_correction$post,
    switch(
      type,
      add = {
        ability_scores_candidate %>%
          mutate(
            score = case_when(
              abId == 2 ~ score + 5,
              TRUE ~ score + 15
            )
          )
      },
      none = ability_scores_candidate
    )
  ) %>%
    mutate(level = cut(score, breaks, labels)) %>%
    add_column(abscoreId = 1:nrow(.), .before = 1) %>%
    select(one_of(key_vars[["abscore"]]))

  # read extra datasets ----
  if (!is.null(configs$data_extra)) {
    # TABLE: extra database
    assign(
      configs$data_extra$name,
      read_excel(file.path(data_dir, configs$data_extra$file))
    )
  }

  # side effects: save all useful information as Excel files ----
  # store ability scores in the local SQLite database
  dbWriteTable(iquizoo_db, name = loc, value = ability_scores, overwrite = TRUE)
  # update users in the local SQLite database
  if (db_has_table(iquizoo_db, "users")) {
    users_to_append <- users %>%
      anti_join(tbl(iquizoo_db, "users"), copy = TRUE)
    dbWriteTable(iquizoo_db, name = "users", value = users_to_append, append = TRUE)
  } else {
    dbWriteTable(iquizoo_db, name = "users", value = users)
  }
  # update scores in the local SQLite database
  if (db_has_table(iquizoo_db, "scores")) {
    scores_to_append <- scores %>%
      anti_join(tbl(iquizoo_db, "scores"), copy = TRUE)
    dbWriteTable(iquizoo_db, name = "scores", value = scores_to_append, append = TRUE)
  } else {
    dbWriteTable(iquizoo_db, name = "scores", value = scores)
  }
}

if (interactive()) {
  main(readline(prompt = "input the region of your dataset: "))
} else {
  main(commandArgs(trailingOnly = TRUE)[1])
}
