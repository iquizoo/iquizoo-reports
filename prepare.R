# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

#' This script is used to calculate ability scores for Daxing cognitive measure
#' project, and the results are directly used in reports.
#'
#' Accepts one argument from command line as location of data.

# load packages and settings ----
library(tidyverse)
library(forcats)
library(dbplyr)
library(yaml)
library(readxl)
library(rvest)
library(glue)
library(DBI)
library(RSQLite)

#' Main function used to prepare datasets
#'
#' Now the main function is a generic function to do preparation works for
#' datasets from any region. The most important work here is to do score
#' correction for some tasks. Configurations are load from a "config.yml" file
#' from the dataset directory.
#'
#' @param loc Location of data, i.e., the specific data folder
main <- function(loc) {
  # open connection
  iquizoo_db <- dbConnect(SQLite(), dbname = "iquizoo.sqlite")
  # ensure database is disconnected after processing
  on.exit(dbDisconnect(iquizoo_db))
  # environmental settings ----
  data_dir <- file.path("datasets", loc)
  # load configurations
  configs <- read_yaml(file.path(data_dir, "config.yml"), fileEncoding = "UTF-8")
  # future work will be to create table with SQL queries
  key_vars <- list(
    user = c("userId", "name", "sex", "school", "grade", "class"),
    score = c("userId", "exerciseId", "createTime", "score"),
    abscore = c("userId", "abId", "score"),
    class_cover = c("school", "grade", "class", "cover"),
    school_cover = c("school", "cover"),
    school_detail = c("school", "province", "prefecture", "county")
  )

  # load dataset ----
  data_origin <- switch(
    tools::file_ext(configs$data_file),
    xls = ,
    xlsx = read_excel(
      file.path(data_dir, configs$data_file),
      guess_max = 1048576 # maximal number of rows
    ),
    json = jsonlite::read_json(
      file.path(data_dir, configs$data_file), simplifyVector = TRUE
    ) %>%
      as.tbl() %>%
      mutate(createTime = lubridate::as_datetime(createTime))
  ) %>%
    # SQLite does not support datetime data type now, store datetime as text
    mutate(createTime = as.character(createTime)) %>%
    rename(exerciseId = excerciseId, class = cls)

  # correction for user information ----
  if (!is.null(configs$user_correction)) {
    user_correction_varnames <- names(configs$user_correction$replace)
    for (user_correction_varname in user_correction_varnames) {
      data_origin[[user_correction_varname]] <-
        configs$user_correction$replace[[user_correction_varname]]
    }
  }
  # this is a special correction, currently only used for 'erxiao' dataset
  if (!is.null(configs$user_correction$special)) {
    data_origin <- switch(
      configs$user_correction$special,
      jcAccount = data_origin %>%
        mutate(
          class = parse_double(str_sub(jcAccount, 6, 7))
        )
    )
  }
  # recode sex as a factor with Chinese labels
  data_origin <- data_origin %>%
    mutate(
      sex = fct_collapse(
        as_factor(tolower(sex)),
        男 = c("男", "male", "m"),
        女 = c("女", "female", "f")
      )
    )
  # check school name, will REMOVE data with numeric school name
  data_origin <- data_origin %>%
    mutate(school = str_replace_all(school, "\\s", "")) %>%
    filter(!str_detect(school, "\\d"))
  # TODO: check grade (ensure grade is numeric and between 1 and 9)
  # check class name
  if (is.numeric(data_origin$class)) {
    # get the maximal class number to determine the 0's to be added
    digits_class <- floor(log10(max(data_origin$class))) + 1
    data_origin <- data_origin %>%
      mutate(class = sprintf(paste0("%0", digits_class, "d班"), class))
  }
  # extract users information
  users <- data_origin %>%
    select(one_of(key_vars[["user"]])) %>%
    unique()
  # update users table of database
  copy_to(iquizoo_db, users, "users_to_write")
  dbExecute(iquizoo_db, "BEGIN;")
  dbExecute(
    iquizoo_db, "
REPLACE INTO users
 SELECT *
   FROM users_to_write;
    "
  )
  dbExecute(iquizoo_db, "COMMIT;")

  # correction for exercise scores ----
  scores_corrected <- with(
    configs$score_correction,
    switch(
      type,
      none = data_origin %>%
        mutate(score = standardScore),
      iquizoo = {
        info_dir <- "_info"
        # read exercise code information
        task_codes <- read_html(file.path(info_dir, "exercise.html")) %>%
          html_node("table") %>%
          html_table(header = TRUE) %>%
          rename(exerciseId = excerciseid)
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
          mutate(exerciseId = parse_double(交互题CODE)) %>%
          select(-交互题CODE) %>%
          rename(
            title.sp = 描述说明,
            avg.sp = 平均数,
            std.sp = 标准差
          ) %>%
          filter(!is.na(exerciseId))
        # correct data by norms
        data_origin %>%
          left_join(task_codes, by = "exerciseId") %>%
          left_join(norms_common_global, by = "code") %>%
          left_join(norms_special_global, by = "exerciseId") %>%
          mutate(
            score = case_when(
              exerciseId %in% args$special ~
                (asin(sqrt(index)) - avg.sp) / std.sp * 15 + 100,
              # note: use 'magic number' here for simplicity
              exerciseId %in% args$common ~
                (asin(sqrt(index)) - avg.com) / std.com * 15 - 14.4 + 100,
              TRUE ~ standardScore
            )
          )
      },
      scale = data_origin %>%
        group_by(exerciseId, grade) %>%
        mutate(score = scale(index) * 15 + 100) %>%
        ungroup()
    )
  )
  # TABLE: scores of all users on all tasks/exercises
  scores <- scores_corrected %>%
    select(one_of(key_vars[["score"]])) %>%
    filter(!is.na(score))
  # update scores table of database
  copy_to(iquizoo_db, scores, "scores_to_write")
  dbExecute(iquizoo_db, "BEGIN;")
  dbExecute(
    iquizoo_db, "
REPLACE INTO scores
 SELECT *
   FROM scores_to_write;
    "
  )
  dbExecute(iquizoo_db, "COMMIT;")

  # calculate ability scores ----
  scores_with_ability <- scores %>%
    left_join(tbl(iquizoo_db, "exercise_ability"), by = "exerciseId", copy = TRUE) %>%
    left_join(tbl(iquizoo_db, "abilities"), by = "abId", copy = TRUE)
  ability_scores_list <- list()
  repeat {
    components_scores <- scores_with_ability %>%
      filter(!abId == 0) %>%
      group_by(userId, abId) %>%
      summarise(score = mean(score, na.rm = TRUE)) %>%
      ungroup()
    ability_scores_list <- c(ability_scores_list, list(components_scores))
    scores_with_ability <- components_scores %>%
      left_join(tbl(iquizoo_db, "abilities"), by = "abId", copy = TRUE) %>%
      mutate(abId = parent)
    if (all(scores_with_ability$abId == 0)) break
  }
  ability_scores <- bind_rows(ability_scores_list)
  if (!is.null(configs$ability_correction)) {
    ability_scores <- with(
      configs$ability_correction,
      switch(
        type,
        modify = {
          # delta gives those to be added based on root abilities
          deltas <- as_tibble(args)
          abilities <- collect(tbl(iquizoo_db, "abilities"))
          ab_root <- function(ab_id) {
            repeat {
              root <- ab_id
              ab_id <- with(abilities, parent[abId == ab_id])
              if (ab_id == 0) break
            }
            return(root)
          }
          ability_scores %>%
            left_join(abilities, by = "abId") %>%
            mutate(abRoot = map_dbl(abId, ab_root)) %>%
            left_join(deltas, by = c("abRoot" = "abId")) %>%
            mutate(score = score + delta) %>%
            select(one_of(key_vars[["abscore"]]))
        }
      )
    )
  }
  # update ability_scores table of database
  copy_to(iquizoo_db, ability_scores, "ab_scores_to_write")
  dbExecute(iquizoo_db, "BEGIN;")
  dbExecute(
    iquizoo_db, "
REPLACE INTO ability_scores
 SELECT *
   FROM ab_scores_to_write;
    "
  )
  dbExecute(iquizoo_db, "COMMIT;")

  # update additional tables ----
  # update class_covers table of database
  if (!is.null(configs$cover$class)) {
    class_covers <- read_excel(file.path(data_dir, configs$cover$class)) %>%
      select(one_of(key_vars[["class_cover"]]))
    copy_to(iquizoo_db, class_covers, "class_covers_to_write")
    dbExecute(iquizoo_db, "BEGIN;")
    dbExecute(
      iquizoo_db, "
REPLACE INTO class_covers
 SELECT *
   FROM class_covers_to_write;
      "
    )
    dbExecute(iquizoo_db, "COMMIT;")
  }
  # update school_covers table of database
  if (!is.null(configs$cover$school)) {
    school_covers <- read_excel(file.path(data_dir, configs$cover$school)) %>%
      select(one_of(key_vars[["school_cover"]]))
    copy_to(iquizoo_db, school_covers, "school_covers_to_write")
    dbExecute(iquizoo_db, "BEGIN;")
    dbExecute(
      iquizoo_db, "
REPLACE INTO school_covers
 SELECT *
   FROM school_covers_to_write;
      "
    )
    dbExecute(iquizoo_db, "COMMIT;")
    # update school_details table of database
    if (!is.null(configs$detail$school)) {
      school_details <- read_excel(file.path(data_dir, configs$detail$school)) %>%
        select(one_of(key_vars[["school_detail"]]))
      copy_to(iquizoo_db, school_details, "school_details_to_write")
      dbExecute(iquizoo_db, "BEGIN;")
      dbExecute(
        iquizoo_db, "
REPLACE INTO school_details
 SELECT *
   FROM school_details_to_write;
      "
      )
      dbExecute(iquizoo_db, "COMMIT;")
    }
  }
}

if (interactive()) {
  main(readline(prompt = "input the region of your dataset: "))
} else {
  main(commandArgs(trailingOnly = TRUE)[1])
}
