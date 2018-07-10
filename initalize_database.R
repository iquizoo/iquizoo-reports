# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# load packages and settings ----
library(tidyverse)
library(dbplyr)
library(readxl)
library(DBI)
library(RSQLite)

# database preparations ----
# open connection
iquizoo_db <- dbConnect(SQLite(), dbname = "iquizoo.sqlite")
# initializing database tables
creation_query_folder <- "sql"
creation_query_files <- list.files(creation_query_folder, "\\.sql")
for (creation_query_file in creation_query_files) {
  table_name <- tools::file_path_sans_ext(creation_query_file)
  dbSendQuery(iquizoo_db, "BEGIN;")
  if (db_has_table(iquizoo_db, table_name)) {
    dbSendQuery(iquizoo_db, glue::glue("DROP TABLE {table_name};"))
  }
  dbSendQuery(
    iquizoo_db, read_file(
      file.path(creation_query_folder, creation_query_file)
    )
  )
  dbSendQuery(iquizoo_db, "COMMIT;")
}
# prepare exercises, abilities
info_dir <- file.path("_info")
# update abilities
abilities <- read_excel(file.path(info_dir, "abilities.xlsx"))
db_insert_into(iquizoo_db, "abilities", abilities, overwrite = TRUE)
# update exercises and exercises map
exercises_joined <- read_excel(file.path(info_dir, "exerciseInfo.xlsx")) %>%
  mutate(excerciseId = parse_double(ID)) %>%
  gather(type, abName, ability_blai, ability_math) %>%
  filter(abName != "null") %>%
  left_join(abilities, by = c("abName" = "name")) %>%
  filter(!is.na(abId)) %>%
  select(excerciseId, abId, name, title) %>%
  rename(exerciseId = excerciseId) %>%
  unique()
exercises <- exercises_joined %>%
  select(exerciseId, name, title) %>%
  unique()
exercise_ability <- exercises_joined %>%
  select(exerciseId, abId) %>%
  unique()
db_insert_into(iquizoo_db, "exercises", exercises, overwrite = TRUE)
db_insert_into(iquizoo_db, "exercise_ability", exercise_ability, overwrite = TRUE)
dbDisconnect(iquizoo_db)
