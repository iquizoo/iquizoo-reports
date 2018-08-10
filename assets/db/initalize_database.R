# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# load packages and settings ----
library(tidyverse)
library(dbplyr)
library(readxl)
library(DBI)
library(RSQLite)

# database preparations ----
oldwd <- setwd(file.path("assets", "db"))
# configurations
db_filename <- "iquizoo.sqlite"
# remove database file beforehand
unlink(db_filename, force = TRUE)
# open connection
iquizoo_db <- dbConnect(SQLite(), dbname = db_filename)
# initializing database tables
sql_files <- list.files("sql", "\\.sql", full.names = TRUE)
walk(sql_files, ~dbExecute(iquizoo_db, read_file(.x)))
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
setwd(oldwd)
