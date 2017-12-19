rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(stringr)

# configurations
data_dir <- "里水镇"
out_user_vars <- c("userId", "姓名", "性别", "学校", "出生日期", "年级", "班", "语文", "数学")
out_data_vars <- c("userId", "createDate", "abname", "abscore")
user_files <- list.files(data_dir, "xls$", full.names = TRUE)
exam_file <- list.files(data_dir, "xlsx$", full.names = TRUE)
ability_file <- list.files(data_dir, "csv$", full.names = TRUE)

# load user info
user_info <- user_files %>%
  map(read_excel) %>%
  reduce(rbind) %>%
  filter(
    !is.na(userId),
    !is.na(姓名)
  ) %>%
  mutate(
    userId = parse_integer(userId),
    外部编码 = parse_integer(外部编码)
  )

# load exam score
exam_score <- excel_sheets(exam_file) %>%
  map(
    function(sheet)
      read_excel(exam_file, sheet = sheet, skip = 1, guess_max = 3000)
  ) %>%
  reduce(rbind) %>%
  rename(
    学校 = 学校名称,
    姓名 = 学生姓名,
    原始备注 = 备注,
    学号 = 学籍号
  ) %>%
  mutate(
    年级 = substr(班级, 1, 1),
    班 = str_match(班级, "\\d+"),
    语文 = parse_double(语文),
    数学 = parse_double(数学)
  )

# merge exam score and user information
user_info_full <- user_info %>%
  left_join(exam_score) %>%
  select(one_of(out_user_vars))
write_rds(user_info_full, file.path(data_dir, "user_info.rds"))

# load ability score
ability_score <- read_csv(ability_file) %>%
  rename(
    abname = name,
    abscore = ability_score
  ) %>%
  select(one_of(out_data_vars))
write_rds(ability_score, file.path(data_dir, "user_ability.rds"))
