rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(stringr)

# configurations
data_dir <- getSrcDirectory(function(x) x)
user_files <- list.files(data_dir, "xls$", full.names = TRUE)
exam_file <- list.files(data_dir, "xlsx$", full.names = TRUE)
ability_file <- list.files(data_dir, "csv$", full.names = TRUE)

# load user info
user_info <- user_files %>%
  map(read_excel) %>%
  reduce(rbind) %>%
  filter(!is.na(userId)) %>%
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

# load ability score
ability_score <- read_csv(ability_file) %>%
  rename(
    姓名 = userName,
    性别 = userGender,
    外部编码 = outCode,
    学号 = userNo
  )

# join results
user_data <- user_info %>%
  left_join(exam_score) %>%
  left_join(ability_score) %>%
  select(-abId) %>%
  filter(!is.na(name)) %>%
  unique() %>%
  spread(name, ability_score)
write_rds(user_data, file.path(data_dir, "user_data.rds"))
