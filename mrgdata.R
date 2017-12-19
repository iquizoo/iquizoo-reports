rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(stringr)

# configurations
data_dir <- "里水镇"
out_user_vars <- c("userId", "姓名", "性别", "学校", "出生日期", "年级", "班")
out_exam_vars <- c("userId", "学科", "成绩")
out_ability_vars <- c("userId", "createDate", "abname", "abscore")
user_files <- list.files(data_dir, "xls$", full.names = TRUE)
exam_file <- list.files(data_dir, "xlsx$", full.names = TRUE)
ability_file <- list.files(data_dir, "csv$", full.names = TRUE)

# load user info
user_info_raw <- user_files %>%
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
# mutate grade and class information to factor type
user_info <- user_info_raw %>%
  select(one_of(out_user_vars)) %>%
  mutate(
    年级 = factor(
      年级, c("一", "二", "三", "四"), c("一年级", "二年级", "三年级", "四年级")
    ),
    班 = factor(班, 1:length(unique(班)))
  )
write_rds(user_info, file.path(data_dir, "user_info_test.rds"))

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
    数学 = parse_double(数学),
    英语 = parse_double(英语)
  ) %>%
  left_join(user_info_raw) %>%
  gather(学科, 成绩, 语文, 数学, 英语, na.rm = TRUE) %>%
  mutate(学科 = factor(学科, levels = c("语文", "数学", "英语"))) %>%
  select(out_exam_vars)
write_rds(exam_score, file.path(data_dir, "user_exam_test.rds"))

# load ability score
ability_score <- read_csv(ability_file) %>%
  rename(
    abname = name,
    abscore = ability_score
  ) %>%
  group_by(userId, createDate, abname) %>%
  mutate(occur = row_number(desc(abscore))) %>%
  filter(
    occur == 1,
    userId > 0
  ) %>%
  select(one_of(out_ability_vars))
write_rds(ability_score, file.path(data_dir, "user_ability_test.rds"))
