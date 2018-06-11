# basic logic:
# The most important modification here is to correct the scores of the
# reasoning tests. The test ids are:
# * 116039
# * 116039
# * 118036
# * 118036

# load packages
library(tidyverse)
library(readxl)
library(writexl)
library(rvest)
wk_dir <- here::here("data", "daxing")
mod_tests_sp <- c(116039, 116041, 116042, 118036)
mod_tests_com <- 97938

# merge data and clean data ----
# load dataset
data_origin <- read_excel(
  file.path(wk_dir, "daxing.xlsx"), guess_max = 1048576 # maximal number of rows
)
# read exercise code information
task_codes <- read_html(file.path(wk_dir, "exercise.html")) %>%
  html_node("table") %>%
  html_table(header = TRUE) %>%
  rename(excerciseId = excerciseid)
# common norms
data_norms_common <- read_excel(
  file.path(wk_dir, "norm_convert_release.xlsx"), skip = 1
) %>%
  mutate(code = parse_integer(交互题CODE)) %>%
  rename(
    title.com = 描述说明,
    avg.com = 平均数,
    std.com = 标准差
  )
# special norms
data_norms_special <- read_excel(
  file.path(wk_dir, "norm_convert_release_special.xlsx"), skip = 1
) %>%
  mutate(excerciseId = parse_double(交互题CODE)) %>%
  rename(
    title.sp = 描述说明,
    avg.sp = 平均数,
    std.sp = 标准差
  )
# merge data and norms
data_merged <- data_origin %>%
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
# data cleanse: remove outliers according boxplot rule
data_clean <- data_merged %>%
  group_by(excerciseId) %>%
  mutate(
    stdScore = ifelse(stdScore %in% boxplot.stats(stdScore)$out, NA, stdScore)
  )

# calculate ability scores ----
# map abilities
subability <- read_excel(file.path(wk_dir, "subability.xlsx"))
ability_map <- read_excel(file.path(wk_dir, "exerciseInfo.xlsx")) %>%
  left_join(subability, by = c("ability_cali" = "subname")) %>%
  mutate(excerciseId = parse_double(ID)) %>%
  rename(taskname = name)
# add ability setting to data
data_clean <- data_clean %>%
  left_join(ability_map, by = "excerciseId") %>%
  select(
    -abId, -allTime, -starts_with("basic"), -config, -dataJson, -device,
    -examId, -subId, -starts_with("交互题CODE"), -starts_with("title"),
    -ID, -birthDay
  )
write_xlsx(data_clean, file.path(wk_dir, "data_clean.xlsx"))
# calculate by averaging
ability_scores <- data_clean %>%
  group_by(userId, name, sex, school, grade, cls, abname) %>%
  summarise(score = mean(stdScore, na.rm = TRUE))
write_xlsx(ability_scores, file.path(wk_dir, "ability_scores.xlsx"))
