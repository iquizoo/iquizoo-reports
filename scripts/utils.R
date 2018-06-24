# Copyright (C) 2018 Liang Zhang - All Rights Reserved

# @author Liang Zhang <psychelzh@outlook.com>

# This script stores a bundle of utility functions used in reporting.

# package loading checking
require(tidyverse)
require(glue)

#' Helper funtion used to get config file content
#'
#' @param name The name of the configuration
#' @param regionid The identifier of region
#' @param type The report type
#' @param ext File extension
get_config <- function(name, regionid, type, ext) {
  config_dir <- getOption("report.include.path")["config"]
  if (is.null(config_dir)) {
    warning(
      "No config path specified! Please set it in option \"report.include.path\"!\n",
      "Will use default path: \"config\""
    )
    config_dir <- "config"
  }
  # note the file name rule
  config_file <- file.path(
    config_dir, paste(name, regionid, type, ext, sep = ".")
  )
  if (!file.exists(config_file)) {
    stop(sprintf("Critical error! Config file \"%s\" not found!", config_file))
  }
  # read content
  config_content <- switch(
    ext,
    yml = ,
    yaml = read_yaml(config_file),
    Rmd = read_file(config_file)
  )
  return(config_content)
}

#' Helper function to generate required \code{.Rmd} files
#'
#' @param ... These parameters are to be passed to \code{\link{bookdown::render_book}}
render_report <- function(...) {
  # render context content as 'context.Rmd' ----
  context_filename <- "context.Rmd"
  # extraction context content from descriptions and substitute r codes in it with value
  report_context <- as_tibble(descriptions$context) %>%
    mutate(
      md = render_title_content(
        # all of these content will be at level 2 heading
        title, content, hlevel = 2, glue = TRUE, .open = "<<", .close = ">>"
      )
    ) %>%
    pull(md) %>%
    paste(collapse = "\n\n")
  context_content <- context_tmpl %>%
    # note that 'report_context' will be pasted into it
    glue(.open = "<<", .close = ">>")
  write_lines(context_content, context_filename)

  # render body content as 'body.Rmd' ----
  body_filename <- "body.Rmd"
  body_title <- "详细报告"
  body_content_vector <- character()
  for (ability_name_id in ability_names_id) {
    # use one template of single ability to generate the 'body.Rmd'
    body_content_vector[ability_name_id] <- body_tmpl %>%
      glue(.open = "<<", .close = ">>")
  }
  body_content <- paste(body_content_vector, collapse = "\n\n")
  write_lines(render_title_content(body_title, body_content), body_filename)

  # render report for current school ----
  bookdown::render_book("index.Rmd", ...)
  # clean up generated building content
  unlink(context_filename)
  unlink(body_filename)
}

#' Render heading according to level.
#'
#' @param text The text to be adjusted
#' @param hlevel The heading level, default to 1
#' @return The corresponding heading level markdown content
render_heading <- function(text, hlevel = 1) {
  prefix <- strrep("#", hlevel)
  md <- paste(prefix, text)
  return(md)
}

#' Custom style block generating.
#'
#' @param text The text to be adjusted
#' @param style The name of the custom style
#' @return The pandoc-flavored markdown string
customize_style <- function(text, style = "") {
  prefix <- if_else(
    style == "", "",
    unclass(glue("::: {{custom-style=\"{style}\"}}"))
  )
  suffix <- if_else(style == "", "", ":::")
  md <- paste(prefix, text, suffix, sep = "\n")
  return(md)
}

#' Render title content pair as a section.
#'
#' @param title The section title
#' @param content The section content
#' @param hlevel The heading level, default to 1
#' @param style The name of the custom style
#' @param glue Logical value to indicate if the texts should be substituted
#' @param ... Additional parameters to be passed to \code{\link{glue::glue}}
#' @return The markdown string to render as a section
render_title_content <- function(title, content, hlevel = 1, style = "", glue = FALSE, ...) {
  heading <- render_heading(title, hlevel) %>%
    customize_style(style = style)
  md <- paste(heading, content, sep = "\n\n")
  if (glue) {
    md <- md %>%
      map_chr(function(x) {glue(x, ...)})
  }
  return(md)
}

#' Summarise function used for summary table generation
#'
#' @param raw_tab Raw data table as a data.frame
#' @param group_vars Variable names used for grouping, default: 'c("region", "cls")'
#' @return The summarised table
sum_tab <- function(raw_tab, group_vars = c("region", "cls")) {
  out_tab <- raw_tab %>%
    group_by(!!! syms(group_vars)) %>%
    summarise(
      实测人数 = n(),
      平均分 = mean(score, na.rm = TRUE),
      标准差 = sd(score, na.rm = TRUE),
      最高分 = max(score, na.rm = TRUE),
      中位数 = median(score, na.rm = TRUE),
      最低分 = min(score, na.rm = TRUE),
      A = sum(level == "A", na.rm = TRUE),
      B = sum(level == "B", na.rm = TRUE),
      C = sum(level == "C", na.rm = TRUE),
      D = sum(level == "D", na.rm = TRUE)
    ) %>%
    ungroup()
  return(out_tab)
}

#' custom theme setter for histogram/density plot
#'
#' @param base_size base font size
#' @param base_family base font family
set_theme_dist <- function(base_size = 18, base_family = "") {
  theme_minimal(base_size, base_family) %+replace%
    theme(
      axis.title.x = element_text(margin = margin(t = 1, unit = "lines")),
      axis.line.x = element_line(),
      axis.ticks.x = element_line(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 1, 1, 1, unit = "lines")
    )
}

#' custom theme setter for three virtual panels graphs
#'
#' @param base_size base font size
#' @param base_family base font family
set_theme_panels <- function(base_size = 18, base_family = "") {
  theme_minimal(base_size, base_family) %+replace%
    theme(
      # x axis is at the bottom
      axis.title.x = element_text(margin = margin(t = 1, unit = "lines")),
      # disable facet panel labels
      strip.text = element_blank(),
      strip.background = element_blank(),
      # ensure facet panels are concatenated
      panel.spacing = unit(-0.5, "lines"),
      # remain major x grid only
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(linetype = "dotdash", color = "grey")
    )
}
