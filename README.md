# Iquizoo report management

[![Build Status](https://travis-ci.org/iquizoo/reports.svg?branch=master)](https://travis-ci.org/iquizoo/reports)

Here stores files to create iquizoo reports.

## Build logic

There are two types of structures used to build our reports: **configurations** and **archetypes**.

### Configurations

Configurations are set based on two main sources. One is the file [config.yml](config.yml), which is used to set static configurations, i.e., those configurations not subject to user's input. The other is *the command line arguments*, which are used to set dynamic configurations, i.e., those configurations modified by user's input. The most significant configurations are listed bellow.

Major settings in file `config.yml`:

* **Customer**. The report receivers are called *customer*s (mostly, they are just pseudo and just play a role of identification). The corresponding organizations of customers are stored.
* **Ability**. This is a structure stored the information (including "name", "definition" and styles information: "heading level" and "custom style name") of abilities  to be reported.

Major settings as command line arguments:

* **Report unit**. The *report unit* is a dynamic configuration used to determine the report unit from which each single report is generated. When ignored, the report unit will be automatically set as `default` and report the results of all the users for the specified customer/project identifier.
* **Report type**.

### Archetypes

Inspired by [user from `StackOverflow`](https://stackoverflow.com/a/14368148/5996475), given that all of the reports are composed of three parts: "introduction" ([01-context.Rmd](01-context.Rmd)), "body part of details of each ability" ([02-body.Rmd](02-body.Rmd)) and "suggestion part" ([03-suggestion.Rmd](03-suggestion.Rmd)), we put these three files, together with `index.Rmd`, in the root folder, which ensure the structure of a `bookdown` project and makes the directory tree neat and pretty. In `archetypes` folder, there are a bunch of child R markdown files whose contents are to be passed to `knitr::knit_expand()` to generate our report contents on the fly.
