# Iquizoo report management

[![Build Status](https://travis-ci.com/iquizoo/reports.svg?branch=master)](https://travis-ci.com/iquizoo/reports)

Here stores files to create iquizoo reports.

## Build logic

There are two types of structures used to build our reports: **configurations** and **archetypes**.

### Configurations

Configurations are set based on two main sources. One is the file *config.yml*, which is used to set static configurations, i.e., those configurations not subject to user's input. The other is *the command line arguments*, which are used to set dynamic configurations, i.e., those configurations modified by user's input. The most significant configurations are listed bellow.

**Customer**. The report receivers are called *customer*s. Customers are then classified as, say, `school` or `region`. There are both static and dynamic configurations to set customer.

**Report type**. The *report type* is a dynamic configuration used to determine the report unit and scores scope (i.e., the necessary records of the scores table). When ignored, the report type will be automatically set as the same as the customer's type (the classification).

**Report parts**. This static configuration is used to set the parts (typically, context, body and suggestion) that constitute the target report.

### Archetypes

As a matter of fact, all of the reports will be composed of three parts: "introduction", "body part of details of each ability" and "suggestion part". These three files, together with `index.Rmd`, should all be placed in the root folder, which ensures the structure of a `bookdown` project and makes the directory tree neat and pretty. In `archetypes` folder, there should be a bunch of child R markdown files whose contents are to be passed to `knitr::knit_expand()`. This is inspired by [user from `StackOverflow`](https://stackoverflow.com/a/14368148/5996475). In the near future, were there time, I would like to refactor it.
