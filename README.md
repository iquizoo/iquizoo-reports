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

The three major configurations are all used to determine which archetype to use to build the target report. When building, one or more archetypical `R markdown` files named as `{report_part}.{customer_type}[.{report_type}].Rmd` will be read to generate the corresponding report parts. These archetypical `R markdown` files are stored in folder `archetypes`, which are the most important parts of the report. The building process will probably make sense after carefully reading those files.
