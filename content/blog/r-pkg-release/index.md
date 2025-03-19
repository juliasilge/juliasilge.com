---
title: Release an R package with Positron
author: Julia Silge
date: '2025-03-19'
format: hugo
slug: r-pkg-releas
categories:
  - rstats
tags:
  - rstats
summary: >-
  See how you can use Positron, a new, next-generation data science IDE, for R
  package development tasks and releasing a new version of an R package.
---


Recently I've been spending my time working on [Positron](https://positron.posit.co/) a new, next-generation data science IDE. Positron is currently in public beta and [may not yet be a good fit for everyone](https://positron.posit.co/start.html#is-positron-for-me), but I personally have been using it in my R package development work and think it's a great fit for that kind of work. In this screencast, I walk through a few typical R package development tasks for [butcher](https://butcher.tidymodels.org/) (one of the R packages I maintain) and release a new version of the package to CRAN.

{{% youtube "uL3NZQIMrpk" %}}

The new version of butcher is [now on CRAN](https://cran.r-project.org/package=butcher)! If you are interested in using this kind of checklist for your own R packages, check out [the documentation for `usethis::use_release_issue()`](https://usethis.r-lib.org/reference/use_release_issue.html), including options for customization.

We know Positron still has some rough edges, but if you are interested in experimenting with it, I invite you to [download the most recent beta release](https://positron.posit.co/download) and [share your feedback with us](https://github.com/posit-dev/positron/discussions).
