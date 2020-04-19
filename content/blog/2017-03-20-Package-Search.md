---
title: "How Do You Discover R Packages?"
slug: "Package-Search"
date: 2017-03-20
excerpt: "Take our survey to share your experiences"
tags: [rstats]
---



Like I mentioned in my [last blog post](http://juliasilge.com/blog/Scraping-CRAN/), I am contributing to a session at [userR 2017](https://user2017.brussels/) this coming July that will focus on discovering and learning about R packages. This is an increasingly important issue for R users as we all decide which of the 10,000+ packages to invest time in understanding and then use in our work.


```{r}
library(dplyr)

available.packages() %>% 
    tbl_df()
```



```
## # A tibble: 10,276 × 17
##        Package Version Priority                                             Depends
##          <chr>   <chr>    <chr>                                               <chr>
## 1           A3   1.0.0     <NA>                      R (>= 2.15.0), xtable, pbapply
## 2       abbyyR   0.5.0     <NA>                                        R (>= 3.2.0)
## 3          abc     2.1     <NA> R (>= 2.10), abc.data, nnet, quantreg, MASS, locfit
## 4  ABCanalysis   1.2.1     <NA>                                         R (>= 2.10)
## 5     abc.data     1.0     <NA>                                         R (>= 2.10)
## 6     abcdeFBA     0.4     <NA>              Rglpk,rgl,corrplot,lattice,R (>= 2.10)
## 7     ABCoptim  0.14.0     <NA>                                                <NA>
## 8        ABCp2     1.2     <NA>                                                MASS
## 9      ABC.RAP   0.9.0     <NA>                                        R (>= 3.1.0)
## 10       abcrf     1.5     <NA>                                           R(>= 3.1)
##                                   Imports LinkingTo
##                                     <chr>     <chr>
## 1                                    <NA>      <NA>
## 2        httr, XML, curl, readr, progress      <NA>
## 3                                    <NA>      <NA>
## 4                                 plotrix      <NA>
## 5                                    <NA>      <NA>
## 6                                    <NA>      <NA>
## 7                                    Rcpp      Rcpp
## 8                                    <NA>      <NA>
## 9                  graphics, stats, utils      <NA>
## 10 readr, MASS, ranger, parallel, stringr      <NA>
## # ... with 10,266 more rows, and 11 more variables: Suggests <chr>, Enhances <chr>, License <chr>,
## #   License_is_FOSS <chr>, License_restricts_use <chr>, OS_type <chr>, Archs <chr>, MD5sum <chr>,
## #   NeedsCompilation <chr>, File <chr>, Repository <chr>
```

![center](/figs/2017-03-20-Package-Search/cran_doge.jpeg)

To prepare for this session and gain some understanding, I am running [an online survey](http://doo.vote/a87ff60) about how R users currently discover and learn about R packages. I know that online polls like this can't give us the same kind of understanding as surveys with carefully designed samples, but it still will give us some insight into how users are currently going about the process of deciding which packages to use. This is important information both for package developers, the maintainers of CRAN Task Views, and R users in general.

There is one question on the survey that allows the respondent to select all the answers that apply:

### [How do you currently discover and learn about R packages?](http://doo.vote/a87ff60)

- Email lists such as r-help, r-packages, or r-pkg-devel
- General search websites such as Google and Yahoo
- R-specific search websites such as [METACRAN](https://www.r-pkg.org) or [Rdocumentation](https://www.rdocumentation.org/)
- R packages built for search such as the [sos](https://cran.r-project.org/package=sos) package
- CRAN Task Views
- Your personal network, such as colleagues and professors
- Conferences, meet-ups, or seminars
- Books, textbooks, or journal articles (JSS, JOSS, R-Journal)
- Social media such as blogs, R-bloggers, Twitter, Slack, or GitHub contacts
- Other

If you are an R user, please [go to the poll and vote](http://doo.vote/a87ff60). If you have other ways that you don't feel were fairly covered in these options, feel free to leave a comment here on my blog and we can consider them in our discussion at useR.
