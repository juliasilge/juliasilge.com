---
title: "A Beginner's Guide to Travis-CI for R"
slug: "Beginners-Guide-to-Travis"
date: 2016-05-20
excerpt: "The Blind Leading the Blind"
tags: [rstats]
---

Have you seen all those attractive green badges on other people's R packages and thought, "*I* want a lovely green badge!"

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Always a nice feeling when Travis manages to actually build. <a href="https://twitter.com/hashtag/runconf16?src=hash">#runconf16</a> <a href="https://t.co/7qZfH2OEij">pic.twitter.com/7qZfH2OEij</a></p>&mdash; Julia Silge (@juliasilge) <a href="https://twitter.com/juliasilge/status/716034756509507584">April 1, 2016</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

OF COURSE YOU DO. Well, let's give it a shot, because today I am going to attempt a beginner's guide to using Travis-CI for continuous integration for R packages. It is going to be a *beginner's* guide because that is all I could possibly write; my knowledge and experience with Travis is limited. Sometimes it can be helpful to have someone walk you through something new that she herself has only recently come to grips with, though, so here we go!

## What Is This? And Why? And Who Is Travis?!

If you have written an R package, you probably have gotten into the habit of running `R CMD check` on a (hopefully) regular basis to check for common problems. It automatically detects lots of problems that can arise that you might not otherwise notice. Also you probably have put your package on GitHub, for version control and also to be able to share your code with other people and track issues and whatnot. The idea behind continuous integration is that CI will automatically run `R CMD check` (along with your tests, etc.) every time you push a commit to GitHub. You don't have to remember to do this; CI automatically checks the code after every commit. [Travis-CI](https://travis-ci.org/) is a continuous integration service that builds and tests projects that are hosted on GitHub; if your R package is open source (i.e., not private), then using Travis-CI is free.

What are some reasons that you would want to do this (if you aren't already convinced by the very idea of automated checking)? If you plan to submit your package to CRAN, using Travis-CI is a way to check your package on a platform (Ubuntu) that you might not have access to otherwise, much like using `build_win()`. You can [look here](https://github.com/juliasilge/tidytext/blob/3a679c815210dee26e56d50dbb730e7058ad144d/cran-comments.md) to see how I explained the different test environments I checked for the tidytext package. If you *don't* plan to submit your package to CRAN, for whatever reason, then having your package passing on Travis signals to possible users that this package is probably not a hot mess and is more likely to be worth investing time in downloading and learning how to use.

## Getting Started with Travis

To get started with Travis-CI, you mosey on over to [their website](https://travis-ci.org/) and make an account that is connected to your GitHub account. After you do that, you will see all your GitHub repositories with switches that you can switch from off to on. Pick a repository (in this case, a repository for an R package) that you want to build on Travis and switch it on.

Now, we need to tell Travis what to do by adding a `.travis.yml` file to the R package. Many packages need only a minimal `.travis.yml` that looks like this:

```
 language: R
 cache: packages
```

This isn't R code or anything; it is telling Travis how to go about building the package. (And FYI, this file needs to be added to `.Rbuildignore` because it is not part of the R package from the point of view of R.) You can use `devtools::use_travis()` to automatically add a minimal `.travis.yml` to your package, add `.travis.yml` to `.Rbuildignore`, and add the code for the badge to your README. This kind of simple `.travis.yml` is what I have for my [janeaustenr package](https://github.com/juliasilge/janeaustenr/blob/2008066a02558ec36bddd9f3deb7d81469684a48/.travis.yml). Once you have made this file and then push it to GitHub, the push triggers Travis to build and check the package. Every commit you push to GitHub after this will trigger a new build on Travis that will go through all the automated checking and testing.

If you are lucky, your package will build on Travis successfully and you can add your beautiful green badge to your README on GitHub, if you haven't already added it via `devtools::use_travis()` or copy/pasting from elsewhere. If you are less than lucky, well...

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Things are going great; why do you ask?! <a href="https://twitter.com/hashtag/rstats?src=hash">#rstats</a> <a href="https://t.co/x3jJQZkuGs">pic.twitter.com/x3jJQZkuGs</a></p>&mdash; Julia Silge (@juliasilge) <a href="https://twitter.com/juliasilge/status/722514130968535042">April 19, 2016</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

If a build on Travis is erroring or failing (which are different), I tend to think of the problems being of two possible types: 

* either the code/package itself has a problem(s) or 
* I have not told Travis how to build the package correctly. 

One example might be that your package depends on a package from GitHub that is not on CRAN. Travis knows to look in your `DESCRIPTION` file and will install all your package's dependencies from CRAN to build your package. What if one of your package's dependencies is not on CRAN? Then you need to list them as `Remotes:` in your `DESCRIPTION` file. You can check out the [`Remotes:` vignette from devtools](https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html) to see more details on this, and here are [two example](https://github.com/ropenscilabs/geojsonlint/blob/7f8c02925c7bca1fb0d5aa7ea750db1b5dd01fc8/DESCRIPTION) [`DESCRIPTION` files](https://github.com/jennybc/googlesheets/blob/df2fc3c227848878627bb44477162b2337b9ba71/DESCRIPTION) that use `Remotes:`. Both of those packages have `.travis.yml` files that are more complicated than the simplest version, but that is not because of a GitHub package dependency.

Speaking of which, another possibility is that you will need to tell Travis about some system requirements to get your package to build. For the tidytext package, we have some dependencies on packages that have system requirements. For one, we have a dependency on [quanteda](https://cran.r-project.org/web/packages/quanteda/index.html), which has a dependency on the [XML package](https://cran.r-project.org/web/packages/XML/index.html). If you check out that [link for the XML package on CRAN](https://cran.r-project.org/web/packages/XML/index.html), you'll notice that it says:

```
 SystemRequirements: libxml2 (>= 2.6.3)
```

This means that wherever you want to build/use the XML package, you need `libxml2`. Thus, to build our package tidytext (which depends on quanteda which depends on XML) we need to get this library installed. The `.travis.yml` for tidytext (which has two such system requirements) looks like this:

```
 language: R
 cache: packages
 sudo: false

 addons:
   apt:
     packages:
       - libgsl0-dev # for topicmodels
       - libxml2-dev # for XML, a dependency of quanteda
```

## Thoughts on Debugging Travis Problems

You may be thinking at this point, "How did she know to do those things specifically?" and that is an excellent question. If you browse the [history](https://github.com/juliasilge/tidytext/commits/master/.travis.yml) of the `.travis.yml` for tidytext, you can see that things were a MESS for a while.

<a href="https://imgflip.com/i/14hqh5"><img src="https://i.imgflip.com/14hqh5.jpg" title="Build Failing"/></a>

Let me share some of the things that have been helpful for me as I have tried to solve problems with building packages on Travis.

Hadley Wickham's *R Packages* book has a [section on Travis](http://r-pkgs.had.co.nz/check.html#travis) that is short but helpful; it is within the section on [automated checking](http://r-pkgs.had.co.nz/check.html) that also has relevant helpful information. The [R docs at Travis](https://docs.travis-ci.com/user/languages/r/) itself are also quite helpful, although when I am having a real headscratcher of a problem I always wish there was more detail.

I tend to end up Googling a lot of things when I am having a problem, looking for packages similar to mine to see how they have solved similar challenges. One thing to be aware of in that situation is that the way Travis has dealt with R projects has changed pretty significantly in the not-so-distant past. If you find an R package that started building on Travis before certain changes took place, their `.travis.yml` will look VERY different than what you will need to use, given that you are starting a new project today. There were changes with containers, sudo, etc. so a slightly older package will be grandfathered in to an older way of building on Travis than a new project. I ran into this in a pretty significant way when trying to solve the problem with building the XML package. Finding other packages that depended on the XML package *did* help me eventually fix my problems, but I couldn't do it in the same way as the older packages.

## Unit Tests

One other aspect I want to mention specifically is that Travis will also run all the unit tests in your package (such as those you have written using [testthat](http://r-pkgs.had.co.nz/tests.html)) and the build will fail if any of your tests fail. I am very new to the worlds of unit testing and code coverage and all of that, but even so, I have seen how valuable it has been to immediately know when a change I have made breaks something in the code somewhere. Formal automated testing plus continuous integration equals less headache.

## The End

I would like to thank [Jim Hester](http://www.jimhester.com/), who gave me feedback on a draft of this post and also helped me when I was struggling with getting Travis set up on my first R package. I'll be honest; it was pretty rough and perplexing that first time I tried to set things up. Every time I have used CI since then has gone more smoothly and left me feeling more knowledgeable, though, and I am a believer now in the benefits and importance of this tool. At the same time, I am far from an expert, so feel free to chime in with feedback or thoughts, especially if I have said anything actually wrong about using Travis for continuous integration for R projects (please noooooooooooo...). I have shared pretty much the extent of my knowledge here, but I will attempt to answer questions if anyone has any.