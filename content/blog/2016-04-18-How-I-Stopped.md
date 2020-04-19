---
title: "How I Learned to Stop Worrying and Love R CMD Check"
slug: "How-I-Stopped"
date: 2016-04-18
excerpt: "Resources for a First CRAN Submission"
tags: [rstats]
---



Last week, I officially became the maintainer of a CRAN package! My [package for the texts of Jane Austen's 6 completed, published novels](https://github.com/juliasilge/janeaustenr), `janeaustenr`, was released on CRAN and my Twitter feed was filled with congratulatory Jane Austen GIFs. I think this might be my favorite.

<blockquote class="twitter-tweet" data-conversation="none" data-lang="en"><p lang="en" dir="ltr">.<a href="https://twitter.com/juliasilge">@juliasilge</a> <br>*clears schedule*<br>*opens <a href="https://twitter.com/rstudio">@rstudio</a> * <a href="https://t.co/Hu7V2E0ULJ">pic.twitter.com/Hu7V2E0ULJ</a></p>&mdash; Andrew MacDonald (@polesasunder) <a href="https://twitter.com/polesasunder/status/721103557869436928">April 15, 2016</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

It was a good day.

During the process of getting `janeaustenr` ready to submit to CRAN, I was pointed to some resources that were very helpful to me as a first-time maintainer. I am putting these together here in this post, along with some of the mistakes that I made and things I learned. 

First of all, did you know that you can't have the word "package" in the title field of your `DESCRIPTION` file? I did not, but now we both do.

Also, are you taking advantage of the tools for package building and using Git/Github that are in RStudio? I was not, until I started getting this package ready for submission to CRAN. I was doing everything in the console/terminal and I did not have a `.proj` file. Things are much better now. (I mostly have the [ROpenSci meeting](http://juliasilge.com/blog/I-Went-to-ROpenSci/) to thank for this; watching someone else use these tools in person made me switch.)

OK, with those out of the way, here are a handful of very helpful resources for getting a package ready for submission to CRAN.

* [The last chapter ("Release") of Hadley Wickham's _R Packages_ book](http://r-pkgs.had.co.nz/release.html)
* [Karl Broman's post on getting an R package ready for CRAN](http://kbroman.org/pkg_primer/pages/cran.html)
* [François Briatte's post on some specific mistakes to watch out for before submitting to CRAN](http://f.briatte.org/r/submitting-packages-to-cran)

## Errors and Warnings and Notes, OH MY!

If you've gotten to the point of working toward putting your package on CRAN, you likely already know that you have to fix anything in your code that is causing `R CMD check` to give an `ERROR`, and almost certainly you need to fix anything causing a `WARNING` or a `NOTE`. When I asked for resources and advice on Twitter for my first submission to CRAN, the overwhelmingly most common response was "Get rid of all your `NOTE`s!" "NO `NOTE`S!!!" And so forth. I had been using [Travis-CI](https://travis-ci.org/) for my packages and that helps get you most of the way there, but a `NOTE` will not stop your package from passing on Travis.

One of the things one must do to get a package ready for CRAN is to make sure it will build correctly on Windows and on R-devel, which is what `build_win` is for. I did not realize before doing this, but `build_win` accesses the actual list of package maintainers, so if you are a first-time maintainer, you will get a note on `build_win` about being a new maintainer. When I first started running `build_win` I got a lot of new notes (and some messages I hadn't seen before too) so it was confusing to disentangle what was going on. Just know that if you are not already a maintainer on CRAN, you will automatically get a (an additional?) `NOTE` when you run `build_win` for being a new maintainer, and that's not unexpected or a problem.

<a href="https://imgflip.com/i/12m9bj"><img src="https://i.imgflip.com/12m9bj.jpg" title="No Idea UseR Dog"/></a>

During the course of getting my package ready, I fixed lots of actual problems, things I had forgotten to add to `.Rbuildignore`, etc. It still seems puzzling to me that [`globalVariables.R`](https://github.com/juliasilge/janeaustenr/blob/master/R/globalVariables.R) is necessary, to be perfectly honest, but whatever, I made it to stop `R CMD check` from complaining. One `NOTE` that I waffled back and forth about, though, was about marked UTF-8 strings. One thing that `R CMD check` does is check the data in a package for non-ASCII characters. There are two such characters in the texts of Jane Austen's novels.


```{r}
library(janeaustenr)
data("mansfieldpark")
mansfieldpark[14652]
```



```
## [1] "the command of her beauty, and her Ł20,000, any one who could satisfy the"
```



```{r}
data("persuasion")
persuasion[7066]
```



```
## [1] "concert.  Something so formal and _arrangé_ in her air!  and she sits so"
```

We've got one British pound symbol and one "e" with an accent aigu. People were so adamant that I must eradicate all `NOTE`s and I am such a rule-follower/people-pleaser that at one point in the past week I changed these characters to a regular "e" and the phrase "20,000 pounds". Then I thought, "WHAT AM I DOING?!?!" These are the actual texts of the published books and I do not think I should be changing them. I went back to the original versions and I left the `NOTE`, with an explanation of exactly where/what the `NOTE` was for in my [CRAN comments](https://github.com/juliasilge/janeaustenr/blob/master/cran-comments.md). The CRAN maintainers seemed fine with this explanation and did not ask me to change anything about this. I'm not saying that I disagree with everybody who was telling me to take `NOTE`s seriously and to fix them, but this was the direction I decided to go because of the context of my package and what my experience was.

## But That IS How You Spell "Northanger"

When I ran `build_win` on my package (I think? I will admit I am not 100% sure when I saw these for the first time) I saw a handful of messages that were not marked as `NOTE`s per se but were addressing possible problems. There were two types of messages. The first was about possible misspellings; in my CRAN comments, I noted the words that were flagged and just said they were spelled correctly. The second was about URLs that were possibly wrong. These were almost all Project Gutenberg URLs from my documentation. Project Gutenberg doesn't like automated traffic very much; in fact, when I was doing my processing of the text files, sometimes it would kick me off for accessing the site too often too quickly and I would have to click through to the website to fill in a captcha. I made a note in my CRAN comments about Project Gutenberg blocking automated traffic. You can see how I worded all of this in my actual [markdown CRAN comments file here](https://github.com/juliasilge/janeaustenr/blob/master/cran-comments.md), if you'd like.

There was one URL that had a message that it turned out I *did* need to change. I had added the badge for CRAN to my `README` file so I could have the great pleasure of seeing it change from brown to green. I had copied the code for the badge from someone somewhere and just changed the applicable bits. I originally had the URL as `http://cran.r-project.org/package=janeaustenr` and this was marked as a possible problem, but I assumed this was only because `janeaustenr` was not actually on CRAN yet. It turns out that it is supposed to be `https://cran.r-project.org/package=janeaustenr`, though. This was the only change the CRAN maintainers asked me to make when I first submitted my package; you can see in my CRAN comments file how I explained my change there and my resubmission.

## Time to Lawyer Up

One thing I contemplated a lot was whether I needed to address any licensing issues with using the texts of Jane Austen's works, or really more about how I had accessed the texts through [Project Gutenberg](https://www.gutenberg.org/). I asked smart people for advice and got quite diverse responses. I think this is an example where people who write software have sort of adjacent experience but are not normally experts, or at least where there is not consensus on what would be the right thing to do. In the end, I decided to take the approach that Jane Austen's works are in the public domain, and thus I did not need to make the license of my package include the Project Gutenberg license. I am clear in the package documentation where the texts were sourced from and how they were processed, but my `LICENSE` file, for example, doesn't include anything about Project Gutenberg or their license. There are other packages on CRAN that also use public domain book texts sourced from Project Gutenberg as data files, and they also don't include any licensing for Project Gutenberg. After all my rumination and spamming of busy people with this, it didn't seem to bother the CRAN maintainers one bit; they didn't mention it at all.

## The End

Look, [my package is on CRAN](https://cran.r-project.org/web/packages/janeaustenr/index.html)! I had so much help and advice from so many lovely people in getting this submission ready; THANK YOU, EVERYONE. I know I have said this before, but the R community has been so welcoming and supportive to me as a newcomer and a learner and it is enormously helpful. Now that `janeaustenr` is on CRAN, my next non-day-job project is to work on [`tidytext`](https://github.com/juliasilge/tidytext) and get it up all shipshape. The R Markdown file used to make this blog post is available [here](https://github.com/juliasilge/juliasilge.github.io/blob/master/_R/2016-04-18-How-I-Stopped.Rmd). I am very happy to hear feedback or questions, or more importantly, if anything I have said here about CRAN or packages is actually wrong (oh noooooooooooo...), feel free to chime in.
