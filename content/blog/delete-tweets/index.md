---
title: "Delete all your tweets using rtweet"
author: Julia Silge
date: '2022-11-10'
format: hugo
slug: delete-tweets
categories:
  - rstats
tags:
  - rstats
summary: "Worried about how a certain social media platform is going and want to start removing yourself? Learn how to delete all your tweets."
---



If you are involved in data science social media, I expect that you are quite familiar with the turmoil on Twitter; it [doesn't look like it is getting better](https://twitter.com/CaseyNewton/status/1590724257608134657). Twitter, and specifically the data science community on Twitter, has brought so much positive to my life and career, and I feel pretty sad about the situation, both the massive layoffs and what looks like a real change for the worse in the platform.

If you are looking to remove yourself from Twitter, you can delete your account, but I've seen some folks say a better initial move may be to delete the content from your account (perhaps including followers and following), and then take your account private or deactivate it. In this blog post, I'll walk through how to use [rtweet](https://docs.ropensci.org/rtweet/) to automate some of these steps.

## Get your tweets

The first step I took was to [download my Twitter archive](https://help.twitter.com/en/managing-your-account/how-to-download-your-twitter-archive). It's my data, so I wanted to make sure I have it! Also, it helps a lot with deletion to have a local copy of all the tweet IDs; if you use `get_my_timeline()` from rtweet, the API only returns a small number of tweets.

Inside of the archive Twitter sends you, there is a file called `tweets.js` that contains all the info on your tweets. I manually opened this file and deleted the header `window.YTD.direct_messages.part0 =` so the file starts with the square bracket and can be read by jsonlite.

``` r
library(jsonlite)
my_tweets <- as_tibble(fromJSON("tweets.js")$tweet)
glimpse(my_tweets)
```

    Rows: 23,826
    Columns: 23
    $ edit_info                 <df[,1]> <data.frame[26 x 1]>
    $ retweeted                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ source                    <chr> "<a href=\"https://mobile.twitter.com\" rel=…
    $ entities                  <df[,5]> <data.frame[26 x 5]>
    $ display_text_range        <list> <"0", "127">, <"0", "101">, <"0", "28">, <"0…
    $ favorite_count            <chr> "0", "0", "0", "1", "6", "21", "14", "0",…
    $ id_str                    <chr> "1587130658895208453", "1587113691253592065…
    $ truncated                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ retweet_count             <chr> "0", "0", "0", "0", "0", "0", "1", "0", "2",…
    $ id                        <chr> "1587130658895208453", "1587113691253592065"…
    $ possibly_sensitive        <lgl> FALSE, FALSE, NA, FALSE, FALSE, NA, FALSE, N…
    $ created_at                <chr> "Mon Oct 31 17:13:31 +0000 2022", "Mon Oct 3…
    $ favorited                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ full_text                 <chr> "RT @sellorm: A little #rstats wrapper packa…
    $ lang                      <chr> "en", "en", "en", "en", "en", "en", "en", "e…
    $ in_reply_to_status_id_str <chr> NA, NA, NA, "1585833235837132800", "15858278…
    $ in_reply_to_user_id       <chr> NA, NA, NA, "14629263", "14629263", "1307404…
    $ in_reply_to_status_id     <chr> NA, NA, NA, "1585833235837132800", "15858278…
    $ in_reply_to_screen_name   <chr> NA, NA, NA, "JonTheGeek", "JonTheGeek", "jul…
    $ in_reply_to_user_id_str   <chr> NA, NA, NA, "14629263", "14629263", "1307404…
    $ extended_entities         <df[,1]> <data.frame[26 x 1]>
    $ withheld_copyright        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    $ withheld_in_countries     <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NUL…

WOW, so many tweets. I'm not quite ready to totally empty out my account, so let's only delete things older than three months.

``` r
library(lubridate)

delete_these_tweets <-
  my_tweets %>%
  mutate(created_at = as.POSIXct(created_at, format="%a %b %d %H:%M:%S +0000 %Y"),
         created_at = as.Date(created_at)) %>%
  filter(created_at < today() - days(90)) %>%
  select(created_at, id)

delete_these_tweets
```

    # A tibble: 23,640 × 2
       created_at id                 
       <date>     <chr>              
     1 2022-08-05 1555607029514809344
     2 2022-08-05 1555591167982518272
     3 2022-08-05 1555411107690426368
     4 2022-08-04 1555242813314326528
     5 2022-08-04 1555242504164741120
     6 2022-08-04 1555188866566717442
     7 2022-08-03 1554845178623447042
     8 2022-08-03 1554844667174277120
     9 2022-08-03 1554844356728594432
    10 2022-08-03 1554701603399032832
    # … with 23,630 more rows

What am I actually deleting here?

``` r
delete_these_tweets %>%
  ggplot(aes(created_at)) +
  geom_histogram() +
  labs(x = NULL, y = "Number of tweets")
```

<img src="index_files/figure-gfm/unnamed-chunk-3-1.png" width="1260" />

Again, WOW.

## Delete your tweets

We can use `post_destroy()` from rtweet to delete a tweet by single status ID. Yes, we are going to delete these one by one! You do need to [set up authentication](https://docs.ropensci.org/rtweet/articles/auth.html) so you can interact with Twitter from R.

``` r
library(rtweet)
post_destroy(delete_these_tweets$id[234])
```

Now it's time to delete *all* of them; I'll iterate through the tweet IDs using purrr. For this blog post, I'll only delete a random 10 of them.

``` r
result <-
  delete_these_tweets %>%
  slice_sample(n = 10) %>%
  mutate(success = map(id, possibly(post_destroy, otherwise = "fail")))

result
```

    # A tibble: 10 × 3
       created_at id                  success   
       <date>     <chr>               <list>    
     1 2012-01-12 157472718097362945  <response>
     2 2017-01-21 822848990127398912  <response>
     3 2017-03-02 836995502776315904  <response>
     4 2010-01-08 7496114114          <response>
     5 2019-01-19 1086502295167217664 <response>
     6 2017-08-30 902859096453373952  <response>
     7 2017-11-09 928338075691900929  <response>
     8 2017-01-24 823902254159654914  <response>
     9 2020-01-30 1222594762940669952 <response>
    10 2013-10-01 385061147793059840  <response>

There are rate limits on the Twitter API so we may not be able to get through all the tweets in one go. The saved `result` records which ones worked, so we can use `result %>% filter(success == "fail")` as the input to try deleting again.

## Next steps

Now that the bulk of my tweets are deleted and I'm only dealing with a couple hundred tweets, I will probably use `get_my_timeline()` moving forward to delete any more.

I'm not quite ready to remove all the people I follow and who follow me, but my plan to do so is:

- Use `get_friends()` to find the people I follow, and then iterate through with `post_unfollow_user()` to unfollow all of them.
- Use `get_followers()` to find the people who follow me, and then probably `user_block()` and immediately `user_unblock()` through the list so no one follows me anymore but I don't have those folks blocked long-term.

This is kind of a depressing blog post to write, but I am super happy to be able to handle my own Twitter data with tools that are so comfortable to me. If you are interested in exploring a new kind of social network, you can [find me on Mastodon](https://mastodon.social/@juliasilge).
