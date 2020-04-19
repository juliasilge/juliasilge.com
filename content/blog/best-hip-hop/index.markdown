---
title: "PCA and the #TidyTuesday best hip hop songs ever"
date: 2020-04-14
slug: "best-hip-hop"
tags: [rstats,tidymodels]
categories: [rstats,tidymodels]
image:
  preview_only: true
---



Lately I've been publishing [screencasts](https://juliasilge.com/tags/tidymodels/) demonstrating how to use the tidymodels framework, from first steps in modeling to how to tune more complex models. Today, I'm exploring a different part of the tidymodels framework; I'm showing how to implement principal component analysis via recipes with this week's [`#TidyTuesday` dataset](https://github.com/rfordatascience/tidytuesday) on the best hip hop songs of all time as determinded by a BBC poll of music critics.

<!--html_preserve-->{{% youtube "OvgzIx5mDNM" %}}<!--/html_preserve-->

</br>

Here is the code I used in the video, for those who prefer reading instead of or in addition to video.

## Explore the data

Our modeling goal here is to understand what kind of songs are more highly rated by music critics in the [#TidyTuesday dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-14/readme.md) on hip hop songs. We'll use principal component analysis and audio features available in the Spotify API to do this! 🎵

First, let's look at the data on the rankings.


```r
library(tidyverse)

rankings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv")

rankings
```

```
## # A tibble: 311 x 12
##       ID title   artist   year gender points     n    n1    n2    n3    n4    n5
##    <dbl> <chr>   <chr>   <dbl> <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1     1 Juicy   The No…  1994 male      140    18     9     3     3     1     2
##  2     2 Fight … Public…  1989 male      100    11     7     3     1     0     0
##  3     3 Shook … Mobb D…  1995 male       94    13     4     5     1     1     2
##  4     4 The Me… Grandm…  1982 male       90    14     5     3     1     0     5
##  5     5 Nuthin… Dr Dre…  1992 male       84    14     2     4     2     4     2
##  6     6 C.R.E.… Wu-Tan…  1993 male       62    10     3     1     1     4     1
##  7     7 93 ’Ti… Souls …  1993 male       50     7     2     2     2     0     1
##  8     8 Passin… The Ph…  1992 male       48     6     3     2     0     0     1
##  9     9 N.Y. S… Nas      1994 male       46     7     1     3     1     1     1
## 10    10 Dear M… 2Pac     1995 male       42     6     2     1     1     2     0
## # … with 301 more rows
```

As a first step, let's recreate the plot from the [source material](https://blog.datawrapper.de/best-hip-hop-songs-of-all-time-visualized/), but adjusted a bit.


```r
rankings %>%
  ggplot(aes(year, points, color = gender)) +
  geom_jitter(alpha = 0.7) +
  scale_y_log10() +
  labs(
    y = "Critic rating",
    x = NULL,
    color = NULL
  )
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-3-1.png" width="2400" />

To see more examples of EDA for this dataset, you can see the great work [that folks share on Twitter](https://twitter.com/hashtag/tidytuesday)! ✨ Next, let's get audio features from the Spotify API.

## Get audio features

Spotify makes a [set of "audio features" available](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/) in its API. This includes features like whether the song is in a major or minor key, the liveness, the instrumentalness, the danceability, and many others. One option to work with these songs would be to get them all at once via a [playlist that Tom Mock made](https://open.spotify.com/playlist/7esD007S7kzeSwVtcH9GFe?si=IHkRIwQoRjqYijDTmAURxQ).


```r
library(spotifyr)
access_token <- get_spotify_access_token()

playlist_features <- get_playlist_audio_features("tmock1923", "7esD007S7kzeSwVtcH9GFe")

playlist_features
```

```
## # A tibble: 250 x 61
##    playlist_id playlist_name playlist_img playlist_owner_… playlist_owner_…
##    <chr>       <chr>         <chr>        <chr>            <chr>
##  1 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  2 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  3 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  4 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  5 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  6 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  7 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  8 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
##  9 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
## 10 7esD007S7k… Top 250 Hiph… https://mos… tmock1923        tmock1923
## # … with 240 more rows, and 56 more variables: danceability <dbl>,
## #   energy <dbl>, key <int>, loudness <dbl>, mode <int>, speechiness <dbl>,
## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
## #   tempo <dbl>, track.id <chr>, analysis_url <chr>, time_signature <int>,
## #   added_at <chr>, is_local <lgl>, primary_color <lgl>, added_by.href <chr>,
## #   added_by.id <chr>, added_by.type <chr>, added_by.uri <chr>,
## #   added_by.external_urls.spotify <chr>, track.artists <list>,
## #   track.available_markets <list>, track.disc_number <int>,
## #   track.duration_ms <int>, track.episode <lgl>, track.explicit <lgl>,
## #   track.href <chr>, track.is_local <lgl>, track.name <chr>,
## #   track.popularity <int>, track.preview_url <chr>, track.track <lgl>,
## #   track.track_number <int>, track.type <chr>, track.uri <chr>,
## #   track.album.album_type <chr>, track.album.artists <list>,
## #   track.album.available_markets <list>, track.album.href <chr>,
## #   track.album.id <chr>, track.album.images <list>, track.album.name <chr>,
## #   track.album.release_date <chr>, track.album.release_date_precision <chr>,
## #   track.album.total_tracks <int>, track.album.type <chr>,
## #   track.album.uri <chr>, track.album.external_urls.spotify <chr>,
## #   track.external_ids.isrc <chr>, track.external_urls.spotify <chr>,
## #   video_thumbnail.url <lgl>, key_name <chr>, mode_name <chr>, key_mode <chr>
```

This would be perfect for exploring the audio features on their own. On the other hand, this is going to be pretty difficult to match up to the songs in the `rankings` dataset because both the titles and artists are significantly different, so let's take a different approach. Let's create a little function to find the Spotify track identifier via `search_spotify()` (Spotify has already handled search pretty well) and use `purrr::map()` to apply it to all the songs we have in our dataset.


```r
pull_id <- function(query) {
  search_spotify(query, "track") %>%
    arrange(-popularity) %>%
    filter(row_number() == 1) %>%
    pull(id)
}

ranking_ids <- rankings %>%
  mutate(
    search_query = paste(title, artist),
    search_query = str_to_lower(search_query),
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))

ranking_ids %>%
  select(title, artist, id)
```

```
## # A tibble: 311 x 3
##    title                  artist                            id
##    <chr>                  <chr>                             <chr>
##  1 Juicy                  The Notorious B.I.G.              5ByAIlEEnxYdvpnezg7…
##  2 Fight The Power        Public Enemy                      1yo16b3u0lptm6Cs7lx…
##  3 Shook Ones (Part II)   Mobb Deep                         4nASzyRbzL5qZQuOPjQ…
##  4 The Message            Grandmaster Flash & The Furious … 5DuTNKFEjJIySAyJH1y…
##  5 Nuthin’ But A ‘G’ Tha… Dr Dre ft. Snoop Doggy Dogg       3j3mo94s22aHdrFqede…
##  6 C.R.E.A.M.             Wu-Tang Clan                      119c93MHjrDLJTApCVG…
##  7 93 ’Til Infinity       Souls of Mischief                 0PV1TFUMTBrDETzW6KQ…
##  8 Passin’ Me By          The Pharcyde                      4G3dZN9o3o2X4VKwt4C…
##  9 N.Y. State Of Mind     Nas                               5zwz05jkQVT68CjUpPw…
## 10 Dear Mama              2Pac                              6tDxrq4FxEL2q15y37t…
## # … with 301 more rows
```

At the end of that, there are 6% of songs that I failed to find a Spotify track identifier for. Not too bad!

Now that we have the track identifiers, we can get the audio features. The function `get_track_audio_features()` can only take 100 tracks at most at once, so let's divide up our tracks into smaller chunks and then `map()` through them.


```r
ranking_features <- ranking_ids %>%
  mutate(id_group = row_number() %/% 80) %>%
  select(id_group, id) %>%
  nest(data = c(id)) %>%
  mutate(audio_features = map(data, ~ get_track_audio_features(.$id)))

ranking_features
```

```
## # A tibble: 4 x 3
##   id_group data              audio_features
##      <dbl> <list>            <list>
## 1        0 <tibble [79 × 1]> <tibble [79 × 18]>
## 2        1 <tibble [80 × 1]> <tibble [80 × 18]>
## 3        2 <tibble [80 × 1]> <tibble [80 × 18]>
## 4        3 <tibble [72 × 1]> <tibble [72 × 18]>
```

We have audio features! 🎉 Now let's put that together with the rankings and create a dataframe for modeling.


```r
ranking_df <- ranking_ids %>%
  bind_cols(ranking_features %>%
    select(audio_features) %>%
    unnest(audio_features)) %>%
  select(title, artist, points, year, danceability:tempo) %>%
  na.omit()

ranking_df
```

```
## # A tibble: 293 x 15
##    title artist points  year danceability energy   key loudness  mode
##    <chr> <chr>   <dbl> <dbl>        <dbl>  <dbl> <int>    <dbl> <int>
##  1 Juicy The N…    140  1994        0.889  0.816     9    -4.67     1
##  2 Figh… Publi…    100  1989        0.797  0.582     2   -13.0      1
##  3 Shoo… Mobb …     94  1995        0.637  0.878     6    -5.51     1
##  4 The … Grand…     90  1982        0.947  0.607    10   -10.6      0
##  5 Nuth… Dr Dr…     84  1992        0.766  0.559    11    -7.03     0
##  6 C.R.… Wu-Ta…     62  1993        0.479  0.549    11   -10.6      0
##  7 93 ’… Souls…     50  1993        0.59   0.672     1   -11.8      1
##  8 Pass… The P…     48  1992        0.759  0.756     4    -8.14     0
##  9 N.Y.… Nas        46  1994        0.665  0.91      6    -4.68     0
## 10 Dear… 2Pac       42  1995        0.773  0.54      6    -7.12     1
## # … with 283 more rows, and 6 more variables: speechiness <dbl>,
## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
## #   tempo <dbl>
```

How are these quantities correlated with each other?


```r
library(corrr)

ranking_df %>%
  select(year:tempo) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan")) +
  theme_plex()
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-8-1.png" width="3000" />

Louder songs have higher energy, and older songs tend to be more danceable and have higher valence (i.e. be more "happy").

Let's train a linear model on these audio features.


```r
ranking_lm <- ranking_df %>%
  select(-title, -artist) %>%
  lm(log(points) ~ ., data = .)

summary(ranking_lm)
```

```
##
## Call:
## lm(formula = log(points) ~ ., data = .)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -1.74734 -0.57186  0.04495  0.43141  2.66998
##
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)      64.603559  13.723105   4.708 3.95e-06 ***
## year             -0.031233   0.006744  -4.631 5.58e-06 ***
## danceability      0.088747   0.434773   0.204    0.838
## energy            0.034760   0.433454   0.080    0.936
## key               0.004287   0.013739   0.312    0.755
## loudness          0.035091   0.022764   1.541    0.124
## mode             -0.078376   0.103268  -0.759    0.449
## speechiness      -0.182031   0.408076  -0.446    0.656
## acousticness      0.442155   0.300148   1.473    0.142
## instrumentalness  0.621053   0.622244   0.998    0.319
## liveness          0.140567   0.262802   0.535    0.593
## valence          -0.336922   0.286438  -1.176    0.240
## tempo             0.001175   0.001637   0.718    0.473
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.8285 on 280 degrees of freedom
## Multiple R-squared:  0.09559,	Adjusted R-squared:  0.05683
## F-statistic: 2.466 on 12 and 280 DF,  p-value: 0.004471
```

We only have evidence for year being important in the critic ratings from this model. We know that some of the features are at least a bit correlated, though, so let's use PCA.

## Principal component analysis

We can use the [recipes](https://tidymodels.github.io/recipes/) package to implement PCA in tidymodels.


```r
library(tidymodels)

ranking_rec <- recipe(points ~ ., data = ranking_df) %>%
  update_role(title, artist, new_role = "id") %>%
  step_log(points) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

ranking_prep <- prep(ranking_rec)

ranking_prep
```

```
## Data Recipe
##
## Inputs:
##
##       role #variables
##         id          2
##    outcome          1
##  predictor         12
##
## Training data contained 293 data points and no missing data.
##
## Operations:
##
## Log transformation on points [trained]
## Centering and scaling for year, danceability, energy, key, loudness, ... [trained]
## PCA extraction with year, danceability, energy, key, loudness, ... [trained]
```

Let's walk through the steps in this recipe.

- First, we must tell the `recipe()` what our model is going to be (using a formula here) and what data we are using.
- Next, we update the role for title and artist, since these are variables we want to keep around for convenience as identifiers for rows but are not a predictor or outcome.
- Next, we take the log of the outcome (`points`, the critic ratings).
- We need to center and scale the numeric predictors, because we are about to implement PCA.
- Finally, we use `step_pca()` for the actual principal component analysis.

Before using `prep()` these steps have been defined but not actually run or implemented. The `prep()` function is where everything gets evaluated.

Once we have that done, we can both explore the results of the PCA and then eventually use it in a model. Let's start with checking out how the PCA turned out. We can `tidy()` any of our recipe steps, including the PCA step, which is the third step. Then let's make a visualization to see what the components look like.


```r
tidied_pca <- tidy(ranking_prep, 3)

tidied_pca %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component) +
  labs(y = NULL)
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-11-1.png" width="1800" />

Let's zoom in on the first four components.


```r
library(tidytext)

tidied_pca %>%
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
  group_by(component) %>%
  top_n(6, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-12-1.png" width="2400" />

So PC1 is mostly about age and danceability, PC2 is mostly energy and loudness, PC3 is mostly speechiness, and PC4 is about the musical characteristics (actual key and major vs. minor key).

How are the songs distributed in the plane of the first two components?


```r
juice(ranking_prep) %>%
  ggplot(aes(PC1, PC2, label = title)) +
  geom_point(alpha = 0.2) +
  geom_text(check_overlap = TRUE, family = "IBMPlexSans")
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-13-1.png" width="3000" />

- Older, more danceable songs are to the left.
- Higher energy, louder songs are towards the top.

You can change out `PC2` for `PC3`, for example, to instead see where more "speechy" songs are.

How much variation are we capturing?


```r
sdev <- ranking_prep$steps[[3]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation ## use cumsum() to find cumulative, if you prefer
) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_variation)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent variance explained by each PCA component")
```

<img src="/blog/2020/2020-04-14-best-hip-hop_files/figure-html/unnamed-chunk-14-1.png" width="2400" />


And finally, let's fit the same kind of model we fit before, but now with `juice(ranking_prep)`. This approach really emphasizes how recipes can be used for data preprocessing. Notice how `juice(ranking_prep)` has already taken the log of `points`, has the component values ready to go, etc.


```r
juice(ranking_prep)
```

```
## # A tibble: 293 x 8
##    title          artist              points     PC1    PC2    PC3    PC4    PC5
##    <fct>          <fct>                <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1 Juicy          The Notorious B.I.…   4.94 -0.904   1.22  -0.964  0.629 -1.21
##  2 Fight The Pow… Public Enemy          4.61 -0.888  -1.45   0.581  0.684  1.84
##  3 Shook Ones (P… Mobb Deep             4.54  0.0894  1.23  -0.769  0.737  0.168
##  4 The Message    Grandmaster Flash …   4.50 -3.44    0.122 -0.122 -0.357  0.334
##  5 Nuthin’ But A… Dr Dre ft. Snoop D…   4.43 -3.10   -1.85   1.91  -5.89  -5.26
##  6 C.R.E.A.M.     Wu-Tang Clan          4.13  0.151  -2.15  -2.25  -0.161 -2.47
##  7 93 ’Til Infin… Souls of Mischief     3.91  0.413  -0.746 -0.450  2.18  -0.999
##  8 Passin’ Me By  The Pharcyde          3.87 -0.974   0.307 -0.616 -0.282  1.15
##  9 N.Y. State Of… Nas                   3.83 -0.739   2.04  -1.28  -0.668  0.705
## 10 Dear Mama      2Pac                  3.74 -0.202  -0.805  1.17   0.101 -0.107
## # … with 283 more rows
```

```r
pca_fit <- juice(ranking_prep) %>%
  select(-title, -artist) %>%
  lm(points ~ ., data = .)

summary(pca_fit)
```

```
##
## Call:
## lm(formula = points ~ ., data = .)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -1.59912 -0.57245  0.03614  0.41393  2.88917
##
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.925161   0.049480  38.908   <2e-16 ***
## PC1         -0.080409   0.034782  -2.312   0.0215 *
## PC2          0.032472   0.037961   0.855   0.3930
## PC3         -0.057849   0.042559  -1.359   0.1751
## PC4         -0.051924   0.045053  -1.153   0.2501
## PC5          0.006719   0.046864   0.143   0.8861
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.847 on 287 degrees of freedom
## Multiple R-squared:  0.0313,	Adjusted R-squared:  0.01442
## F-statistic: 1.855 on 5 and 287 DF,  p-value: 0.1023
```


So what did we find? There is some evidence here that older, more danceable, higher valence songs (PC1) were rated higher by critics.

