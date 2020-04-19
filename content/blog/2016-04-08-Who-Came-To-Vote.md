---
title: "Who Came to Vote in Utah's Caucuses?"
slug: "Who-Came-To-Vote"
date: 2016-04-08
excerpt: "Mapping Voter Turnout"
tags: [rstats]
---



Late last month, I analyzed results from Utah's Republican and Democratic caucuses to show [how the different presidential candidates fared across Utah](http://juliasilge.com/blog/Mapping-Utah-Caucus/). That was fun work to do, but I realized there was one more map I wanted to make; I want to compare the Republican and Democratic voter turnout across the counties in Utah. Utah is a politically conservative state and we know from the last plot I made in that post that many more people voted in the Republican caucus than the Democratic caucus, but I would like to see how voter turnout was distributed across the state. Ari Lamstein has organized an [R election analysis contest](http://www.arilamstein.com/blog/2016/03/28/announcing-r-election-analysis-contest/), so I will finish this up and contribute to the interesting projects that people are putting together.

First, let's download the caucus results from CNN.


```{r}
library(jsonlite)
utahRJSON <- fromJSON("http://data.cnn.com/ELECTION/2016primary/UT/county/S.json",
               flatten=TRUE)
utahDJSON <- fromJSON("http://data.cnn.com/ELECTION/2016primary/UT/county/E.json",
               flatten=TRUE)
```

Now, let's add up every vote cast for each candidate in each county, first for the Republicans and then for the Democrats.


```{r}
library(purrr)
library(dplyr)

votesR <- mutate(map_df(utahRJSON$counties$race.candidates, function(x) {
        x %>% summarise(votes = sum(votes))
        }), FIPS=utahRJSON$counties$countycode)
votesD <- mutate(map_df(utahDJSON$counties$race.candidates, function(x) {
        x %>% summarise(votes = sum(votes))
        }), FIPS=utahRJSON$counties$countycode)
```

Once again, `purrr` has made life so lovely and easy to understand. I am still getting my mind wrapped around it but WOW. 

## How Many People Can Vote?

Now we have the number of votes cast in each county, but we also need to know the number of people in each county who were eligible to vote. It turns out that this is a tricky thing to find out, and there [are different ways to estimate it](http://www.pewtrusts.org/en/research-and-analysis/analysis/2014/07/31/measuring-voter-turnout). I am going to use the [voting age  citizen population](https://www.census.gov/rdo/data/voting_age_population_by_citizenship_and_race_cvap.html) as estimated by the US Census. The Census provides a zip file for download [here](https://www.census.gov/rdo/data/voting_age_population_by_citizenship_and_race_cvap.html) that includes separate files for state, county, census tract, etc. One of the quantities in this file is the estimated total number of United States citizens 18 years of age or older for each county, which is what I want.


```{r}
library(stringr)
countiesCSV <- read.csv("./County.csv", stringsAsFactors = FALSE)
countiesVAP <- countiesCSV %>% filter(LNNUMBER == 1) %>%
        select(GEOID, VAP = CVAP_EST) %>% 
        mutate(FIPS = as.integer(str_extract(GEOID, ".{5}$")))
```

It is important to note that this estimate is not perfect or unambiguous. This number includes some people who are not eligible to vote, such as felons, and also it includes people who are not registered to vote; these people sometimes are and sometimes are not included in the denominator when reporting voter turnout. Anyway, let's blithely move on and join each of the data frames of votes cast to the data frame of voting age citizen population.



```{r}
votesR <- inner_join(votesR, countiesVAP)
votesD <- inner_join(votesD, countiesVAP)
```

Now let's calculate a voter turnout.


```{r}
votesR <- votesR %>% mutate(turnout = votes/VAP*100) %>% 
        mutate(region = FIPS, value = turnout)
votesD <- votesD %>% mutate(turnout = votes/VAP*100) %>% 
        mutate(region = FIPS, value = turnout)
```

We need `region` and `value` for the mapping. Speaking of which...

## Now, Maps!


```{r}
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
choro1 = CountyChoropleth$new(votesR)
choro1$set_zoom("utah")
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "Percent", 
                                           colours = brewer.pal(8, "Reds"),
                                           limits = c(0, 20))
choro2 = CountyChoropleth$new(votesD)
choro2$set_zoom("utah")
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Percent",
                                           colours = brewer.pal(8, "Blues"),
                                           limits = c(0, 12))
map1 <- choro1$render() + 
        labs(title = "Republican Caucus Turnout",
             subtitle = "Percent of voting age citizens who voted in the Republican caucus") +
        theme(plot.title=element_text(family="Roboto-Bold")) +
        theme(text=element_text(family="RobotoCondensed-Regular")) + 
        theme(plot.title=element_text(hjust = .15)) +
        theme(plot.subtitle=element_text(hjust = -2)) +
        theme(plot.subtitle=element_text(size=9))
map2 <- choro2$render() + 
        labs(title = "Democratic Caucus Turnout",
             subtitle = "Percent of voting age citizens who voted in the Democratic caucus") +
        theme(plot.title=element_text(family="Roboto-Bold")) +
        theme(text=element_text(family="RobotoCondensed-Regular")) +
        theme(plot.title=element_text(hjust = .15)) +
        theme(plot.subtitle=element_text(hjust = -1.5)) +
        theme(plot.subtitle=element_text(size=9))
grid.arrange(map1, map2, ncol = 2)
```

![center](/figs/2016-04-08-Who-Came-To-Vote/unnamed-chunk-7-1.png)


The Republican voter turnout (the percent of the total voting age citizen population who voted in the Republican caucus) is higher across the state than the Democratic voter turnout, as we would expect. What is going on over there in that eastern county with the highest Democratic turnout? That is Grand County, Utah, which is home to Moab, Arches National Park, and Canyonland National Park. There are only about 7000 voter age citizens in the whole county, but 797 of them voted in the Democratic caucus while 337 voted in the Republican caucus. Moab has a significant environmentalist influence because of the national parks and such. The other counties that had more significant Democratic voter turnout are Summit County, home to Park City and ski resorts galore, and Salt Lake City, my own home county where we have the largest city in Utah and more significant diversity.

## The End

Voter turnout is typically lower in primaries and caucuses than in a general election, but these numbers are quite low for what is being reported in this election cycle. The sum of the voter turnout for the Democratic and Republican caucuses in Utah has a median of 14.13% and this sum was only higher than 20% in one county. This is somewhat lower than what [has been reported](http://www.pewresearch.org/fact-tank/2016/03/08/so-far-turnout-in-this-years-primaries-rivals-2008-record/) for primaries and caucuses in other states this cycle. This is partly because there are so few Democrats in Utah, of course, but even if you just compare Republican turnout to typical Republican turnout this cycle, Utah still comes up short. Is this because different numbers are being used in the denominator (i.e. how is the pool of eligible voters defined)? It is hard to tell because people don't get very specific in the popular press. I learned that Utah does have a history of [comparatively low voter turnout](http://www.google.com/search?q=utah+voter+turnout). It is interesting that I started this analysis in the context of [reports of shockingly high turnout in the Democratic caucuses](http://www.sltrib.com/home/3694882-155/utah-dems-republicans-see-huge-turnout)! It turns out that much of that perception in both political parties was [due to Utah's recent shift from a primary to a caucus](http://kutv.com/news/local/utahs-caucus-not-the-turnout-blockbuster-many-might-think). The R Markdown file used to make this blog post is available [here](https://github.com/juliasilge/juliasilge.github.io/blob/master/_R/2016-04-08-Who-Came-To-Vote.Rmd). I am very happy to hear feedback or questions!
