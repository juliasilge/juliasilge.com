---
title: "Fatal Police Shootings Across the U.S."
slug: "Fatal-Shootings"
date: 2016-07-07
excerpt: "A flexdashboard exploring the Washington Post's data set on police shootings"
tags: [rstats]
---



I have been full of grief and sadness and some anger in the wake of yet more videos going viral in the past couple days showing black men being killed by police officers. I am not an expert on what it means to be a person of color in the United States or what is or isn't wrong with policing today here, but it sure feels like something is deeply broken. I was reminded today that the Washington Post is compiling a database of every fatal shooting in the United States by a police officer in the line of duty since January 1, 2015 and has made that database [publicly available on GitHub](https://github.com/washingtonpost/data-police-shootings). 

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">This is your regular reminder that we have the last 18 months of fatal police shooting data on Github: <a href="https://t.co/O6zP2JG7Ae">https://t.co/O6zP2JG7Ae</a></p>&mdash; Steven Rich (@dataeditor) <a href="https://twitter.com/dataeditor/status/751057842501783552">July 7, 2016</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

Their own visualizations and reporting are [online here](https://www.washingtonpost.com/graphics/national/police-shootings-2016/) and are great, and today I decided to make a flexdashboard exploring the Washington Post's data set, as it exists right now.

[![center](/figs/2016-07-07-Fatal-Shootings/shootingsdashboard.png)](https://beta.rstudioconnect.com/juliasilge/policeshooting/)

You can see and interact with the [flexdashboard here](https://beta.rstudioconnect.com/juliasilge/policeshooting/).

As I note there in the sidebar, these numbers are presented without any adjustment for demographics here in the U.S. If you look at the bar graph showing which states have the most fatal police shootings, those tend to be the highest population states. I have not yet done any analysis looking at, for example, which states have a disproportionate number of fatal police shootings or anything like that. Also, it isn't entirely clear yet if there are issues with underreporting that might bias these results. Is a shooting of a certain kind less likely to be reported in this data set? But for all those caveats, it is a start, and certainly we want to know where we are to move forward to a more just and peaceful world. 

The code for the flexdashboard is [here at this Gist](https://gist.github.com/juliasilge/9acbe97c549502bac85404779edceba0). I am happy to hear feedback or questions on it!


