---
title: "Reproducible Research: Peer Assessment 2"
output: 
  html_document:
    keep_md: true
---

## Project

Goal - To explore the NOAA Storm Database regarding severe weather events.

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

The data for this assignment is Storm Data from the National Weather Service. Documentation for the data can be found at the [National Weather Service](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) and [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

## Data Processing


```r
### start necessary libraries
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ───────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
library(ggplot2)

### set figure save path to figure/
knitr::opts_chunk$set(fig.path = 'figure/')
```

## Download data

Data comes in the form of a comma-separated-value file compressed via the bzip2 algorithm.


```r
### download data ZIP file if necessary, and extract if necessary
if (!file.exists('StormData.csv.bz2')) {
  if (download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2','StormData.csv.bz2')) {
    stop('Unable to download data file')
  }
}

StormData <- read.csv(bzfile('StormData.csv.bz2'))
```

Variables of interest

Variable    | Description                 
------------|-----------------------------
BGN_DATE    | date of event starting      
FATALATIES  | number of fatalities        
INJURIES    | number of injuries          
PROPDMG     | value of damage in dollars



PROPDMGEXP - property damage dollar multiplier (K thousands, M millions, B billions)
CROPDMG
CROPDMGEXP

