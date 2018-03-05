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

StormData <- as.tibble(read.csv(bzfile('StormData.csv.bz2')))
```

Variables of interest

Variable    | Description                 
============|=============================
BGN_DATE    | date of event starting
BGN_TIME    | time of event starting
TIME_ZONE   | time zone of event
------------|-----------------------------
EVTYPE      | type of event
------------|-----------------------------
FATALITIES  | number of fatalities        
INJURIES    | number of injuries
------------|-----------------------------
PROPDMG     | value of property damage in dollars
PROPDMGEXP  | property damage dollar multiplier (K thousands, M millions, B billions)
------------|-----------------------------
CROPDMG     | value of crop damage in dollars
CROPDMGEXP  | crop damage dollar multiplier (K thousands, M millions, B billions)


```r
print(unique(StormData$TIME_ZONE))
```

```
##  [1] CST EST PST MST CDT PDT EDT UNK HST GMT MDT AST ADT CSt ESt CSC SCT
## [18] ESY UTC SST AKS GST
## 22 Levels: ADT AKS AST CDT CSC CSt CST EDT ESt EST ESY GMT GST HST ... UTC
```


```r
StormData$BEGIN_TIME <- strptime(paste(sub(' .*','',StormData$BGN_DATE),
                                       StormData$BGN_TIME),'%m/%d/%Y %H%M')

timezones <- c(-3,-9,-4,-5)
names(timezones) <- c('ADT','AKS','AST','CDT')
```
