#Live Session 4 For Live Session Web Scraping Code

library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML

# Method 1: XML

data <-getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
zipcode <- xpathSApply(doc,"//zipcode",xmlValue)
dist <- xpathSApply(doc,"//councildistrict",xmlValue)
rests = data.frame(names,zipcode,dist)
rests
length(grep("SUSHI",rests$names))

dwntwn = rests %>% filter(dist=="11")
dwntwn