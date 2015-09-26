# Reproducible Research: Peer Assessment 2
# Impact of Severe Weather Events on Public Health and Economy in the United States
# Synopsis
# This document is used to analyze the impact of stroms and other severe weather events on public health and economy.
# The data analysis mainly address the following questions: (1) Across the United States, which types of events 
# (as indicated in the EVTYPE variable) are most harmful with respect to population health. (2) Across the United States, 
# which types of events have the greatest economic consequences.

# Data Processing
setwd("C:/Users/c3179782/Desktop/R/Coursera/Lecture5_project2")

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(R.utils)
require(gridExtra)

## download files
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file <- "stormdata.csv.bz2"
if(!file.exists(file)){
  download.file(url, file)
}

#unzip and create folders (if those ain't exist)
unzip <- bzfile("stormData.csv.bz2", "r")
stormData <- read.csv(unzip, stringsAsFactors = FALSE)
close(unzip)

dim(stormData)
names(stormData)
head(stormData)

# list the stormData by year from 1950 to 2010
if (dim(stormData)[2] == 37) {
  stormData$year <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
}
hist(stormData$year, breaks = 50)

# subset the data from 1995 to 2010
storm <- stormData[stormData$year > 1980, ]
dim(storm)


# Impact on public health
# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful 
# with respect to population health?

##sum of fatalities by event type, sort them 
fatalities <- aggregate(FATALITIES~EVTYPE,storm,sum)
sort.fatalities <- fatalities[order(-fatalities$FATALITIES),]
fatalities30 <- sort.fatalities[1:30,]

# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful 
# with respect to population health?

##sum of injuries by event type, sort them 
injuries <- aggregate(INJURIES~EVTYPE,storm,sum)
sort.injuries <- injuries[order(-injuries$INJURIES),]
injuries30<- sort.injuries[1:30,]

head(fatalities30)
head(injuries30)


# Across the United States, which types of events have the greatest economic consequences?
## we focus on the property damage and crop damage that types of events cause in order to see the most costly.
unique(storm$PROPDMGEXP)

##lets make them numeric by the multipliers in data documentation
storm$PROPEXPN[storm$PROPDMGEXP ==""] <- 1
storm$PROPEXPN[storm$PROPDMGEXP =="-"] <- 0
storm$PROPEXPN[storm$PROPDMGEXP =="?"] <- 0
storm$PROPEXPN[storm$PROPDMGEXP =="+"] <- 0
storm$PROPEXPN[storm$PROPDMGEXP =="0"] <- 1
storm$PROPEXPN[storm$PROPDMGEXP =="1"] <- 1
storm$PROPEXPN[storm$PROPDMGEXP =="2"] <- 100
storm$PROPEXPN[storm$PROPDMGEXP =="3"] <- 1000
storm$PROPEXPN[storm$PROPDMGEXP =="4"] <- 10000
storm$PROPEXPN[storm$PROPDMGEXP =="5"] <- 100000
storm$PROPEXPN[storm$PROPDMGEXP =="6"] <- 1e+06
storm$PROPEXPN[storm$PROPDMGEXP =="7"] <- 1e+07
storm$PROPEXPN[storm$PROPDMGEXP =="8"] <- 1e+08
storm$PROPEXPN[storm$PROPDMGEXP =="B"] <- 1e+09
storm$PROPEXPN[storm$PROPDMGEXP =="h"] <- 100
storm$PROPEXPN[storm$PROPDMGEXP =="H"] <- 100
storm$PROPEXPN[storm$PROPDMGEXP =="K"] <- 1000
storm$PROPEXPN[storm$PROPDMGEXP =="m"] <- 1e+06
storm$PROPEXPN[storm$PROPDMGEXP =="M"] <- 1e+06

storm$PROPDMGWITHEXP <- storm$PROPDMG * storm$PROPEXPN

pdmg <- aggregate(PROPDMGWITHEXP~EVTYPE,storm,sum)
sort.pdmg <- pdmg[order(-pdmg$PROPDMGWITHEXP),]
pdmg30 <- sort.pdmg[1:30,]



##sum of crop damage multiplied by their respective exponent by event type, sort them and get top 20
unique(storm$CROPDMGEXP) ##to see the levels

##lets make them numeric by the multipliers in data documentation
storm$CROPEXPN[storm$CROPDMGEXP == ""] <- 1
storm$CROPEXPN[storm$CROPDMGEXP == "?"] <- 0
storm$CROPEXPN[storm$CROPDMGEXP == "0"] <- 1
storm$CROPEXPN[storm$CROPDMGEXP == "2"] <- 10
storm$CROPEXPN[storm$CROPDMGEXP == "B"] <- 1e+9
storm$CROPEXPN[storm$CROPDMGEXP == "k"] <- 1000
storm$CROPEXPN[storm$CROPDMGEXP == "K"] <- 1000
storm$CROPEXPN[storm$CROPDMGEXP == "m"] <- 1e+06
storm$CROPEXPN[storm$CROPDMGEXP == "M"] <- 1e+06

storm$CROPDMGWITHEXP <- storm$CROPDMG * storm$CROPEXPN

cdmg <- aggregate(CROPDMGWITHEXP~EVTYPE,storm,sum)
sort.cdmg <- cdmg[order(-cdmg$CROPDMGWITHEXP),]
cdmg30 <- sort.cdmg[1:30,]

# Results
##List of the events with the largest number of fatalities.
fatalities30
##List of the events with the largest number of injuries.
injuries30

##Graphs of the fatalities and injuries by event.

library(ggplot2)## To create the plots
fatalitiesPlot <- ggplot(fatalities30,aes(EVTYPE,FATALITIES))+
                          geom_bar(stat="identity",fill="blue")+
                          coord_flip()+xlab("Weather Event Type")+
                          ylab("Fatalities")+
                          theme(axis.text.x = element_text(angle = 90, hjust = 1))
injuriesPlot <- ggplot(injuries30,aes(EVTYPE,INJURIES))+
                        geom_bar(stat="identity",fill="red")+
                        coord_flip()+
                        xlab("Weather Event Type")+
                        ylab("Injuries")+
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))

##putting the plots in a single row 
library(gridExtra)

## Loading required package: grid (some problem)
grid.arrange(fatalitiesPlot,injuriesPlot,ncol = 2)



#As can be seen in the graphs and tables; tornadoes have the most significant impact on 
#the public health in terms of injuries and fatalities. 
#Excessive heat is the 2nd leading cause of fatalities caused by severe weather events


# List of the events with the highest economical impact(considering properties).
pdmg30
# List of the events with the highest economical impact(considering crops).
cdmg30

#Graphs of the property and crop damages by event.
propertyPlot <- ggplot(pdmg30,aes(EVTYPE,PROPDMGWITHEXP))+
                geom_bar(stat="identity",fill="blue")+
                coord_flip()+
                xlab("Weather Event Type")+
                ylab("Property Damage in USD")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
cropPlot<- ggplot(cdmg30,aes(EVTYPE,CROPDMGWITHEXP))+
                geom_bar(stat="identity",fill="red")+
                coord_flip()+xlab("Weather Event Type")+
                ylab("Crop Damage in USD")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

##putting the plots in a single row 
library(gridExtra)

##putting the plots in a single row (some problem)
grid.arrange(propertyPlot,cropPlot,ncol = 2)


