# load relevant libraries
library(xlsx)
library(stringdist)
library(Hmisc)
library(dplyr)
library(ggplot2)

if(!file.exists("stormdata.csv.bz2")){download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","stormdata.csv.bz2")}

# determine if the data file needs to be read in or if a data.frame exists within the workspace already.
if(!exists("stormdata")){
      stormdata <- read.csv("stormdata.csv.bz2",header = TRUE)
}


# identify the data
names(stormdata)

# create properly formatted begin dates and then year
stormdata$BGN_DATE <- as.Date(as.character(stormdata$BGN_DATE),"%m/%d/%Y")
stormdata$year <- as.numeric(format(stormdata$BGN_DATE,"%Y"))

# first eliminate the rows with "summary" in EVTYPE
stormdata$EVTYPE <- toupper(stormdata$EVTYPE)
stormdata <-subset(stormdata,!grepl("SUMMARY",stormdata$EVTYPE))

# replace the TSTM with MARINE THUNDERSTORM so that it can be matched with amatch()
stormdata$EVTYPE<-gsub("TSTM","MARINE THUNDERSTORM",stormdata$EVTYPE)

# read in the categories from an externally generated file.
categories <- read.csv("categories.csv")
categories$EventName<-toupper(categories$EventName)

# now use amatch() to clean up misspelled event types.

stormdata$EVTYPE.cor <- categories[amatch(stormdata$EVTYPE,categories$EventName,method="soundex",maxDist = 120),"EventName"]

stormdata$EVTYPE.cor[grepl("LANDSLIDE",stormdata$EVTYPE)]<-"LANDSLIDE"

# read in the CPI data. Series information is contained in the first 16 rows of the spreadsheet

PriceLevel <- read.xlsx("GDPDEF.xls",sheetIndex = 1,startRow =17)

# Pull out the year to use for matching with storm data
PriceLevel$year <- substr(PriceLevel$DATE,0,4)

# restrict PriceLevel data to time frame of storm data
PriceLevel <- subset(PriceLevel,PriceLevel$year %in% 1950:2011)
pricelevel <- PriceLevel$VALUE
names(pricelevel) <- PriceLevel$year



# need to address problems with incorrectly coded PROPDMGEXP. 

stormdata <- subset(stormdata, stormdata$PROPDMGEXP %nin% c("-", "?", "+", "0" ,"1" ,"2" ,"3", "4" ,"5", "6" ,"7", "8","h","H"))
stormdata$PROPDMGEXP[stormdata$PROPDMGEXP=="m"]<-"M"
stormdata$PROPDMGEXP <- factor(stormdata$PROPDMGEXP)
stormdata$prop.multiples <- 0
stormdata$Prop.multiples[stormdata$PROPDMGEXP=="K"]<-1000
stormdata$prop.multiples[stormdata$PROPDMGEXP=="M"]<-1000000
stormdata$prop.multiples[stormdata$PROPDMGEXP=="B"]<-1000000000


# need to address problems with incorrectly coded CROPDMGEXP. 

stormdata <- subset(stormdata, stormdata$CROPDMGEXP %nin% c("?", "0" ,"2"))
stormdata$CROPDMGEXP[stormdata$CROPDMGEXP=="m"]<-"M"
stormdata$CROPDMGEXP <- factor(stormdata$CROPDMGEXP)
stormdata$crop.multiples <- 0
stormdata$crop.multiples[stormdata$CROPDMGEXP=="K"]<-1000
stormdata$crop.multiples[stormdata$CROPDMGEXP=="M"]<-1000000
stormdata$crop.multiples[stormdata$CROPDMGEXP=="B"]<-1000000000


# now add price level data to stormdata table and compute real costs
stormdata$pricelevel <- pricelevel[as.character(stormdata$year)]
stormdata$PROPDMG.real <- with(stormdata,multiples*100*PROPDMG/pricelevel)
stormdata$CROPDMG.real <- with(stormdata,100*CROPDMG/pricelevel)
