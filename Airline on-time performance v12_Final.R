######################## Airline on-time performance  ########################################
## Title: Airline on-time performance
## Author: Sreekantha Thimmareddy
## Date: 10-10-2017
## Version: 2.0

# Packages required
install.packages("devtools")
#install.packages("rlang") # The package is failing hence using below frm github
devtools::install_github("tidyverse/rlang", build_vignettes = TRUE)
install.packages("shiny")
install.packages("dbplyr")
install.packages("digest")
install.packages("backports")
install.packages("sparklyr") #  If thee package is failing hence using below frm github
#devtools::install_github("rstudio/sparklyr")
install.packages("sqldf")
install.packages("corrplot")  # For plotting co-relation
install.packages("Hmisc") # For co-relation and P values
install.packages("MASS")  # StepAIC function to choose variables
install.packages("caret")
install.packages("R.utils")

library(corrplot)
library(R.utils) 
library(sqldf)
library(data.table)
library(MASS)
library(caret)
library(stats)
library(devtools) 
library(rlang)
library(shiny)
library(dbplyr)
library(digest)
library(backports)
library(sparklyr)
library(ggplot2)  #the cookbook for R: http://www.cookbook-r.com/Graphs/
library(dplyr)
library(DBI)  # For SQL on Spark

#########################################
## Loading and Processing Data

# Set Working dir
setwd("D:/Data Science/Flight Data Analysis/")
getwd()

#Download data from Web
download.file(url="http://stat-computing.org/dataexpo/2009/2007.csv.bz2", destfile = "./Data/new/2007.csv.bz2")
download.file(url="http://stat-computing.org/dataexpo/2009/2008.csv.bz2", destfile = "./Data/new/2008.csv.bz2")
download.file(url="http://stat-computing.org/dataexpo/2009/airports.csv", destfile = "./Data/new/airports.csv")
download.file(url="http://stat-computing.org/dataexpo/2009/carriers.csv", destfile = "./Data/new/carriers.csv")
download.file(url="http://stat-computing.org/dataexpo/2009/plane-data.csv", destfile = "./Data/new/plane_data.csv")

# Unzip files
bunzip2("./Data/new/2007.csv.bz2", "./Data/new/2007.csv", remove = FALSE, skip = TRUE)
bunzip2("./Data/new/2008.csv.bz2", "./Data/new/2008.csv", remove = FALSE, skip = TRUE)

# Loading data files and working with sample set
trips2007 <- fread(".\\Data\\2007.csv")  #Fastest way to load large files.
trips2008 <- fread(".\\Data\\2008.csv")

trips2007a <- sample_n(trips2007, 100000)
trips2008b <- sample_n(trips2008, 100000)

trips2007a <- as.data.table(trips2007a)
trips2008b <- as.data.table(trips2008b)

tripsfile <- rbind(trips2007a, trips2008b)
nrow(tripsfile)

# To free up unused files.
memory.size()
rm(trips2007, trips2008)


# Load other supporting files as well.
airportsfile <- read.csv2('.\\Data\\airports.csv',sep = ",",header = TRUE)
carriersfile <- read.csv2('.\\Data\\carriers.csv',sep = ",",header = TRUE)
planedatafile <- read.csv2('.\\Data\\plane_data.csv',sep = ",",header = TRUE)

# This is to have copy of raw data untouched available for any reference.
trips <- tripsfile
airports <- airportsfile
carriers <- carriersfile
planedata <- planedatafile

################################ Feature Engineering ################################ 

summary(trips)
# We can see there are ~40% of values are NAs for WeatherDelay, NASDelays, SecurityDelay, LateAircraftDelay.
head(filter(trips,  WeatherDelay == 0 ))
# We cannot eaither mark them as zero or backfill with other value. It's best to omit and use remaining data or exclude these variables in prediction. We can try both.
# And in other variables, small number of NAs present and we can omit them as well.

nrow(trips)

trips <- na.omit(trips)

nrow(trips)


# Creating hour variables as the time is in HHMM format and not suitable for analysis.
trips$ArrHour <- trunc(trips$CRSArrTime/100) 
trips$DepHour <- trunc(trips$CRSDepTime/100)

# Deviding day into 4 time periods for analysis
# have devided 24 hours into 4 buckets. i.e new variables DepTimeOfDay, ArrTimeOfDay
# 12am to 5:59 am as 0 (Night)
# 6am to 11:59 am as 1 (Morning) 
# 12noon to 5:59pm (Afternoon)
# 6pm to 11:59pm (Evening)

trips$ArrTimeofday <- trunc(trips$ArrHour/6)
trips$DepTimeofday <- trunc(trips$DepHour/6)

# To see week of month has any significance. It may.
trips$WeekOfMonth <- trunc(trips$DayofMonth/7+1)
#### Clearly based on data, the predictors/dependent variables are DepDelay and ArrDelay which is the total delay.
#Adding week of month.

#Now let us merge/join supporting data set. This is to make one big de-normalized flat table with all the data fields required available.
#This can be done after initial EDA as well to find out which fields ussually to be included/excluded.
#Since I did some exploration in previous versions of code so doing it ahead before EDA here. 
# First Plane data, not all fields are important. Also need to delete any NAs as we don't have source to backfill.
planedatanew <- planedata[, c("tailnum","type","manufacturer","aircraft_type","engine_type","year") ] 
planedatanew <- na.omit(filter(planedatanew, year != "" ))
planedatanew <- na.omit(planedatanew)

#Now Left join two tables
tripsplane <- merge(x=trips, y=planedatanew , by.x="TailNum", by.y="tailnum", all.x = TRUE)[]

# Now let's use airports

nrow(airports)
airportsnew <- na.omit(airports[, c("iata","city", "state", "country")])
nrow(airportsnew)

#Merging for Origin Airports
tripsplaneairports <- merge(x=tripsplane, y=airportsnew , by.x="Origin", by.y="iata", all.x = TRUE)[]
setnames(tripsplaneairports, old = c("city", "state", "country"), new= c("Origincity", "Originstate", "Origincountry"))

# Merging for Destination airports
tripsplaneairports <- merge(x=tripsplaneairports, y=airportsnew , by.x="Dest", by.y="iata", all.x = TRUE)[]

#Renaming the columns to reflect correctly
setnames(tripsplaneairports, old = c("city", "state", "country"), new= c("Destcity", "Deststate", "Destcountry"))

# Again some feature Engineering
tripsplaneairports$planeAge <-  tripsplaneairports$Year  - as.numeric(as.character(tripsplaneairports$year))  
str(tripsplaneairports$planeAge)

################ Exploratory Data Analysis ###########################################
# Let's do EDA and find out significance of the variables in predicting Flight Delays ###########
# For Continous variables, we can use co-relation to see how y is co-related to x1, x2 etc.. which is a good starting point to include in the model.
# For Categorical variables, let's find if there are patterns.

######################################
# Which Carrier performs better!
######################################
# This is bit generic question. Based on Case Study context "Airlines on-time performance". 
# We can make an assumption that the ask is in terms of flight delays...

str(trips)
CarrierDelay <- trips %>% 
                  group_by(UniqueCarrier) %>%
                  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanNASDelay=mean(NASDelay)) %>%
                  filter ( count > 1000, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
                  collect

#plot
ggplot(CarrierDelay, aes(UniqueCarrier, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) +  scale_size_area(max_size = 2)
ggplot(CarrierDelay, aes(UniqueCarrier, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) +  scale_size_area(max_size = 2)

# Same stuff with Bar chart.
ggplot(CarrierDelay, aes(reorder(UniqueCarrier, MeanDepDelay), MeanDepDelay)) + geom_bar( stat = "identity")  
ggplot(CarrierDelay, aes(reorder(UniqueCarrier, MeanArrDelay), MeanArrDelay)) + geom_bar( stat = "identity")  

### We can see that Carrier = WN has managed beter on-time perfornance with Arrival (avg ~14min delay)
#better even with large number of trips(), while DL has done better with Departure ( avg ~ 14min delay)

# Again, Carrier performance differs between Airport to Airport. 
# We can easily apply filter on Airport and get best parforming of Carrier.

#### Origin Airport
OriginAirportDelay <- trips %>% 
  group_by(Origin) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay=mean(WeatherDelay)) %>%
  filter ( count > 2000, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect
# Increasing count to 2000 for filtering in order to avoid too much cluttering.

#plot
ggplot(OriginAirportDelay, aes(Origin, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(OriginAirportDelay, aes(Origin, MeanWeatherDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# Lax and SLS are doing better for Departure. Weather is also not much impacted at LAX compared to others.
# However, except these airports, there is no perticular pattern we can observe. May be we need to combine other variables.

##### Dest Airport
DestAirportDelay <- trips %>% 
  group_by(Dest) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 500, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect
# 
#plot
ggplot(DestAirportDelay, aes(Dest, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DestAirportDelay, aes(Dest, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DestAirportDelay, aes(count, MeanArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
# SLC, PHX are doing very good with Arrival. And ORD, LGA, JFK, ESR are not so great. 

##########################
# Q-2: When is the best time of the day/week/month/year to travel to minimize delays. 
##########################
#Day/Time/Month
DayMonthDelay <- trips %>% 
  group_by(Month) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

#plot
ggplot(DayMonthDelay, aes(as.factor(Month), MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayMonthDelay, aes(Month, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayMonthDelay, aes(Month, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# We can clearly see Sept, Oct, Nov are the best months to fly as the delays are very min.

DayofMonthDelay <- trips %>%
  group_by(Month,DayofMonth) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(DayofMonthDelay, aes(DayofMonth, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayofMonthDelay, aes(DayofMonth, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayofMonthDelay, aes(DayofMonth, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# There is no patteren what so ever. No point in usng this as variable. However, we can try deviding the same by 7 and get the week info to see if there is a patteren.

WeekOfMonthDelay <- trips %>%
  group_by(Month,WeekOfMonth) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect


ggplot(WeekOfMonthDelay, aes(WeekOfMonth, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(WeekOfMonthDelay, aes(WeekOfMonth, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(WeekOfMonthDelay, aes(WeekOfMonth, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# You can see overall 2nd week is better in a month and 5th week(i.e month end - 29th, 30th 31st) is better time to fly.

TimeArrDelay <- trips %>%
  group_by(ArrHour,ArrTimeofday) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(TimeArrDelay, aes(ArrTimeofday, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(TimeArrDelay, aes(ArrTimeofday, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# Plan your flight such a way, your sheduled arrival is morning. i.e better overall.

TimeDepDelay <- trips %>%
  group_by(DepTimeofday) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(TimeDepDelay, aes(DepTimeofday, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(TimeDepDelay, aes(DepTimeofday, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
## Generally Departure delays keeps increasing as day prgress. So early morning flights are better.

HourDepDelay <- trips %>%
  group_by(DepHour) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(HourDepDelay, aes(DepHour, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(HourDepDelay, aes(DepHour, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
## Clearly 5-10am departures are better. and almost very negligible delays of less then ~10min. 

HourArrDelay <- trips %>%
  group_by(ArrHour) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(HourArrDelay, aes(ArrHour, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(HourArrDelay, aes(DepHour, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
## Clearly 5-10am departures are better. and almost very negligible delays of less then ~10min. 


DayOfWeekDelay <- trips %>%
  group_by(DayOfWeek) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(DayOfWeekDelay, aes(DayOfWeek, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayOfWeekDelay, aes(DayOfWeek, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DayOfWeekDelay, aes(DayOfWeek, MeanWeatherDelay2)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
## Flight on Saturday seems to be better. Tuesday and Wednesday are ok too. Avoid Friday, Mon and Sun.
## Same applicable even on Arrival. 

#### So to answer the question
#### Best Month: Sept, Oct, Nov with avg delay of ~10 min delay both for Departure and Arrival. 
#### Best Week: 2nd week of the month is best. Month ends are ok two. 
#### Best Day: Saturday is the best day to travel(Both arrival and departure). Tuesday and Wednesday's are ok too. Avoid Thursday (Avg Delays: 30min) 
#### Best Time of the day: 5am to 10 am is best for both Departure and Arrival: ~ 10 min delay.
#### Note, this can differ from airport to airport and carrier to carrier. This is in-general with out specifically considering any of the conditions.
#### If, we want to find based on a condition then we can do the same, by using additonal condition. 

#########################################################
# Do older planes suffer more delays ?
#########################################################

planeAgeDelay <- tripsplaneairports %>%
  filter(planeAge < 1500) %>%
  group_by(planeAge) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(planeAgeDelay, aes(planeAge, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(planeAgeDelay, aes(planeAge, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
#Flights less then fiver years tend to be having less delays: ~avg 17min and 5-25 years: ~22min
#And more then 25years ones tend to be havin less delays as well. 
#However, planes which are more then 25years ones not much operational.
# So, yes, older flights has more delays, but very old ones(>25 yesrs) seems to be having less delays though they are not much in use.

AircraftEngineDelay <- tripsplaneairports %>%
  group_by(aircraft_type, engine_type, type) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 50, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect

ggplot(AircraftEngineDelay, aes(aircraft_type, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(AircraftEngineDelay, aes(aircraft_type, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)

ggplot(AircraftEngineDelay, aes(engine_type, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(AircraftEngineDelay, aes(engine_type, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)

ggplot(AircraftEngineDelay, aes(type, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(AircraftEngineDelay, aes(type, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
# No strong relationships encountered. 

# Now let's look at Airports! This may have importance.

str(tripsplaneairports)

OriginAirportDelay <- tripsplaneairports %>%
  group_by(Origin, Origincity, Originstate, Origincountry) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 1000, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect
# Here increased the count, to study cities/states where major no of trips occoured.

ggplot(OriginAirportDelay, aes(Origincountry, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(OriginAirportDelay, aes(Originstate, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(OriginAirportDelay, aes(Origincity, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(OriginAirportDelay, aes(Origin, MeanDepDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
#State seems to have relationship, not country. California seems to be best state and LOSAngles seems to be best city to fly from.
# Fligts from UT, CA and TN states are better in terms of departure.

str(tripsplaneairports)

DestAirportDelay <- tripsplaneairports %>%
  group_by(Dest, Destcity, Deststate, Destcountry) %>%
  summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), MeanWeatherDelay2=mean(WeatherDelay)) %>%
  filter ( count > 1000, !is.na(MeanArrDelay), !is.na(MeanDepDelay) ) %>%
  collect
# Here increased the count, to study cities/states where major no of trips occoured.

ggplot(DestAirportDelay, aes(Destcountry, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DestAirportDelay, aes(Deststate, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DestAirportDelay, aes(Destcity, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
ggplot(DestAirportDelay, aes(Dest, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)
#State seems to have relationship, not country. CA = mean ~15min and AZ seems to be best state with ~13min delay and LOSAngles seems to be best city to fly to as well and even Phonix as well.

####################### Now let's explore continous variables #################################
# Let's plot continous variables and see how they are co-related
# Now based on above Exploratory Data Analysis, we need to choose the variables that are potentially significant and then start the modeling.

continousdelay <- select(tripsplaneairports, ActualElapsedTime, AirTime, ArrDelay, DepDelay, Distance, 
                         TaxiIn, TaxiOut, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay )


tripscorr <- cor(continousdelay, method="pearson")
# Let's plot the co-relation matix
corrplot(tripscorr, method="color")
# This clearly provides co-relation between the variables.

# Also let's plot few graphs as well.
ggplot(tripsplaneairports, aes(Distance, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(TaxiIn, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(TaxiOut, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(CarrierDelay,ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(WeatherDelay, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(NASDelay, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(SecurityDelay, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(LateAircraftDelay, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)

ggplot(tripsplaneairports, aes(Distance, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(TaxiIn, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(TaxiOut, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(CarrierDelay,DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(WeatherDelay, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(NASDelay, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(SecurityDelay, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(LateAircraftDelay, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)


#Based on this plot, we can see ArrDelay has positive co-relation with DepDelay, TaxiOut, CarrierDelay, NASDelay, WeatherDelay, LateAirCraftDelay
#And DepDelay has positive co-relation wth TaxiOut, CarrierDelay, NASDelay, WeatherDelay, LateAirCraftDelay
#So, from continous variable point of view, we got the starting point.
# We can also see that the significant variables are not co-related to each other. i.e there is no multico-linearity.
# If the multi-colinirity is present, then we should have addressed. i.e combining variables or picking only most significant one and leaving other.
# PCA is one of the method to address the same.
##########################################################################################################
########################################## 
# Now we have fair idea, now let us see variable significane and based on the results  we can choose the best variable combination.
# We can also statistically, prove what we saw visually befor choosing the final machine learning model.
modeltrips <- na.omit(tripsplaneairports[,c("Dest","Origin", "Month", "DayOfWeek",  "ArrTime", "UniqueCarrier", "ActualElapsedTime", "CRSElapsedTime", 
                                    "TaxiOut", "Diverted",  "CarrierDelay", "WeatherDelay",    "NASDelay",        "SecurityDelay",   "LateAircraftDelay" ,"ArrHour",        
                                    "DepHour",         "ArrTimeofday",    "DepTimeofday",       "WeekOfMonth",    "Origincity",      
                                    "Originstate",   "Destcity", "Deststate" ,  "ArrDelay", "DepDelay" )])

################################################3 See each variable, how significant to the DepDelay and ArrDelay ###############
lm.fit1 <- stats::lm(formula = DepDelay ~ factor(Month), data = modeltrips)
summary(lm.fit1)
# Month seems to be significant as evident in EDA and here with p value less than 0.0001.

lm.fit2 <-  stats::lm(formula = DepDelay ~ factor(DepTimeofday) + factor(DepHour), data = modeltrips)
summary(lm.fit2)
# Both seems to be signifiant

lm.fit3 <-  stats::lm(formula = DepDelay ~ factor(DepTimeofday) , data = modeltrips)
summary(lm.fit3)
# This seems to be significant

lm.fit4 <-  stats::lm(formula = DepDelay ~ factor(DepHour) , data = modeltrips)
summary(lm.fit4)
# Dep hour seems to be more significant

lm.fit5 <-  stats::lm(formula = DepDelay ~ factor(DayOfWeek) + factor(WeekOfMonth), data = modeltrips)
summary(lm.fit5)
# The variance it is explaining is very small.

#Now let's combine by adding these together and check significance.
lm.fit.com.1 <- stats::lm(formula = DepDelay ~ factor(Month) + factor(WeekOfMonth)  , data = modeltrips)
summary(lm.fit.com.1)
# Increase in R-Squared is not visible ~ 0.1%

lm.fit.com.2 <- stats::lm(formula = DepDelay ~ factor(Month) + factor(WeekOfMonth) + factor(DayOfWeek) , data = modeltrips)
summary(lm.fit.com.2)

lm.fit.com.3 <- stats::lm(formula = DepDelay ~  factor(Month) + factor(DepHour) , data = modeltrips) 
summary(lm.fit.com.3)
# Out of all above variables, Month adn DepHour seems to be predicting better.
lm.fit.com.3 <- stats::lm(formula = DepDelay ~  factor(Month) +  factor(ArrHour) , data = modeltrips) 
summary(lm.fit.com.3)
# Very different insight. Scheduled ArrHour is better predictor to DepDelay then compared with Secheduled DepHour. 
# The difference is not huge though.

########## OTher variables
lm.fit6 <-  stats::lm(formula = DepDelay ~ factor(UniqueCarrier),  data = modeltrips)
summary(lm.fit6)
# Through p-value they seem to be significant, not when we look for R-squared. Let's proceed further.


lm.fit7 <-  stats::lm(formula = DepDelay ~ factor(Originstate) ,  data = modeltrips)
summary(lm.fit7)
# Seems to have some significance
lm.fit7a <-  stats::lm(formula = DepDelay ~  factor(Deststate),  data = modeltrips)
summary(lm.fit7a)
# Seems to have some significance
lm.fit7c <-  stats::lm(formula = DepDelay ~ factor(Originstate) + factor(Deststate),  data = modeltrips)
summary(lm.fit7c)
# Combining them is better.


lm.fit8 <-  stats::lm(formula = DepDelay ~ factor(Origin),  data = modeltrips)
summary(lm.fit8)
# Seems to be significant.
lm.fit8a <-  stats::lm(formula = DepDelay ~ factor(Dest),  data = modeltrips)
summary(lm.fit8a)
# It is significant.

lm.fit8b <-  stats::lm(formula = DepDelay ~ factor(Dest) + factor(Origin),  data = modeltrips)
summary(lm.fit8b)
# Does not work due to memory issues.

lm.fit9 <-  stats::lm(formula = DepDelay ~ factor(Origincity),  data = modeltrips)
summary(lm.fit9)
# City also seems to be having some significance.
# Lets check Dest city and see if any combination is better.
lm.fit9a <-  stats::lm(formula = DepDelay ~ factor(Destcity),  data = modeltrips)
summary(lm.fit9a)
# This also seems to have significance.
lm.fit9b <-  stats::lm(formula = DepDelay ~ factor(Origincity) + factor(Destcity),  data = modeltrips)
summary(lm.fit9b)
# Both together could explain better variance then with one.

lm.fit10 <-  stats::lm(formula = DepDelay ~ Diverted,  data = modeltrips)
summary(lm.fit10)
# No significance

####
summary(aov1)


#Create dummy variables for all catagorical variables which are significand from the graphs.
tripsplaneairports <- na.omit(tripsplaneairports)

tripsdummy <- model.matrix(~ as.factor(Month)+as.factor(DayOfWeek)+UniqueCarrier
                           +as.factor(DepHour)+as.factor(DepTimeofday) + as.factor(WeekOfMonth) 
                           +as.factor(ArrHour) + as.factor(ArrTimeofday) + 
                            as.factor(Deststate) + as.factor(Originstate) ,data=tripsplaneairports) 

tripsdummy <- tripsdummy[,-1] 

head(tripsdummy,5)
str(tripsdummy)
nrow(tripsdummy)

# Based on the co-relations above the significant columns are choosen.
tripscontinous <- tripsplaneairports[, c( "TaxiOut", "CarrierDelay", "WeatherDelay", "NASDelay", "LateAircraftDelay" )]

str(tripscontinous)

# Scaling all continous independent variables excluding Dependent variables.
tripscontinous <- as.data.frame(scale (tripscontinous))
str(tripscontinous)
head(tripscontinous)

tripscontinous$ArrDelay <- tripsplaneairports$ArrDelay
tripscontinous$DepDelay <- tripsplaneairports$DepDelay
tripscontinous <- as.data.frame(tripscontinous)

nrow(tripscontinous)
tripsnew <- cbind(tripsdummy, tripscontinous )
tripsnew <- as.data.frame(tripsnew)

str(tripsnew)
head(tripsnew)

# Removing the dummy
rm(tripsdummy)
###########################
#Divide data into training, test and validation data set.

# 3.2 Split your dataset

set.seed(110)
sub <- sample(nrow(tripsnew), floor(nrow(tripsnew) * 0.5))

traintrips <- tripsnew[sub,]
remaining <- tripsnew[-sub,]

#Remaining data is agian devided into test and validation
sub1 <- sample(nrow(remaining), floor(nrow(remaining) * 0.6))

testtrips <- remaining[sub1,]
validtrips <- remaining[-sub1,]

rm(remaining)

#Now lets build actual model by combining the significant variables
###################################################
#Now time to build model on the Training Data
#### Let's do first for DepDelay prediction

traintripsdep <- traintrips[, !grepl("ArrDelay" , names(traintrips))]  # Removing Arrvilal related variables.

str(traintripsdep)
ncol(traintripsdep)

lm.dep.fit1 <- stats::lm(DepDelay ~ . , data=traintripsdep)
summary(lm.dep.fit1)

# At first look. R Squared values look prety good. i.e 94%. Also Resudal error: 10.35 is not bad.
# p-value is also good.
# Let's see the training error and validation error!

# Predict values on training set
traintripsdep$pred.DepDelay <- predict(lm.dep.fit1)
traintripsdep$error <- residuals(lm.dep.fit1)


#  Predict values on validation set
validtrips$predict.DepDelay <- predict(lm.dep.fit1,
                                       newdata=validtrips)
validtrips$error <- 
  validtrips$predict.DepDelay - validtrips$DepDelay

# Check residual plots
hist(traintripsdep$error)
hist(validtrips$error)
# In both cases the histogram is centered at zero. Espicially with Test data. This is a good test.
# Follows nornal distribution which is a good measure of model performance.

# Correlation
a<-cor(traintripsdep$DepDelay,
       traintripsdep$pred.DepDelay )
b<-cor(validtrips$DepDelay,
       validtrips$predict.DepDelay )
a
b

a*a
b*b
# co-relation between actul and predicted is very high satisfying the accuracy of the model.
plot(traintripsdep$DepDelay, 
     traintripsdep$pred.DepDelay )

plot(validtrips$DepDelay, 
     validtrips$predict.DepDelay )
# If we look at the Actual vs Predicted it almost follows stright line over diaognal.

plot(traintripsdep$error, 
     traintripsdep$pred.DepDelay )

plot(validtrips$error, 
     validtrips$predict.DepDelay )
# Follows normal distributuon.
# Model performance is good and accuracy is good as well.

qplot(y=traintripsdep$error)
qplot(y=validtrips$error)
# There are very few outliers which is good as well. 
#########################################################
#### Though we have tested each and every independent variable and max combinations before choosing final model. 
#We can use alternate methods to choose variables. One of such methods is AIC. 
#This takes combinations of variables and computes statistics.
### StepAIC to find alternate models
lm.dep.step1  <- stepAIC(lm.dep.fit1)
summary(lm.dep.step1)

lm.dep.step1$coefficients
lm.dep.step1$residuals
lm.dep.step1$rank
lm.dep.step1$fitted.values

lm.dep.step1$

importance(lm.dep.fit1)

lm.dep.step1$anova
#### Based on STEPAIC, Below variables is significant to the model. 
#### Let's build one more time and find out the results
lm.dep.fit2 <-  stats::lm(formula = DepDelay ~ `as.factor(Month)3` + `as.factor(Month)6` + 
      `as.factor(Month)7` + `as.factor(Month)8` + `as.factor(Month)9` + 
      `as.factor(Month)11` + `as.factor(Month)12` + `as.factor(DayOfWeek)2` + 
      `as.factor(DayOfWeek)3` + `as.factor(DayOfWeek)4` + `as.factor(DayOfWeek)7` + 
      UniqueCarrierAA + UniqueCarrierAQ + UniqueCarrierAS + UniqueCarrierB6 + 
      UniqueCarrierCO + UniqueCarrierDL + UniqueCarrierEV + UniqueCarrierF9 + 
      UniqueCarrierFL + UniqueCarrierHA + UniqueCarrierMQ + UniqueCarrierNW + 
      UniqueCarrierOH + UniqueCarrierOO + UniqueCarrierUS + UniqueCarrierWN + 
      UniqueCarrierYV + `as.factor(DepHour)5` + `as.factor(DepHour)6` + 
      `as.factor(DepHour)7` + `as.factor(DepHour)9` + `as.factor(DepHour)10` + 
      `as.factor(DepHour)11` + `as.factor(DepHour)12` + `as.factor(DepHour)13` + 
      `as.factor(DepHour)14` + `as.factor(DepHour)15` + `as.factor(DepHour)16` + 
      `as.factor(DepHour)17` + `as.factor(DepHour)18` + `as.factor(DepHour)19` + 
      `as.factor(DepHour)20` + `as.factor(DepHour)21` + `as.factor(DepHour)22` + 
      `as.factor(DepHour)23` + `as.factor(WeekOfMonth)3` + `as.factor(WeekOfMonth)4` + 
      TaxiOut + CarrierDelay + WeatherDelay + NASDelay + LateAircraftDelay, 
    data = traintripsdep)

summary(lm.dep.fit2)
# 445498
#Let's see the results

# Predict values on training set
traintripsdep$pred.DepDelay <- predict(lm.dep.fit2)
traintripsdep$error <- residuals(lm.dep.fit2)

#  Predict values on validation set
validtrips$predict.DepDelay <- predict(lm.dep.fit2,
                                       newdata=validtrips)
validtrips$error <- 
  validtrips$predict.DepDelay - validtrips$DepDelay

# Check residual plots
hist(traintripsdep$error)
hist(validtrips$error)
# This is an important graph. We still see room for improvement.

# Correlation
a<-cor(traintripsdep$DepDelay,
       traintripsdep$pred.DepDelay )
b<-cor(validtrips$DepDelay,
       validtrips$predict.DepDelay )
a
b

a*a
b*b

plot(traintripsdep$DepDelay, 
     traintripsdep$pred.DepDelay )

plot(validtrips$DepDelay, 
     validtrips$predict.DepDelay )

plot(traintripsdep$error, 
     traintripsdep$pred.DepDelay )

plot(validtrips$error, 
     validtrips$predict.DepDelay )

qplot(y=traintripsdep$error)
head(validtrips[,c("error", "predict.DepDelay", "DepDelay")],20)
######## Finally we can see that out first model is better. 
###Even if we get better model by tweeking the variables. It may not do well on completely new data.
#Let's verify both the models on test data set and conclude.


####### Now let us assume that we don't have the information about the delays due to Weather, NAS, 
####### Carrier, LateAircraft. Weather info is only available say 7-15 days prior. 
####### And other Delays are more or less not available before hand.  

####### So, when we want to predict DepDelay for a future flight. 
###### We may need to assume some values for these. 
#i.e query available historical data and pass eaither mean or median for these variables for a given condition ( airport, carrier, month, day, time )

#### With out above variables, I tried to create model to predict. See how bad the results of prediction.
summary(lm.dep.fit4)
# R Squared value is too low for you to predict.
# So, we have to provide these variables in order to get accurate prediction. 
# One method could be! having a summary table, which has mean and median value of these numbers which can be readily used.
# i.e For a given month, week, day, time of day in the specific airport/city for a given carrier. Have the mean and median numbers. 
# Use one of them!

tripssummary <- tripsplaneairports %>% group_by(Month, WeekOfMonth, DayOfWeek, DepTimeofday, UniqueCarrier, Origin, Origincity, Originstate, Dest, Destcity, Deststate, ) %>%
summarise(count=n(), MeanArrDelay = mean(ArrDelay), MeanDepDelay=mean(DepDelay), 
                       MeanWeatherDelay=mean(WeatherDelay), MeanCarrierDelay=mean(CarrierDelay),
                       MeanWeatherDelay=mean(LateAircraftDelay), MeanCarrierDelay=mean(CarrierDelay),                       
                       MeanTaxiIn=mean(TaxiIn), MeanTaxiOut=mean(TaxiOut),
                       MeanplaneAge=mean(AirTime), MeanplaneAge=mean(planeAge),
                       MeanNASDelay=mean(NASDelay), MeanSecurityDelay=mean(SecurityDelay))%>%
    collect

nrow(tripssummary)
nrow(tripsplaneairports)
#############################################################################################
#Now it's time to predict ArrDelay as well. We can use DespDelay as well for predicting Arrival Delay.

traintripsArr <- traintrips
#traintripsArr <- traintrips[, !grepl("DepDelay" , names(traintrips))]  # Removing Arrvilal related variables.

str(traintripsdep)
ncol(traintripsdep)

lm.arr.fit1 <- stats::lm(ArrDelay ~ . , data=traintripsArr)
summary(lm.arr.fit1)

#R-Squared: 97.2 and Residual error: 7.505
# Predict values on training set
# Predict values on training set
traintripsArr$pred.ArrDelay <- predict(lm.arr.fit1)
traintripsArr$ArrError <- residuals(lm.arr.fit1)


#  Predict values on validation set
validtrips$predict.ArrDelay <- predict(lm.arr.fit1,
                                       newdata=validtrips)
validtrips$ArrError <- 
  validtrips$predict.ArrDelay - validtrips$ArrDelay

# Check residual plots
hist(traintripsArr$ArrError)
hist(validtrips$ArrError)
# In both cases the histogram is centered at zero. Espicially with Test data. This is a good test.
# Follows nornal distribution which is a good measure of model performance.

# Correlation
a<-cor(traintripsArr$ArrDelay,
       traintripsArr$pred.ArrDelay )
b<-cor(validtrips$ArrDelay,
       validtrips$predict.ArrDelay )
a
b

a*a
b*b
# co-relation between actul and predicted is very high satisfying the accuracy of the model.
plot(traintripsArr$ArrDelay, 
     traintripsArr$pred.ArrDelay )

plot(validtrips$ArrDelay, 
     validtrips$predict.ArrDelay )
# If we look at the Actual vs Predicted it almost follows stright line over diaognal.

plot(traintripsArr$ArrError, 
     traintripsArr$pred.ArrDelay )

plot(validtrips$ArrError, 
     validtrips$predict.ArrDelay )
# Follows normal distributuon.
# Model performance is good and accuracy is good as well.

qplot(y=traintripsArr$ArrError)
qplot(y=validtrips$ArrError)
################################################################################
# We can try removing DepDelay (independent variable) and try and predict the ArrDelay.

traintripsArr <- traintrips[, !grepl("DepDelay" , names(traintrips))]  # Removing Arrvilal related variables.

str(traintripsdep)
ncol(traintripsdep)

lm.arr.fit2 <- stats::lm(ArrDelay ~ . , data=traintripsArr)
summary(lm.arr.fit2)

#R-Squared: 96.8 and Residual error: 8. Even this seems to be good. 
# It may predict well on Validation data set.
# Predict values on training set

traintripsArr$pred.ArrDelay <- predict(lm.arr.fit2)
traintripsArr$ArrError <- residuals(lm.arr.fit2)


#  Predict values on validation set
validtrips$predict.ArrDelay <- predict(lm.arr.fit2,
                                       newdata=validtrips)
validtrips$ArrError <- 
  validtrips$predict.ArrDelay - validtrips$ArrDelay

# Check residual plots
hist(traintripsArr$ArrError)
hist(validtrips$ArrError)
# In both cases the histogram is centered at zero. Espicially with Test data. This is a good test.
# Follows nornal distribution which is a good measure of model performance.

# Correlation
a<-cor(traintripsArr$ArrDelay,
       traintripsArr$pred.ArrDelay )
b<-cor(validtrips$ArrDelay,
       validtrips$predict.ArrDelay )
a
b

a*a
b*b
# co-relation between actul and predicted is very high satisfying the accuracy of the model.
plot(traintripsArr$ArrDelay, 
     traintripsArr$pred.ArrDelay )

plot(validtrips$ArrDelay, 
     validtrips$predict.ArrDelay )
# If we look at the Actual vs Predicted it almost follows stright line over diaognal.

plot(traintripsArr$ArrError, 
     traintripsArr$pred.ArrDelay )

plot(validtrips$ArrError, 
     validtrips$predict.ArrDelay )
# Follows normal distributuon.
# Model performance is good and accuracy is good as well.

qplot(y=traintripsArr$ArrError)
qplot(y=validtrips$ArrError)
################################################################################
# Finally, we need to apply this model to test data set to see the results.
# And once this is ready, we need to use the final model in function which accepts these input variables as parameters and predicts the delay.


##### For Departure.


#  Predict values on validation set
testtrips$predict.DepDelay <- predict(lm.dep.fit1,
                                       newdata=testtrips)
testtrips$DepError <- 
  testtrips$predict.DepDelay - testtrips$DepDelay


hist(traintrips$DepError)
hist(testtrips$DepError)
# In both cases the histogram is centered at zero. Espicially with Test data. This is a good test.
# Follows nornal distribution which is a good measure of model performance.

# Correlation
a<-cor(traintrips$DepDelay,
       traintrips$pred.DepDelay )
b<-cor(testtrips$DepDelay,
       testtrips$predict.DepDelay )
       a
       b
       
       a*a
       b*b
#### The accuracy seems to be better then validation data set for departure.


##### For Arrival. 
#  Predict values on validation set
testtrips$predict.ArrDelay <- predict(lm.arr.fit2,
                                       newdata=testtrips)
testtrips$ArrError <- 
  testtrips$predict.ArrDelay - testtrips$ArrDelay


hist(traintrips$ArrError)
hist(testtrips$ArrError)
# In both cases the histogram is centered at zero. Espicially with Test data. This is a good test.
# Follows nornal distribution which is a good measure of model performance.

# Correlation
a<-cor(traintripsArr$ArrDelay,
       traintripsArr$pred.ArrDelay )
b<-cor(testtrips$ArrDelay,
       testtrips$predict.ArrDelay )
a
b

a*a
b*b

# Performance is good for test data set as well.
###################################################################################################
###################################################################################################

####################### Now let us answer other questions ##############################3
#############################################
#Can you detect cascading failures as delays in one airport create delays in others? 
#Are there critical links in the system?
#############################################
# To detect cascading failures, have devided 24 hours into 4 buckets. i.e new variables DepTimeOfDay, ArrTimeOfDay
# 12am to 5:59 am as 0 (Night)
# 6am to 11:59 am as 1 (Morning) 
# 12noon to 5:59pm (Afternoon)
# 6pm to 11:59pm (Evening)
# This is to check if there is delay in previous bucket, will it have impact in next bucket in the Airport context.
# We could do it hourly, but for convinence have choosen this. We can try hourly as well.
#
# Now let's take e.g: Let's Say. 
# Delays (Both ArrDelay and DepDelay) in Origin at a given time can cause Delays in later time at Dependent Dest.
# Let's test this.
# Let's see what is Mean Delay(Arr, Dep) at Origin over DepTimeOfDay. 
# 
tripsOrigin <- select (trips, Origin, Dest,  DepDelay, ArrDelay, DepTimeofday, ArrTimeofday ) 
tripsDest <- select (trips, Origin, Dest, DepDelay, ArrDelay, DepTimeofday, ArrTimeofday )


tripsOriginFD <- tripsOrigin %>% 
  group_by(Origin, Dest, DepTimeofday , ArrTimeofday ) %>%
  summarise(md=mean(DepDelay), ma = mean(ArrDelay)) 

AirDestFD <- tripsOrigin %>% 
  group_by( Dest, ArrTimeofday  ) %>%
  summarise(md1=mean(DepDelay), ma1 = mean(ArrDelay)) 

tripsOriginFD <- as.data.frame(tripsOriginFD)
AirDestFD <- as.data.frame(AirDestFD)

head(AirDestFD)
head(tripsOriginFD)

# Joining the tables... Matching "Dest" and ArrTime 
AirportDep <- merge(x=tripsOriginFD, y=AirDestFD, by.x = c("Dest", "ArrTimeofday"), by.y= c("Dest", "ArrTimeofday"))

head(AirportDep,20)

gplot(AirportDep, aes(md+ma, md1+ma1)) + geom_point() + geom_smooth()
ggplot(AirportDep, aes(md+ma, md1+ma1)) + geom_point(aes(size=DepTimeofday), alpha=1/2) + geom_smooth()
### This doesn't mean anything. 

##### We need to agreegate Based on Dest, ArrTime of day and find mean delay at Origin vs Dest
AirportDep1 <- AirportDep %>% group_by(Dest,ArrTimeofday) %>%
  summarize(mdo=mean(md), mao=mean(ma), mdd=mean(md1), mad=mean(ma1) )

head(AirportDep1,20)
ggplot(AirportDep1, aes(mdo+mao, mdd+mad)) + geom_point() + geom_smooth() + xlab("Mean Delay at Origin") + ylab("Mean Delay at Dest")
ggplot(AirportDep1, aes(mdo+mao, mdd+mad)) + geom_point(aes(size=ArrTimeofday), alpha=1/2) + geom_smooth() + xlab("Mean Delay at Origin") + ylab("Mean Delay at Dest")

ggplot(AirportDep1, aes(mdo, mdd)) + geom_point() + geom_smooth() + xlab("Mean Dep Delay at Origin") + ylab("Mean Dep Delay at Dest")
ggplot(AirportDep1, aes(mao, mad)) + geom_point(aes(size=ArrTimeofday), alpha=1/2) + geom_smooth() + xlab("Mean Arr Delay at Origin") + ylab("Mean Arr Delay at Dest")

####Clearly, there is leanier relationship between delays in one airport cascading to downstream airports.
#### Have ploted the same above. 
### Critical link is: For a given airport at a given TimeOfDay, match Dest Airport from the upstream(Ogigin) and Arrival times. 
### Infact, we can very well use this as a feature in predicting ArrTime and DepTime. Accuracy will significantly improve. I am not going there, given we have limited time now.

#################################
# Can Weather Predict Flight delayes ?
#################################
# Certainly, weather has influence on ArrDelay and DepDelay as we have seen in Exploratory Data Analysis.
# However, if we could download weather data, we could have further expanded on this. The site is not up for few days.
# For the same model, WeatherDelay parameter should be passed from what we get from site provided. 

# Reproducing same graphs again.
ggplot(tripsplaneairports, aes(WeatherDelay, DepDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)
ggplot(tripsplaneairports, aes(WeatherDelay, ArrDelay)) + geom_point() + geom_smooth() + scale_size_area(max_size = 2)

# Let's plot the co-relation matix one more time.
corrplot(tripscorr, method="color")

################################
# Running same analysis on BigData/Spark
################################
# Installing Spark
spark_install(version = "2.1.0")

###################### Connecting to Spark Local server #########################
sc <- spark_connect(master = "local")


#Loading data to Spark
trips <- as.data.frame(trips)
trips_tbl1 <- copy_to(sc, trips, "trips1" , overwrite = TRUE)


###############
# Loading Large data files
setwd("D:/Data Science/Flight Data Analysis/")
getwd()

#sc_trips2007tbl <- spark_read_csv(sc, "sc_trips2007", ".\\Data\\2007.csv", delimiter = "," , memory = TRUE, overwrite = TRUE)
#sc_trips2008tbl <- spark_read_csv(sc, "sc_trips2008", ".\\Data\\2008.csv", , delimiter = "," , memory = TRUE, overwrite = TRUE)
# First tried loading big large files. It's failing due to issues with my spark session.

# Traditional R route.
trips2007 <- fread(".\\Data\\2007.csv")  #Fastest way to load large files.
trips2008 <- fread(".\\Data\\2008.csv")

#Merge two tables.
###############################################
trips2007a <- sample_n(trips2007, 1000000)
trips2008b <- sample_n(trips2008, 1000000)
rm(trips2007,trips2008)

tripsboth <- rbind(trips2007a, trips2008b)

# copy to spark
str(tripsboth)
tripsboth <- as.data.frame(tripsboth)
trips_tbl2 <- copy_to(sc, tripsboth, "trips2" , overwrite = TRUE)

####
src_tbls(sc)
#######

trips$ArrHour <- trunc(trips$CRSArrTime/100) 
trips$DepHour <- trunc(trips$CRSDepTime/100)

# Deviding day into 4 time periods for analysis
trips$ArrTimeofday <- trunc(trips$ArrHour/6)
trips$DepTimeofday <- trunc(trips$DepHour/6)

# To see week of month has any significance. It may.
trips$WeekOfMonth <- trunc(trips$DayofMonth/7+1)

########################################################
######### Exploratory Data Analysis ####################
########################################################
####################################
tripsdelayCarrier <- trips_tbl2 %>% 
  group_by(UniqueCarrier) %>%
  summarise(count=n(), MeanArrDelay=mean(ArrDelay), MeanDepdelay=mean(DepDelay)) %>%
  filter (count > 20 ) %>%
  collect
#Plot the delay

ggplot(tripsdelayCarrier, aes(UniqueCarrier, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + scale_size_area(max_size = 2)


tripsdelayMonth <- trips_tbl2 %>% 
  group_by(Month) %>%
  summarise(count=n(), MeanArrDelay=mean(ArrDelay), MeanDepdelay=mean(DepDelay)) %>%
  filter (count > 20 ) %>%
  collect
#Plot the delay

ggplot(tripsdelayMonth, aes(Month, MeanArrDelay)) + geom_point(aes(size=count), alpha=1/2) + geom_smooth() + scale_size_area(max_size = 2)


####################################
####### Linear Regression using Spark ML ############
#####################################################
# Let's remove all the NAs and partition data  
model_partition <- trips_tbl2 %>% 
  filter( !is.na(DepDelay) & !is.na(WeatherDelay) &  !is.na(Month) ) %>%
  select(DepDelay , WeatherDelay ,  Month  ) %>%
  sdf_partition(tripstrain = 0.7, tripsvalid = 0.3, seed = 5555) 


# Fit a linear model
mlfit1 <- model_partition$tripstrain %>%
  ml_linear_regression(DepDelay ~ WeatherDelay + Month  )

# Summarize the linear model
summary(mlfit1)
# R-Squared: 74%, RMSE: 41.93. Not a great thing, how ever these variables are insignificant. 
########################
## Let's predict on both training and validation data set. 
model_pred <- lapply(model_partition, function(x) {
  sdf_predict(mlfit1, x) %>%
    collect()
})

# Since there are large number of points, before plotting let's summarize
sum1 <- rbind(
  data.frame(data = 'train', model_pred$tripstrain),
  data.frame(data = 'valid', model_pred$tripsvalid),
  make.row.names = FALSE
)

predsum <- sum1 %>%
           group_by(Month, data) %>%
           summarize( DepDelayMean = mean(DepDelay), PredDepDelayMean = mean(prediction) ) %>%
           collect()
           

ggplot(predsum, aes(DepDelayMean, PredDepDelayMean, color=data)) + geom_point() + geom_abline()

# From results it's clear that we don't necessirly have choosen all the features. 
# We can continue same steps we did on R on spark environment as well and surely we will have similar results.
# Leaving this here, as there are some issues with the my laptop infrastructure which is not allowing me to load these large files into Spark.
#############################################################
#############################################################