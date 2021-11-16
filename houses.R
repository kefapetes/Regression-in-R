library(tidyverse)
library(ggplot2)
library(Hmisc)
library(GGally)
library(xlsx)
library(ggmap)

#Importing data
oridata <- read.csv("real_estate.csv")

#Copying oridata for editing
df <- oridata

#peaking at the first 6 rows of the data
head(df)

#Did not work I suspect it needs a package
describe(df)

#checking on the general structure of the data
str(df)

#checking missing values
table(is.na(df))

####Data cleaning
#Removing the No column
df <- df[,-c(1)]
#Renaming the head columns
colnames(df) <- c("TransactionDate","HouseAge","DistToNearestMRTstation",
                  "NumberOfConviStores","Latitude","Longitude","HousePrice")

#Deal with longitudes and latitudes possibly with library
#converting longitudes and latitudes into continuous data to obtain general
  #location of houses and also eliminate possibility of error
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)#Turns out they are on the same locale

###Constructing box plots
boxplot(df)#they are squeezed plotting for individual variables
boxplot(df$HouseAge)#No outliers observed
boxplot(df$DistToNearestMRTstation)#Multiple outliers observed though I don't think 
                                  #it's wise to be rid of them
#Removing outliers in DistToNearestMRTstation: 
      #Outliers were completely eliminated after the 6th iteration
df <- df[-which(df$DistToNearestMRTstation %in% boxplot.stats
                (df$DistToNearestMRTstation)$out),]

boxplot(df$NumberOfConviStores)
boxplot(df$HousePrice)#Outliers observed
#Removing outliers on HousePrice variable: Eliminated on the first iteration
df <- df[-which(df$HousePrice %in% boxplot.stats(df$HousePrice)$out),]

#Latitudes and transaction dates don't need a boxplot,
                      #not sure of transaction date significance to the data.


###Histogram
hist(df$HousePrice)#Normal distribution
hist(df$HouseAge)
hist(df$DistToNearestMRTstation)#Normal distribution
hist(df$NumberOfConviStores)

###Creating a scatter plot matrix
pairs(df, upper.panel = NULL)#Cant see well, plotting individual scatters

#Scatter plot of HouseAge vs HousePrice
plot(df$HouseAge, df$HousePrice)
#It appears to be no linear relationship between the variables

#Scatter plot of DistToNearestConviStores vs HousePrice
plot(df$DistToNearestMRTstation, df$HousePrice)
#There appears to be no linear relationship between the variables

#Scatter plot on NumberOfConviStores vs HousePrice
plot(df$NumberOfConviStores, df$HousePrice)
#There appears to be no linear relationship between the variables

#Trying to figure out the exact locations on the data: will use ggmap

#linear models