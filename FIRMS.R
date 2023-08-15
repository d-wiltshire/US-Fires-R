df<-read.csv("modis_2022_all_countries\\modis\\2022\\Countries\\modis_2022_United_States.csv")

head(df)

library(tidyverse)
library(sf)
library(mapview)

# create subset of df of only active volcano fires
volcano <- subset(df, type==1)

# create subset of df of most powerful fires
frp <- subset(df, frp>700)

# create subset of data most likely to be fires
high_confidence <- subset(df, confidence>95)

# create subset of brightest fires
brightest <- subset(df, brightness>500)

# Map fires associated with an active volcano 
mapview(volcano, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

# Map most intense fires by FRP (Fire Radiative Power, in megawatts)
mapview(frp, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

# Map observations most likely to be fires
mapview(high_confidence, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

# Map brightest fire observations
mapview(brightest, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

# condense dates with similar lat/long into one dot? one dot per lat/long bin per day? 

#combine frp and brightness? by percentile, rather than hard number 
