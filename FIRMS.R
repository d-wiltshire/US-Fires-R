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

# create subset of brightest/highest confidence fires
brightest_high_confidence <- subset(df, confidence>95 & brightness>400)

alaska <-subset(df, latitude>59 & longitude< -145)

summary(df)

# different color dot by month 

# Map fires associated with an active volcano 
mapview(
  volcano
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
  )

# Map most intense fires by FRP (Fire Radiative Power, in megawatts)
mapview(
  frp
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
  )

# Map observations most likely to be fires
mapview(
  high_confidence
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
  )

# Map brightest fire observations
mapview(
  brightest
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
  )


pal = mapviewPalette("mapviewTopoColors")
pal2 <-  mapviewPalette("mapviewSpectralColors")


# Map brightest + high confidence observations
# cex changes dot size based on a variable

library(leafpop) 

mapview(
  brightest_high_confidence
  , xcol = "longitude"
  , ycol = "latitude"
  , zcol = "brightness"
  , crs = 4269
  , cex = "brightness"
  , col.regions = pal(100), at = seq(500, 1000, 100)
  , grid = FALSE
  , popup = popupTable(brightest_high_confidence,
               zcol = c("brightness",
                        "frp"
                        )
                      )
  )


#limit to a specific lat/long area

# Map observations in Alaska
mapview(
  alaska
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
)

# view options
#https://r-spatial.github.io/mapview/articles/mapview_03-options.html


# add more descriptive data to tooltip 

# diff size/color dot based on a variable 
# https://stackoverflow.com/questions/56743265/point-color-and-symbol-size-based-on-different-variables-in-mapview

#diff size dot based on brightness




# condense dates with similar lat/long into one dot? one dot per lat/long bin per day? 
#brightest_by_day<- energy %>% group_by(datetime) %>% summarise(sum =sum(value)) )

#combine frp and brightness? by percentile, rather than hard number 

# it's likely many fires are reported multiple times; goal is to take brightest fire by day by lat/long bin

library(dplyr)

#perform binning with specific number of bins
df_latbreaks <- df %>% mutate(newlat_bin = cut(latitude, breaks=500))
df_latlongbreaks <- df_latbreaks %>% mutate(newlong_bin = cut(longitude, breaks=500))
df_latlongbreaks$geog_bin <- with(df_latlongbreaks, c("newlat_bin", "newlong_bin"))

#check on why not filtering
df_binned <- df_latlongbreaks %>% group_by(geog_bin, acq_date)
df_binned %>% filter(brightness == max(brightness) )

mapview(
  df_binned
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
)

