#Set working directory to source file location

df_us<-read.csv("modis_2022_all_countries\\modis\\2022\\Countries\\modis_2022_United_States.csv")

df_canada<-read.csv("modis_2022_all_countries\\modis\\2022\\Countries\\modis_2022_Canada.csv")

#Combine US and Canada datasets
df <- rbind(df_us, df_canada)

head(df)

library(tidyverse)
library(sf)
library(mapview)
library(leafpop) 
library(lubridate)

# Create subset of df of only active volcano fires
volcano <- subset(df, type==1)

# Create subset of df of most powerful fires
frp <- subset(df, frp>700)

# Create subset of data most likely to be fires
high_confidence <- subset(df, confidence>95)

# Create subset of brightest fires
brightest <- subset(df, brightness>500)

# Create subset of brightest/highest confidence fires
brightest_high_confidence <- subset(df, confidence>95 & brightness>400)

# Create subset with Alaska fires
alaska <-subset(df, latitude>59 & longitude< -145)


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

# Establish variables for mapview color palettes
pal = mapviewPalette("mapviewTopoColors")
pal2 = mapviewPalette("mapviewSpectralColors")


# Map brightest + high confidence observations
# Note: cex changes dot size based on a variable

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






# condense dates with similar lat/long into one dot? one dot per lat/long bin per day? 
#brightest_by_day<- energy %>% group_by(datetime) %>% summarise(sum =sum(value)) )

#combine frp and brightness? by percentile, rather than hard number 

# it's likely many fires are reported multiple times; goal is to take brightest fire by day by lat/long bin


# ----------------------------------- 
# Binning by geographical location


#perform binning with specific number of bins
df_latbreaks <- df %>% mutate(newlat_bin = cut(latitude, breaks=500))
df_latlongbreaks <- df_latbreaks %>% mutate(newlong_bin = cut(longitude, breaks=500))
df_latlongbreaks$geog_bin <- with(df_latlongbreaks, c("newlat_bin", "newlong_bin"))

#Find brightest per geographical bin and day (i.e., one dot per geog bin per day, selecting out the highest brightness reading )
# Why does geog bin produce different results than newlat and new long bins?
df_brightest_geog <- df_latlongbreaks %>% group_by(newlat_bin, newlong_bin, acq_date)
#df_brightest_geog <- df_latlongbreaks %>% group_by(geog_bin, acq_date)
df_brightest_geog <- df_brightest_geog %>% filter(brightness == max(brightness))
# The line above can create duplicates where day and brightness are the same; see Hawaii

# Export CSV
write.csv(df_brightest_geog, "df_brightest_geog.csv", row.names=FALSE)

mapview(
  df_brightest_geog
  , xcol = "longitude"
  , ycol = "latitude"
  , zcol = "brightness"
  , crs = 4269
  , cex = "brightness"
  , col.regions = pal2(100), at = seq(300, 800, 100)
  , grid = FALSE
  , popup = popupTable(df_brightest_geog,
                       zcol = c("brightness",
                                "acq_date"
                       )
  )
)



#Group by geographic "bin" and date
df_binned <- df_latlongbreaks %>% group_by(newlat_bin, newlong_bin, acq_date)

#Filter 
df_binned_brightness <- df_binned %>% filter(brightness > 500 )

mapview(
  df_binned_brightness
  , xcol = "longitude"
  , ycol = "latitude"
  , crs = 4269
  , grid = FALSE
)


# ----------------------------------- 
# Counting by Month

# Goal: ID and graph days/months of the year with the most reported fires

# Create data datatype (acq_date previously stored as character)
df$acq_date2 <-ymd(df$acq_date)

#Show simple bar plot of daily frequencies
count_by_day <- df %>% count(acq_date)
barplot(count_by_day$n)

# View histogram by month
hist(df$acq_date2, breaks = 12)

# ----------------------------------- 
# Barplot: Count by Month

# Separate date column into month and year columns
df_months <- df %>% 
  mutate(year=lubridate::year(df$acq_date2),
         month=lubridate::month(df$acq_date2),
         month_name=month.name[lubridate::month(df$acq_date2)])

count_by_month <- df_months %>% count(month)

#Simple barplot with month numbers as x-axis values
barplot(count_by_month$n, names.arg = count_by_month$month)

#Export count_by_month for viz
write.csv(count_by_month, "count_by_month.csv", row.names=FALSE)

#Barplot with additional parameters
barplot(count_by_month$n,
        main = "Count by Month",
        xlab = "Month",
        ylab = "Count",
        names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"),
        col = "blue",
        horiz = FALSE)

# July has highest total of the 12 months; investigate further 
# Surprising tendency of lower-brightness fires toward southeast region


# ----------------------------------- 
# Visualizing July fires

# Goal: Learn more about July fires, since July has highest total of the 12 months

july <- df_months %>% filter(month_name=="July")

mapview(
  july
  , xcol = "longitude"
  , ycol = "latitude"
  , zcol = "brightness"
  , crs = 4269
  , cex = "brightness"
  , col.regions = pal2(100), at = seq(100, 800, 100)
  , grid = FALSE
  , popup = popupTable(july,
                       zcol = c("brightness",
                                "acq_date"
                       )
  )
)


# ----------------------------------- 
# Coloring by Month using brightest_high_confidence

# Goal: Color dot based on month to see whether there are trends associated with time of year (summer, Southeast; winter, west, etc.)


# Create data datatype for brightest_high_confidence (acq_date prev stored as char)
brightest_high_confidence$acq_date2 <-ymd(brightest_high_confidence$acq_date)

# Prepare brightest_high_confidence for use visualizing over months 
bhc_months <- brightest_high_confidence %>% 
  mutate(year=lubridate::year(brightest_high_confidence$acq_date2),
         month=lubridate::month(brightest_high_confidence$acq_date2),
         month_name=month.name[lubridate::month(brightest_high_confidence$acq_date2)])


#Export for viz
write.csv(bhc_months, "bhc_months.csv", row.names=FALSE)

mapview(
  bhc_months
  , xcol = "longitude"
  , ycol = "latitude"
  , zcol = "month"
  , crs = 4269
  , cex = "brightness"
  , col.regions = viridis::viridis(12, direction=-1)
  , grid = FALSE
  , popup = popupTable(bhc_months,
                       zcol = c("brightness",
                                "acq_date",
                                "month_name"
                       )
  )
)
#This map demonstrates that the brightest fires are winter fires in the west and PNW, and these trend toward summer/fall; the brightest fires in the middle of the country trend toward spring 


# Visualize which fires are being picked up by each of the two satellites (i.e., Aqua and Terra)

mapview(
  brightest_high_confidence
  , xcol = "longitude"
  , ycol = "latitude"
  , zcol = "satellite"
  , crs = 4269
  #, cex = "brightness"
  , col.regions = viridis::viridis(2, direction=-1)
  , grid = FALSE
  #, popup = popupTable(brightest_high_confidence,
                       #zcol = c("brightness",
                              #  "acq_date"
                                #"month_name"
                       #)
  #)
)

#--------------------------
# Histogram of confidence levels of all df sightings

df_confidence_bins <- df %>% mutate(confidence_bin = cut(confidence, breaks=10))

count_by_confbins<- df_confidence_bins %>% count(confidence_bin)

count_by_confbins

#Barplot with additional parameters
barplot(count_by_confbins$n,
        main = "Confidence Bins",
        xlab = "x",
        ylab = "Count",
        names.arg = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"),
        col = "blue",
        horiz = FALSE)
