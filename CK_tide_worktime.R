#an exercise to plot the tides in CK over a week using the reference
#points for the construction work

#install.packages('rtide') 
# install the package on your computer

library(rtide) 
library(ggplot2)
library(scales) # for the date_format function
library(suncalc) #for sunrise/sunset


#load the package whenever you start a new R session

#tide_stations() 
#this returns all stations in US

#tide height from tide_stations is in MLLW in meters

dat = tide_height('Cedar Key',from = as.Date('2018-11-01'),
                  to = as.Date('2019-04-30'), minutes = 15, 
                  tz = 'America/New_York')

#the dates here are the dates you are interested in. 
#So you enter a from and to date in YYYY-MM-DD format.  
#minutes are the minutes for the prediction so a value of 15 is 
#a predicted tide every 15 minutes

x_conversion<- -0.687
dat$TideHeight_NAVD_m=dat$TideHeight + x_conversion 
#convert from MLLW to NAVD using conversion from Peter's table

ft_conversion<- 3.281
dat$TideHeight_NAVD_ft=dat$TideHeight_NAVD_m*ft_conversion
#convert from m to ft using 1 m = 3.281 ft

#rename columns
colnames(dat) <- c("station", "datetime", "mllw_m", "navd_m","navd_ft")

##nothing works from here down...


#grab the sunrise/sunset for one day only as an example

y<-getSunlightTimes(date =  as.Date("2018-10-23"), lat = 29.14, lon = -83.04, data = NULL,
                    keep = c("sunrise", "sunset"), tz = "America/New_York")


###to do

#calculate the "work time" per day that the tides are below -2.5 dat$navd_ft

#this needs to be bounded such as before 7 AM and before 7 PM so that the
#work time is during daylight hours.  Can use a global cutoff or extract

#return as a table (simplest) or some sort of graph that shows
#each day of the month and then the amount of work time for that day
#sunrise/sunset from the code on line 45.