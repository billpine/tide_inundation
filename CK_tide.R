#an exercise to plot the tides in CK over a week using the reference
#points for the construction work

install.packages('rtide') 
# install the package on your computer

library(rtide) 
#load the package whenever you start a new R session

#tide_stations() 
#this returns all stations in US

#tide height from tide_stations is in MLLW in meters

dat = tide_height('Cedar Key',from = as.Date('2018-08-21'),
                  to = as.Date('2018-08-24'), minutes = 15, 
                  tz = 'America/New_York')

#the dates here are the dates you are interested in. So you enter a from and to date in YYYY-MM-DD format.  minutes are the minutes for the prediction so a value of 15 is a predicted tide every 15 minutes


x_conversion<- -0.687
dat$TideHeight_NAVD_m=dat$TideHeight + x_conversion 
#convert from MLLW to NAVD using conversion from Peter's table

ft_conversion<- 3.281
dat$TideHeight_NAVD_ft=dat$TideHeight_NAVD_m*ft_conversion
#convert from m to ft using 1 m = 3.281 ft

windows(record=T)

library(ggplot2)
library(scales) # for the date_format function

ggplot(data = dat, aes(x = DateTime, y = TideHeight_NAVD_ft)) +
  geom_line() +
  scale_x_datetime(name = "August 21-24, 2018",
                   labels = date_format("%H:%M", tz="America/New_York")) +
  scale_y_continuous(name = "Tide Height NAVD (ft)") +
  geom_hline(yintercept = -1.45, color = "green", size=1, linetype = 2) +
  geom_hline(yintercept = -1.20, color = "blue", size=1, linetype = 3) +
  geom_hline(yintercept = -1.95, color = "blue", size=1, linetype = 3) +
  geom_hline(yintercept = -0.7, color = "black", size=1, linetype = 1) +
    ggtitle("Cedar Key")

#-1.45 is Target
#-1.2 is Target +3
#-1.95 Target -6
#-0.7 is Refrigerator

#need to add labels or legend to the geom_hline
#one option https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline

#need to make x axis finer time scale so you can see more tick marks and days 

