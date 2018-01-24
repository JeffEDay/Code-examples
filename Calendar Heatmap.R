# http://margintale.blogspot.in/2012/04/ggplot2-time-series-heatmaps.html
library(ggplot2)
library(plyr)
library(scales)
library(zoo)
library(lubridate)
df <- read.csv("Master1.23.2018.csv")
df$date <- as.Date(df$date_var, "%m/%d/%Y")  # format date


# Create Month Week
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df$week <-week(df$date)
df$day <- day(df$date)
# Use the next line when you want month week to span Sun-Sat time period
# and confined to 5 weeks
# df$monthweek <- ceiling(df$day/7)
df$year <-year(df$date)
df$month <- month(df$date)
df$monthf <- factor(df$month)
library(dplyr)
# Use this function when you want partial weeks to count as a week,
# e.g., first day  of month is a Wednesday and therefore Sunday thru 
# Tuesday for week one will be blank.  But we will have 6 weeks sometimes

first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}
df$monthweek <-ceiling((day(df$date) + first_day_of_month_wday(df$date) - 1) / 7)


df$monthf <-month.abb[df$monthf]
# Let's order the levels so they appear in the map in the desired order
df$monthf <- factor(df$monthf, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov",
                                          "Dec"))
df$Day_of_week <-factor(df$Day_of_week, levels = c("Saturday", "Friday", "Thursday",
                                                   "Wednesday", "Tuesday","Monday",
                                                   "Sunday"))

#df <- ddply(df,.(yearmonthf), transform, monthweek= 1 + (week-min(week)), minw=min(week))  # compute week number of month
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "Day_of_week", "WoW_DAU")]
head(df)
#>   year yearmonthf monthf week monthweek weekdayf VIX.Close
#> 1 2012   Jan 2012    Jan    1         1      Tue     22.97
#> 2 2012   Jan 2012    Jan    1         1      Wed     22.22
#> 3 2012   Jan 2012    Jan    1         1      Thu     21.48
#> 4 2012   Jan 2012    Jan    1         1      Fri     20.63
#> 5 2012   Jan 2012    Jan    2         2      Mon     21.07
#> 6 2012   Jan 2012    Jan    2         2      Tue     20.69




# Plot
ggplot(df, aes(monthweek, Day_of_week, fill = WoW_DAU)) + 
  geom_tile(colour = "white") + 
  facet_grid(year ~ monthf) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Calendar Heatmap", 
       subtitle="WoW DAU Level", 
       fill="DAU")



