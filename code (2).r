### Macroeconomics Project ###

require(tidyverse)

rm(list = ls())

# read data 
# Data: https://fred.stlouisfed.org/series/DCOILBRENTEU
# choose greatest timeframe!

oil.df <- read_csv("DCOILBRENTEU.csv")
colnames(oil.df) = c("Date", "Price")
oil.df$Price = as.numeric(oil.df$Price)

all(!is.na(oil.df)) # Table has NAs
dim(oil.df)
oil.df <- na.omit(oil.df)

all(!is.na(oil.df)) # Table has no NAs
dim(oil.df)

glimpse(oil.df)

#  monthly averages instead of daily data
oil.df$Month <- months(oil.df$Date, abbreviate=T)
oil.df$Year <- format(oil.df$Date, format="%Y")
oil.monthly = aggregate( Price ~ Month + Year , oil.df , mean )

glimpse(oil.monthly)
oil.monthly$Date = paste(oil.monthly$Month, oil.monthly$Year, "1")
oil.monthly$Date = as.Date(oil.monthly$Date, format = "%b %Y %d")
oil.monthly = select(oil.monthly, "Price", "Date")

glimpse(oil.monthly)
oil.monthly = arrange(oil.monthly, Date)
attach(oil.monthly)


plot(Date, Price, type="l")

# First Order Differences
diff_val <- Price[2:length(Price)] - Price[1:length(Price)-1]
plot(Date[1:length(Date)-1], diff_val, type="h"); abline(h=0)

# whatever you were doing
plot(Price[1:length(Price)-1], Price[2:length(Price)],); abline(a=0,b=1)


detach(oil.monthly)
