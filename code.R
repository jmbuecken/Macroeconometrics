### Macroeconomics Project ###

rm(list = ls())

# read data
oil_prices <- read_csv("Downloads/oil-import-prices-all-countries.csv")

# drop all countries, keep Austria
temp_oil_prices_aut <- oil_prices[which(oil_prices$LOCATION=='AUT'),]

# delete unnecessary columns
temp <- c("TIME", "Value")
oil_prices_aut <- temp_oil_prices_aut[temp]
rm(temp_oil_prices_aut)

# plot 1st order difference
plot(oil_prices_aut[1:40], oil_prices_aut[2:41], 'l')
plot(oil_prices_aut$Value[1:40], oil_prices_aut$Value[2:41],)