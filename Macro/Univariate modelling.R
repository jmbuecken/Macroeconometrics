### Univariate  Time-Series Analysis###
library(forecast)
library(tseries)


#1. Total production Austria, monthly 01/2020 - 01/2021----

  #data-source: https://fred.stlouisfed.org/series/AUTPROINDMISMEI
  #I followed the tutorial on: https://www.youtube.com/watch?v=VPhyVSJMbpA&t=326s
  #I used data for "total production Austria", originally I wanted to use the data for GDP, but I only found 
  #this normalized data, which was not so easy to work with: https://fred.stlouisfed.org/series/AUTLORSGPNOSTSAM
  # in case you figure out how to use this datase, we can also use it instead of total production. 

## Import data & create time-series
df <- read_csv("production_of_total_industry.csv")
class(df) # Needs to be converted into time-series data
prod=ts(df$AUTPROINDMISMEI, start = 01/2000, frequency = 12)
# We have 253 observations. Jan 2000-Jan 2021. Maybe betterto exclude Jan 2021? --> Yes, I think it would be better to exclude Jan 2021
plot(prod)

## Check Stationarity
acf(prod) # data is non-stationary atm 
pacf(prod) # no issues
adf.test(prod) # p-value of 0.4078 shows again that data is non-stationary

## Selecting the best model:
prod.model = auto.arima(prod, ic="aic", trace = TRUE)
 #Result:Best model: ARIMA(0,1,0)(0,0,2)[12] with drift. --> Does the 12 stand for months?  
    # What does this mean?!
 # (0,1,0)-> non-seasonal part of the model
 # (0,0,2)-> seasonal part of the model 
prod.model


## Check Stationarity for prod.mod: ACF of model residuals
acf(ts(prod.model$residuals))
pacf(ts(prod.model$residuals)) #is this ok? 

## Forecasting
prod.forecast=forecast(prod.model,level = c(95),h=5*12)
prod.forecast 
plot(prod.forecast)

## Validate Forecast
Box.test(prod.forecast$residuals, lag = 5, type = "Ljung-Box")
 #Result interpretation? p-value = 0.02386
 #The null hypothesis of the Box Ljung Test is that our model does not show lack of fit.
 #A significant p-value in this test rejects the null hypothesis that the time series isn’t autocorrelated.
 # So this model does not forecast well?

#2. LOG Total production Austria, monthly 01/2020 - 01/2021------
  #Since the prod.model in #1 is not good for forecasting I'm repeating the steps with the logarithmic values.

## Take Logarithm of total production
View(df)
log.prod<- log(df$AUTPROINDMISMEI)
ts.log.pro = ts(log.prod, start = 01/2000, frequency = 12)
plot(ts.log.pro)

## Check Stationarity
acf(ts.log.pro) #non-stationary
pacf(ts.log.pro)
adf.test(ts.log.pro)

## Select best model:
log.prod.model = auto.arima(ts.log.pro, ic="aic", trace = TRUE)
# Result: ARIMA(1,1,3) with drift // Why are all AIC negative here?
log.prod.model

## Check Stationarity for prod.mod: ACF of model residuals
acf(ts(log.prod.model$residuals))
pacf(ts(log.prod.model$residuals)) 

## Forecasting:
log.prod.forecast=forecast(log.prod.model,level = c(95),h=5*12)
log.prod.forecast
plot(prod.forecast)

## Validate Forecast
Box.test(log.prod.forecast$residuals, lag = 5, type = "Ljung-Box")
 # p-value of 1 seems a bit sketchy. But should be better like that.


#3. Future Crude Oil Prices, monthly 01/2020 - 01/2021------

  #data-source: https://www.investing.com/commodities/crude-oil-historical-data
  #! Those are prices for futures: Shall we treat them differently? Maybe important for joint analysis? 

## Import data & Convert to time-series:
df1 <- read.csv("CrudeOilPrices.csv")
class(df1) # Needs to be converted into time-series data
oilprice=ts(df1$Price, start = 01/2000, frequency = 12)
plot(oilprice)

## Check stationarity
acf(oilprice)
pacf(oilprice)
adf.test(oilprice, k=12) #took k=12 because it's monthly data
#-> non-stationary

## Taking first differences: (To see if an AR(1) model is fitting)
oilprice_d1 <- diff(oilprice, differences = 1) 
plot(oilprice_d1)
acf(oilprice_d1)
pacf(oilprice_d1)
adf.test(oilprice_d1) # p-value=0.01 -> first differences are stationary, looks good, still gonna try with the modelselection command though. 

oilprice.model = auto.arima(oilprice_d1, ic="aic", trace = TRUE)
 #Result: Best model: ARIMA(1,0,0)(1,0,0)[12] with zero mean: AR(1) with seasonal and non-seasonal component?
oilprice.model

acf(oilprice.model$residuals)
pacf(oilprice.model$residuals)

## Forecasting:
oilprice.forecast=forecast(oilprice.model, level = c(95),h=5*12)
oilprice.forecast
plot(oilprice.forecast) #doesn't look very exciting lol


## Validate Forecast
Box.test(oilprice.forecast$residuals, lag = 5, type = "Ljung-Box")
 #p-value=0.4935: Means that we reject the nullhypothesis that there is still autocorrelation if I understood it correctly.

#4. LOG Future Crude Oil Prices, monthly 01/2020 - 01/2021----
  
  # Analysis with log-prices is not necessary - I think?

#View(df1)
#log.oilprice<- log(df1$Price)
#ts.log.oilprice = ts(log.oilprice, start = 01/2000, frequency = 12)
#plot(ts.log.oilprice)

## Check Stationarity
#acf(ts.log.oilprice) #non-stationary
#pacf(ts.log.oilprice)
#adf.test(ts.log.oilprice)

## Select best model:
#log.oilprice.model = auto.arima(ts.log.oilprice, ic="aic", trace = TRUE)
# Result: Best model: ARIMA(1,1,0)(1,0,0)[12]
#log.oilprice.model

##Check Stationarity for prod.mod: ACF of model residuals
#acf(ts(log.oilprice.model$residuals))
#pacf(ts(log.oilprice.model$residuals)) 

##Forecasting:
#log.oilprice.forecast=forecast(log.oilprice.model,level = c(95),h=5*12)
#log.oilprice.forecast
#plot(log.oilprice.forecast)

## Validate Forecast
#Box.test(log.oilprice.forecast$residuals, lag = 5, type = "Ljung-Box")
# p-value insignificant: Means that we reject the nullhypothesis that there is still autocorrelation if I understood it correctly?

#I did the same with GDP to compute the multivariate model
df2 <- read.csv("AUTLORSGPNOSTSAM.csv")
class(df2)
gdp=ts(df2$AUTLORSGPNOSTSAM, start = 01/2000, frequency = 12)
plot(gdp)

acf(gdp)
pacf(gdp)
adf.test(gdp, k=12)

gdp_d1 <- diff(gdp, differences = 1) 
plot(gdp_d1)
acf(gdp_d1)
pacf(gdp_d1)
adf.test(gdp_d1)

gdp.model = auto.arima(gdp_d1, ic="aic", trace = TRUE)
gdp.model

acf(gdp.model$residuals)
pacf(gdp.model$residuals)

gdp.forecast=forecast(gdp.model, level = c(95),h=5*12)
gdp.forecast
plot(gdp.forecast)

Box.test(gdp.forecast$residuals, lag = 5, type = "Ljung-Box")
