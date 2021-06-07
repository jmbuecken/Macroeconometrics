#libraries
library (vars)
library (urca)
library (tseries)

#start with the data
oil_aut <- read.csv("C:/Users/lbryson/Downloads/DP_LIVE_04062021095348549.csv")
View(oil_aut)
mean(oil_aut$Value)
sd(oil_aut$Value)
acf(Y$Value, lwd = 3)

#first-order differences 
#(dont really need the DF test, as non-stationary is
#not a problem in dynamic regressions)
plot(oil_aut$Value[1:40], oil_aut$Value[2:41],)

#ar1
for(ii in 2:41){ar1[ii] <- 0.8*ar1[ii-1]}
acf(ar1,lwd=3)
lines(0:30, 0.8^(0:30), col = 'red', lwd = 2)

#ma1
ma1 <- Y$Value[2:41]+0.8*Y$Value[1:40]
plot(1:40, ma1, col = 'blue', type = 'l')
acf(ma1, col = 'red', lwd = 3)
mean(ma1); var(ma1)

#construct ts
oil_ts <- ts(oil_aut, frequency = 1)
oil_new <- subset(oil_ts, select = c(Value))

#ARIMA
arima (x = oil_new,order = c(1,0,0))

#df test
adf.test(oil_new,k=0)