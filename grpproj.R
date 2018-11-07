#install.packages("data.table")
#install.packages("TSA")
#install.packages("forecast")
#install.packages("anytime")
#install.packages("xts")

require(data.table)
require(TSA)
require(forecast)
require(xts)

data <- fread("BikeSharingDaily.csv")

df <- data.frame(data$date, data$cnt)

df$datec <- as.Date(as.character(df$data.date), '%A, %B %d, %Y')
rownames(df) <- df$datec
df['cnt'] <- df$data.cnt
df <- df[c('datec','cnt')]

df <- xts(df$cnt, df$datec)

plot(df)

y <- window(df, end = "2012-10-31")
y.te <- window(df, start = "2012-11-01")

ndiffs(y)

p <- periodogram(df)
data.table(period = 1/p$freq, spec=p$spec)[order(-spec)][1:4]

# Base model - ARIMA(2,1,3)
fit0 <- auto.arima(y)
(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))
fc0 <- forecast(fit0, h=60)
plot(fc0)

for(i in 1:3) {
  for (j in 1:3){
    z1 <- fourier(ts(y, frequency=375), K=i)
    z2 <- fourier(ts(y, frequency=250), K=j)
    fit <- auto.arima(y, xreg=cbind(z1, z2), seasonal=F)
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit

fc <- forecast(bestfit$fit, 
               xreg=cbind(
                 fourier(ts(y, frequency=375), K=bestfit$i, h=60),
                 fourier(ts(y, frequency=250), K=bestfit$j, h=60)))
plot(fc)




#TBATS
cfg <- tbats(y, seasonal.periods=c(250, 375))
fc.tbats <- forecast(tbats(cfg), h=60)
plot(fc.tbats)

