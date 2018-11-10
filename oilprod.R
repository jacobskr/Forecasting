require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)
require(dplyr)


oil <- read.csv("US_OIL.csv")
oil <- oil[,2]
oil <- ts(oil, freq=365.25/7, start=1991+38/365.25)
plot(oil)
oil.tr <- ts(oil[1:(length(oil)-52)],freq=365.25/7, start=1991+38/365.25)
len <- length(oil.tr)
oil.val <- ts(oil[(length(oil)-51):length(oil)],freq=365.25/7, start=1991+(38+(7*length(oil.tr)))/365.25)

#naive model
fc_naive <- naive(oil.tr, h = 104)
autoplot(fc_naive)

#Using period set in ts, find # fourier regressors to use to minimize AIC
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  z <- fourier(oil.tr, K=i)
  fit <- auto.arima(oil.tr, xreg=fourier(oil.tr, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- list(aicc=fit$aicc, k=i, fit=fit)
  else break;
}
bestfit$k
fc0 <- forecast(bestfit$fit, xreg=fourier(oil.tr, K=12, h=104))
autoplot(fc)

#Try finding potential multi-seasonality. Use periodogram to find
#frequencies wih the highest spectral power densities
periodogram(oil.val)
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:5]

bestfit1 <- list(aicc=Inf)
for(i in 1:3) {
  for (j in 1:3){
    z1 <- fourier(D, K=i)
    z2 <- fourier(ts(oil.tr, frequency=75), K=j)
    fit <- auto.arima(oil.tr, xreg=cbind(z1, z2), seasonal=F, stepwise = F)
    if(fit$aicc < bestfit1$aicc) {
      bestfit1 <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit1

fc1 <- forecast(bestfit1$fit, 
                       xreg=cbind(
                         fourier(ts(oil.tr, frequency=6.944444), K=bestfit1$i, h=length(oil.val)),
                         fourier(ts(oil.tr, frequency=75), K=bestfit1$j, h=length(oil.val))))
autoplot(fc1)

#Not good at all

# We can check if a seasons regressor will make forecast better
seasons <- read.csv("seasons.csv")
seasons.tr <- seasons[1:(length(oil)-52),c(2,3,4)]


auto.arima(oil.tr, xreg = seasons.tr)
