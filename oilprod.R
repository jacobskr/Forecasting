require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)
require(dplyr)


dta <- read.csv("US_OIL.csv")
oil <- dta[,2]
oil <- ts(oil, freq=365.25/7, start=1991+38/365.25)
plot(oil)
oil.tr <- ts(oil[1:(length(oil)-52)],freq=365.25/7, start=1991+38/365.25)
len <- length(oil.tr)
oil.val <- ts(oil[(length(oil)-51):length(oil)],freq=365.25/7, start=1991+(38+(7*length(oil.tr)))/365.25)

#seasons data
seasons <- read.csv("seasons.csv")
seasons <- seasons[,c(2,3,4)]
seasons.tr <- seasons[1:(length(oil)-52),]
seasons.val <- seasons[(length(oil)-51):length(oil),]

#oil ts with seasons
oil_xregs <- data.frame(dta[,2])
oil_xregs[,c(2,3,4)] <- seasons
oil_xregs <- ts(oil_xregs, freq=365.25/7, start=1991+38/365.25)
oil_xregs.tr <- window(oil_xregs, end=1991+(38+(7*(nrow(oil_xregs)-53)))/365.25)
oil_xregs.val <- window(oil_xregs, start=1991+(38+(7*(nrow(oil_xregs)-52)))/365.25)
colnames <- c("barrels", "winter", "spring", "summer")
colnames(oil_xregs.tr) <- colnames
colnames(oil_xregs.val) <- colnames
colnames(oil_xregs) <- colnames

#naive model
fc_naive <- naive(oil.tr, h = 52)
autoplot(fc_naive)

#Base Arima Model
fc_base <- forecast(auto.arima(oil.tr, seasonal = F), h = 52)
autoplot(fc_base)

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
fc0 <- forecast(bestfit$fit, xreg=fourier(oil.tr, K=12, h=52))
autoplot(fc0, h=52)

#Try finding potential multi-seasonality. Use periodogram to find
#frequencies wih the highest spectral power densities
periodogram(oil.val)
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:5]

bestfit1 <- list(aicc=Inf)
for(i in 1:3) {
  for (j in 1:3){
    z1 <- fourier(ts(oil.tr, frequency=6.944444), K=i)
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
autoplot(fc1, h=52)

#Not good at all

# We can check if a seasons regressor will make forecast better
covariates <- c("winter", "spring", "summer")
fit_seasons <- auto.arima(oil_xregs.tr[,"barrels"], xreg = oil_xregs.tr[, covariates])
fc_seasons <- forecast(fit_seasons, xreg = oil_xregs.val[, covariates])
autoplot(fc_seasons, h=52)




#Can we do fourier with other external regressors?
bestfit2 <- list(aicc=Inf)
for(i in 1:25)
{
  z <- data.frame(fourier(oil_xregs.tr[,"barrels"], K=i))
  z['winter'] <- seasons.tr[,'winter']
  z['spring'] <- seasons.tr[,'spring']
  z['summer'] <- seasons.tr[,'summer']
  z <- ts(z, freq=365.25/7, start=1991+38/365.25)
  fit <- auto.arima(oil_xregs.tr[,"barrels"], xreg=z, seasonal=FALSE)
  if(fit$aicc < bestfit2$aicc)
    bestfit2 <- list(aicc=fit$aicc, k=i, fit=fit)
  else break;
}
bestfit2$k
fc_comb <- forecast(bestfit2$fit, xreg=cbind(fourier(oil_xregs.tr, K=8, h=52), oil_xregs.val[, covariates]))
autoplot(fc_comb, h=52)


#Multiseasonal dataset
wkly <- 365.25 / 7
mthly <- wkly / 12
oil_msts <- data.frame(dta[,2])
oil_msts[,c(2,3,4)] <- seasons
oil_msts <- msts(oil_msts, seasonal.periods=c(wkly, mthly), start=1991+38/365.25)
oil_msts.tr <- window(oil_msts, end=1991+(38+(7*(nrow(oil_msts)-53)))/365.25)
oil_msts.val <- window(oil_msts, start=1991+(38+(7*(nrow(oil_msts)-52)))/365.25)
colnames(oil_msts.tr) <- colnames
colnames(oil_msts.val) <- colnames
colnames(oil_msts) <- colnames

#tbats using msts
fit_tbats.msts <- tbats(oil_msts.tr[, "barrels"], seasonal.periods=c(wkly, mthly))
fc_tbats.msts <- forecast(fit_tbats.msts, h=52)
autoplot(fc_tbats.msts)

#Try to use covariates with msts
fit_arima.msts <- auto.arima(oil_msts.tr[, "barrels"], xreg=oil_msts.tr[, c(2,3,4)])
fc_arima.msts <- forecast(fit_arima.msts, xreg=oil_msts.val[, c(2,3,4)], h=52)
autoplot(fc_arima.msts)


checkresiduals(fit_seasons2)
Box.test(fit_seasons2$residuals)

