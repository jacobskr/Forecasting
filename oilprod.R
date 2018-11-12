require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(nnfor)

#https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=wgfupus2&f=W

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

checkresiduals(fc_naive)

#ETS Model
fit_ets <- ets(oil.tr)
fc_ets <- forecast(fit_ets, h=52)
autoplot(fc_ets)

checkresiduals(fit_ets)

#STLF Model
fit_stlf <- stlf(oil.tr)
fc_stlf <- forecast(fit_stlf, h=52)
autoplot(fc_stlf)

checkresiduals(fit_stlf)

#Base Arima Model
fit_base <- auto.arima(oil.tr, stepwise=F)
fc_base <- forecast(fit_base, h = 52)
autoplot(fc_base)

checkresiduals(fc_base)

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
bestfit
bestfit$k
fc_freg <- forecast(bestfit$fit, xreg=fourier(oil.tr, K=12, h=52))
autoplot(fc_freg, h=52)

checkresiduals(bestfit$fit)

#Try finding potential multi-seasonality. Use periodogram to find
#frequencies wih the highest spectral power densities
periodogram(oil.tr)
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:5]

bestfit1 <- list(aicc=Inf)
for(i in 1:3) { 
  for (j in 1:3){ #i and j need to be < freq/2
    z1 <- fourier(ts(oil.tr, frequency=7.058824), K=i)
    z2 <- fourier(ts(oil.tr, frequency=120), K=j)
    fit <- auto.arima(oil.tr, xreg=cbind(z1, z2), seasonal=F, stepwise = F)
    if(fit$aicc < bestfit1$aicc) {
      bestfit1 <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit1

fc_fper <- forecast(bestfit1$fit, 
                       xreg=cbind(
                         fourier(ts(oil.tr, frequency=7.058824), K=bestfit1$i, h=length(oil.val)),
                         fourier(ts(oil.tr, frequency=120), K=bestfit1$j, h=length(oil.val))))
autoplot(fc_fper, h=52)

checkresiduals(bestfit1$fit)

#Not good at all

# We can check if a seasons regressor will make forecast better
covariates <- c("winter", "spring", "summer")
fit_seasons <- auto.arima(oil_xregs.tr[,"barrels"], xreg = oil_xregs.tr[, covariates])
fc_seasons <- forecast(fit_seasons, xreg = oil_xregs.val[, covariates], h=52)
autoplot(fc_seasons, h=52)

checkresiduals(fit_seasons)


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
bestfit2
bestfit2$k
fc_comb <- forecast(bestfit2$fit, xreg=cbind(fourier(oil_xregs.tr, K=8, h=52), oil_xregs.val[, covariates]))
autoplot(fc_comb, h=52)

checkresiduals(bestfit2$fit)

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
autoplot(fc_tbats.msts, h=52)

checkresiduals(fit_tbats.msts)

#Try to use covariates with msts
fit_arima.msts <- auto.arima(oil_msts.tr[, "barrels"], xreg=oil_msts.tr[, c(2,3,4)])
fc_arima.msts <- forecast(fit_arima.msts, xreg=oil_msts.val[, c(2,3,4)], h=52)
autoplot(fc_arima.msts, h=52)


checkresiduals(fit_arima.msts)

#Maybe a neural network will be nice
  #no xregs
fit_nn <- nnetar(oil_xregs.tr[, "barrels"], lambda = 0.5)
autoplot(forecast(fit_nn, h=52))

  #simulattion path for 9 possible future paths
set.seed(2005)
sim <- ts(matrix(0, nrow=30L, ncol=9L), frequency=365.25/7 , start = end(oil_xregs.tr)[1L]+(7/365.25))
for(i in seq(9))
  sim[,i] <- simulate(fit_nn, nsim=30L)
autoplot(window(oil_xregs.tr[, "barrels"], start=1991+(38+(7*(nrow(oil_xregs.tr)-300)))/365.25)) + autolayer(sim)

fc_nn <- forecast(fit_nn, PI=T, h=52)
autoplot(fc_nn)
checkresiduals(fit_nn$residuals)
Box.test(fit_nn$residuals)

  #xregs
fit_nn.xregs <- nnetar(oil_xregs.tr[, "barrels"], xreg = oil_xregs.tr[, c(2,3,4)], lambda = 0.5)
fc_nn.xregs <- forecast(fit_nn.xregs, xreg = oil_xregs.val[, c(2,3,4)], PI=T, h=52)
autoplot(fc_nn.xregs)

checkresiduals(fit_nn.xregs$residuals)
Box.test(fit_nn.xregs$residuals)

# "Extreme learning machines"
fit_elm <- elm(oil_xregs.tr[, "barrels"], type="lasso")
fc_elm <- forecast(fit_elm, PI=T, h=52)
autoplot(fc_elm)

# ELM w/ xreg
fit_elm.xreg <- elm(oil_xregs.tr[, "barrels"], xreg=oil_xregs.tr[, c(2,3,4)])
fc_elm.xreg <- forecast(fit_elm.xreg, xreg = oil_xregs[, c(2,3,4)], PI=T, h=52)
autoplot(fc_elm.xreg)

#Combine Models
comb_msts <- (fc_arima.msts[["mean"]] + fc_tbats.msts[["mean"]])/2
autoplot(oil) + autolayer(comb_msts)
comb_fourier <- (fc_comb[["mean"]] + fc_freg[["mean"]] +
                   fc_fper[["mean"]])/3
autoplot(oil) + autolayer(comb_fourier)
comb_season <- (fc_seasons[["mean"]] + fc_freg[["mean"]] +
                  fc_elm.xreg[["mean"]] + fc_nn.xregs[["mean"]] +
                  fc_comb[["mean"]])/5
autoplot(oil) + autolayer(comb_season)
comb_nn <- (fc_nn[["mean"]] + fc_nn.xregs[["mean"]] +
              fc_elm[["mean"]] + fc_elm.xreg[["mean"]])/4
autoplot(oil) + autolayer(comb_nn)
comb_simp <- (fc_ets[["mean"]] + fc_stlf[["mean"]] + fc_base[["mean"]])/3
autoplot(oil) + autolayer(comb_simp)

#Compare Models
NAIVE_ac <- accuracy(fc_naive, oil)
ETS_ac <- accuracy(fc_ets, oil)
STLF_ac <- accuracy(fc_stlf, oil)
ARIMA.base_ac <- accuracy(fc_base, oil)
ARIMA.freg_ac <- accuracy(fc_freg, oil)
ARIMA.fper_ac <- accuracy(fc_fper, oil)
ARIMA.sns_ac <- accuracy(fc_seasons, oil)
ARIMA.comb_ac <- accuracy(fc_comb, oil)
ARIMA.msts_ac <-  accuracy(fc_arima.msts, oil)
TBATS_ac <- accuracy(fc_tbats.msts, oil)
NNETAR_ac <- accuracy(fc_nn, oil)
NNETAR.sns_ac <- accuracy(fc_nn.xregs, oil)
ELM_ac <- accuracy(fc_elm, oil)
ELM.sn_ac <- accuracy(fc_elm.xreg, oil)
COMB.fourier_ac <- accuracy(comb_fourier, oil)
COMB.msts_ac <- accuracy(comb_msts, oil)
COMB.nn_ac <- accuracy(comb_nn, oil)
COMB.season_ac <- accuracy(comb_season, oil)
COMB.simp_ac <- accuracy(comb_simp, oil)

sort(c(NAIVE_ac = accuracy(fc_naive, oil)["Test set","RMSE"],
  ETS_ac = accuracy(fc_ets, oil)["Test set","RMSE"],
  STLF_ac = accuracy(fc_stlf, oil)["Test set","RMSE"],
  ARIMA.base_ac = accuracy(fc_base, oil)["Test set","RMSE"],
  ARIMA.freg_ac = accuracy(fc_freg, oil)["Test set","RMSE"],
  ARIMA.fper_ac = accuracy(fc_fper, oil)["Test set","RMSE"],
  ARIMA.sns_ac = accuracy(fc_seasons, oil)["Test set","RMSE"],
  ARIMA.comb_ac = accuracy(fc_comb, oil)["Test set","RMSE"],
  ARIMA.msts_ac =  accuracy(fc_arima.msts, oil)["Test set","RMSE"],
  TBATS_ac = accuracy(fc_tbats.msts, oil)["Test set","RMSE"],
  NNETAR_ac = accuracy(fc_nn, oil)["Test set","RMSE"],
  NNETAR.sns_ac = accuracy(fc_nn.xregs, oil)["Test set","RMSE"],
  ELM_ac = accuracy(fc_elm, oil)["Test set","RMSE"],
  ELM.sn_ac = accuracy(fc_elm.xreg, oil)["Test set","RMSE"],
  COMB.fourier_ac = accuracy(comb_fourier, oil)["Test set","RMSE"],
  COMB.msts_ac = accuracy(comb_msts, oil)["Test set","RMSE"],
  COMB.nn_ac = accuracy(comb_nn, oil)["Test set","RMSE"],
  COMB.season_ac = accuracy(comb_season, oil)["Test set","RMSE"],
  COMB.simp_ac = accuracy(comb_simp, oil)["Test set","RMSE"]))

#winner!
autoplot(oil.tr) + autolayer(comb_season)



         