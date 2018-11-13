require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(nnfor)
require(ELMR)

#https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=wgfupus2&f=W
dta <- read.csv("US_OIL.csv")
dta2 <- tail(dta, 260)[,2]

#seasons data
seasons <- read.csv("seasons.csv")
seasons <- tail(seasons, 260)
seasons <- seasons[,c(2,3,4)]
seasons.tr <- seasons[1:208,]
seasons.val <- seasons[209:260,]

# True frequency training/validation sets
oil_freq <- ts(dta2, frequency = 365.25/7, start=2014-(46/365.25))
oil_freq.tr <- ts(oil_freq[1:208], freq=365.25/7, start=2014-(46/365.25))
oil_freq.val <- ts(oil_freq[(length(oil_freq)-51):length(oil_freq)],
                   freq=365.25/7, 
                   start=1991+(38+(7*(nrow(dta)-51)))/365.25)
autoplot(oil_freq.tr) + autolayer(oil_freq.val)

#Rounded frequency training/validation sets
oil_round <- ts(dta2, freq=52, start = c(2013, 46))
oil_round.tr <- ts(oil_freq[1:208], freq=52, start=c(2013, 46))
oil_round.val <- ts(oil_freq[209:260], freq=52, start=c(2017, 46))
autoplot(oil_freq.tr) + autolayer(oil_freq.val)

#oil ts with seasons
oil_xregs <- data.frame(dta[,2])
oil_xregs[,c(2,3,4)] <- seasons
oil_xregs <- ts(oil_xregs, freq=365.25/7, start=1991+38/365.25)
oil_xregs.tr <- window(oil_xregs, start=1991+(38+(7*(nrow(oil_xregs)-260)))/365.25,
                       end=1991+(38+(7*(nrow(oil_xregs)-52)))/365.25)
oil_xregs.val <- window(oil_xregs, start=1991+(38+(7*(nrow(oil_xregs)-51)))/365.25)
colnames <- c("barrels", "winter", "spring", "summer")
colnames(oil_xregs.tr) <- colnames
colnames(oil_xregs.val) <- colnames
colnames(oil_xregs) <- colnames

#naive model
fc_naive <- naive(oil_freq.tr, h = 52)
autoplot(fc_naive) + autolayer(oil_freq.val)

checkresiduals(fc_naive)

#ETS Model
fit_ets <- ets(oil_freq.tr)
fc_ets <- forecast(fit_ets, h=52)
autoplot(fc_ets) + autolayer(oil_freq.val)

checkresiduals(fit_ets)

#STLF Model
fit_stlf <- stlf(oil_freq.tr)
fc_stlf <- forecast(fit_stlf, h=52)
autoplot(fc_stlf)
autoplot(fc_stlf) + autolayer(oil_freq.val)

checkresiduals(fit_stlf)

#Base Arima Model
fit_base <- auto.arima(oil_round.tr, stepwise=T)
fc_base <- forecast(fit_base, h = 52)
autoplot(fc_base)

checkresiduals(fc_base)

#Using period set in ts, find # fourier regressors to use to minimize AIC
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  z <- fourier(oil_freq.tr, K=i)
  fit <- auto.arima(oil_freq.tr, xreg=fourier(oil.tr, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- list(aicc=fit$aicc, k=i, fit=fit)
  else break;
}
bestfit
bestfit$k
fc_freg <- forecast(bestfit$fit, xreg=fourier(oil_freq.tr, K=6, h=52))
autoplot(fc_freg, h=52)
autoplot(fc_freg) + autolayer(oil_freq.val)

checkresiduals(bestfit$fit)

#Try finding potential multi-seasonality. Use periodogram to find
#frequencies wih the highest spectral power densities
pd <- periodogram(oil_freq.tr)
data.table(period=1/pd$freq, spec=pd$spec)[order(-spec)][1:5]

bestfit1 <- list(aicc=Inf)
for(i in 0:25) { 
  for (j in 0:15){ 
    for (p in 0:15){ #i,j, and p need to be < freq/2
      try({
      z1 <- fourier(ts(oil_freq.tr, frequency=54), K=i)
      z2 <- fourier(ts(oil_freq.tr, frequency=72), K=j)
      z3 <- fourier(ts(oil_freq.tr, frequency=30.85714286), K=p)
      fit <- auto.arima(oil_freq.tr, xreg=cbind(z1, z2, z3), seasonal=F, stepwise = T)
      print(cbind(i,j,p))
      })
      if(fit$aicc < bestfit1$aicc) {
        bestfit1 <- list(aicc=fit$aicc, i=i, j=j, p=p, fit=fit)
      }
    }
  }
}
bestfit1
#(3,1,3) was the best
fc_fper <- forecast(bestfit1$fit, 
                       xreg=cbind(
                         fourier(ts(oil_freq.tr, frequency=54),
                                 K=bestfit1$i, h=length(oil_freq.val)),
                         fourier(ts(oil_freq.tr, frequency=72), K=bestfit1$j,
                                 h=length(oil_freq.val)),
                         fourier(ts(oil_freq.tr, frequency=30.85714286),
                                 K=bestfit1$p, h=length(oil_freq.val))))
autoplot(fc_fper, h=52)
autoplot(fc_fper, h=52) + autolayer(oil_freq.val)

checkresiduals(bestfit1$fit)


# We can check if a seasons regressor will make forecast better
covariates <- c("winter", "spring", "summer")
fit_seasons <- auto.arima(oil_freq.tr, xreg = seasons.tr)
fc_seasons <- forecast(fit_seasons, xreg = seasons.val, h=52)
autoplot(fc_seasons, h=52)
autoplot(fc_seasons, h=52) + autolayer(oil_round.val)

checkresiduals(fit_seasons)


#Can we do fourier with other external regressors?
bestfit2 <- list(aicc=Inf)
for(i in 1:25)
{
  #z <- data.frame(fourier(oil_xregs.tr[,"barrels"], K=i))
  #z['winter'] <- seasons.tr[,'winter']
  #z['spring'] <- seasons.tr[,'spring']
  #z['summer'] <- seasons.tr[,'summer']
  #z <- ts(z, freq=365.25/7, start=1991+38/365.25)
  #fit <- auto.arima(oil_xregs.tr[,"barrels"], xreg=z, seasonal=FALSE)
  x1 <- fourier(oil_freq.tr, K=i)
  x2 <- seasons.tr
  fit <- auto.arima(oil_freq.tr, xreg=cbind(x1, x2), seasonal=F)
  if(fit$aicc < bestfit2$aicc)
    bestfit2 <- list(aicc=fit$aicc, k=i, fit=fit)
  else break;
}
bestfit2
bestfit2$k
fc_comb <- forecast(bestfit2$fit, xreg=cbind(fourier(oil_freq.tr, K=bestfit2$k, h=52), seasons.val))
autoplot(fc_comb, h=52)
autoplot(fc_comb, h=52) + autolayer(oil_round.val)

checkresiduals(bestfit2$fit)

#Multiseasonal dataset
wkly <- 365.25 / 7
mthly <- wkly / 12
oil_msts <- data.frame(dta2)
oil_msts[,c(2,3,4)] <- seasons
oil_msts <- msts(oil_msts, seasonal.periods=c(wkly, mthly),
                 start=2014-(46/365.25))
oil_msts.tr <- window(oil_msts,
                      end=(2014-(46/365.25))+(7*(nrow(oil_msts)-53))/365.25)
oil_msts.val <- window(oil_msts, 
                       start=(2014-(46/365.25))+(7*(nrow(oil_msts)-52))/365.25)
colnames <- c("barrels", "winter", "spring", "summer")
colnames(oil_msts.tr) <- colnames
colnames(oil_msts.val) <- colnames
colnames(oil_msts) <- colnames

#tbats using msts
fit_tbats.msts <- tbats(oil_msts.tr[, "barrels"], seasonal.periods=c(wkly, mthly))
fc_tbats.msts <- forecast(fit_tbats.msts, h=52)
autoplot(fc_tbats.msts, h=52)
autoplot(fc_tbats.msts, h=52) + autolayer(oil_round.val)

checkresiduals(fit_tbats.msts)

#Try to use covariates with msts
fit_arima.msts <- auto.arima(oil_msts.tr[, "barrels"], xreg=oil_msts.tr[, c(2,3,4)])
fc_arima.msts <- forecast(fit_arima.msts, xreg=oil_msts.val[, c(2,3,4)], h=52)
autoplot(fc_arima.msts, h=52)
autoplot(fc_arima.msts, h=52) + autolayer(oil_msts.val[,"barrels"])

checkresiduals(fit_arima.msts)

#Maybe a neural network will be nice
  #no xregs
fit_nn <- nnetar(oil_freq.tr, lambda = 0.5)
autoplot(forecast(fit_nn, h=52))

  #simulattion path for 9 possible future paths
set.seed(2005)
sim <- ts(matrix(0, nrow=30L, ncol=9L), frequency=365.25/7 ,
          start = end(oil_freq.tr)[1L]+(7/365.25))
for(i in seq(9))
  sim[,i] <- simulate(fit_nn, nsim=30L)
autoplot(oil_freq.tr) + autolayer(sim)

fc_nn <- forecast(fit_nn, PI=T, h=52)
autoplot(fc_nn)
autoplot(fc_nn) + autolayer(oil_freq.val)
checkresiduals(fit_nn$residuals)
Box.test(fit_nn$residuals)

  #xregs
fit_nn.xregs <- nnetar(oil_freq.tr, xreg = seasons.tr, lambda = 0.5)
fc_nn.xregs <- forecast(fit_nn.xregs, xreg = seasons.val, PI=T, h=52)
autoplot(fc_nn.xregs)
autoplot(fc_nn.xregs) + autolayer(oil_freq.val)

checkresiduals(fit_nn.xregs$residuals)
Box.test(fit_nn.xregs$residuals)

# "Extreme learning machines"
#fit_elm <- elm(oil_freq.tr, type="lasso")
#fc_elm <- forecast(fit_elm, PI=T, h=52)
#autoplot(fc_elm)

# ELM w/ xreg
#fit_elm.xreg <- elm(oil_freq.tr, xreg=seasons.tr)
#fc_elm.xreg <- forecast(fit_elm.xreg, xreg = seasons, PI=T, h=52)
#autoplot(fc_elm.xreg)

#Combine Models
comb_msts <- (fc_arima.msts[["mean"]] + fc_tbats.msts[["mean"]])/2
autoplot(oil) + autolayer(comb_msts)
comb_fourier <- (fc_comb[["mean"]] + fc_freg[["mean"]] +
                   fc_fper[["mean"]])/3
autoplot(oil) + autolayer(comb_fourier)
comb_season <- (fc_seasons[["mean"]] + fc_freg[["mean"]] +
                  + fc_nn.xregs[["mean"]] + fc_comb[["mean"]])/4
autoplot(oil) + autolayer(comb_season)
comb_nn <- (fc_nn[["mean"]] + fc_nn.xregs[["mean"]])/2
autoplot(oil) + autolayer(comb_nn)
#comb_simp <- (fc_ets[["mean"]] + fc_stlf[["mean"]] + fc_base[["mean"]])/3
#autoplot(oil) + autolayer(comb_simp)

#Compare Models
NAIVE_ac <- accuracy(fc_naive, oil_freq.val)
ETS_ac <- accuracy(fc_ets, oil_freq.val)
STLF_ac <- accuracy(fc_stlf, oil_freq.val)
ARIMA.base_ac <- accuracy(fc_base, oil_freq.val)
ARIMA.freg_ac <- accuracy(fc_freg, oil_freq.val)
ARIMA.fper_ac <- accuracy(fc_fper, oil_freq.val)
ARIMA.sns_ac <- accuracy(fc_seasons, oil_freq.val)
ARIMA.comb_ac <- accuracy(fc_comb, oil_freq.val)
ARIMA.msts_ac <-  accuracy(fc_arima.msts, oil_freq.val)
TBATS_ac <- accuracy(fc_tbats.msts, oil_freq.val)
NNETAR_ac <- accuracy(fc_nn, oil_freq.val)
NNETAR.sns_ac <- accuracy(fc_nn.xregs, oil_freq.val)
#ELM_ac <- accuracy(fc_elm, oil_freq.val)
#ELM.sn_ac <- accuracy(fc_elm.xreg, oil_freq.val)
COMB.fourier_ac <- accuracy(comb_fourier, oil_freq.val)
COMB.msts_ac <- accuracy(comb_msts, oil_freq.val)
COMB.nn_ac <- accuracy(comb_nn, oil_freq.val)
COMB.season_ac <- accuracy(comb_season, oil_freq.val)
#COMB.simp_ac <- accuracy(comb_simp, oil_freq.val)

sort(c(NAIVE_ac = accuracy(fc_naive, oil_freq.val)["Test set","RMSE"],
  ETS_ac = accuracy(fc_ets, oil_freq.val)["Test set","RMSE"],
  STLF_ac = accuracy(fc_stlf, oil_freq.val)["Test set","RMSE"],
  ARIMA.base_ac = accuracy(fc_base, oil_freq.val)["Test set","RMSE"],
  ARIMA.freg_ac = accuracy(fc_freg, oil_freq.val)["Test set","RMSE"],
  ARIMA.fper_ac = accuracy(fc_fper, oil_freq.val)["Test set","RMSE"],
  ARIMA.sns_ac = accuracy(fc_seasons, oil_freq.val)["Test set","RMSE"],
  ARIMA.comb_ac = accuracy(fc_comb, oil_freq.val)["Test set","RMSE"],
  ARIMA.msts_ac =  accuracy(fc_arima.msts, oil_freq.val)["Test set","RMSE"],
  TBATS_ac = accuracy(fc_tbats.msts, oil_freq.val)["Test set","RMSE"],
  NNETAR_ac = accuracy(fc_nn, oil_freq.val)["Test set","RMSE"],
  NNETAR.sns_ac = accuracy(fc_nn.xregs, oil_freq.val)["Test set","RMSE"],
  #ELM_ac = accuracy(fc_elm, oil_freq.val)["Test set","RMSE"],
  #ELM.sn_ac = accuracy(fc_elm.xreg, oil_freq.val)["Test set","RMSE"],
  COMB.fourier_ac = accuracy(comb_fourier, oil_freq.val)["Test set","RMSE"],
  COMB.msts_ac = accuracy(comb_msts, oil_freq.val)["Test set","RMSE"],
  COMB.nn_ac = accuracy(comb_nn, oil_freq.val)["Test set","RMSE"],
  COMB.season_ac = accuracy(comb_season, oil_freq.val)["Test set","RMSE"]
  #COMB.simp_ac = accuracy(comb_simp, oil_freq.val)["Test set","RMSE"])
))

#winner!
autoplot(oil_freq.tr) + autolayer(fc_tbats.msts) + autolayer(oil_freq.val)

#Forecast out - prediction interval far too wide though
fit_for.tbats <- tbats(oil_msts[, "barrels"], seasonal.periods=c(wkly, mthly), model=fit_tbats.msts)
fc_for.tbats <- forecast(fit_for.tbats, h=52)
autoplot(fc_for.tbats, h=52)


#Try the combined season forecast instead - create prediction intervals
autoplot(oil_freq.tr) + autolayer(comb_season, h=52)

tst0 <- cbind(fc_seasons$lower, fc_seasons$upper) * .25
tst1 <- cbind(fc_comb$lower, fc_comb$upper) * .25
tst2 <- cbind(fc_freg$lower, fc_freg$upper) * .25
tst3 <- cbind(fc_nn.xregs$lower, fc_nn.xregs$upper) * .25
combined_tst <- cbind((tst0[,1]+tst1[,1]+tst2[,1]+tst3[,1]),
                      (tst0[,2]+tst1[,2]+tst2[,2]+tst3[,2]),
                      (tst0[,3]+tst1[,3]+tst2[,3]+tst3[,3]),
                      (tst0[,4]+tst1[,4]+tst2[,4]+tst3[,4]))
combin
intnames <- c('Lo 80', 'Hi 80', 'Lo 95', 'Hi 95')
colnames(combined_tst) <- intnames
autoplot(oil_freq.tr) + autoplot(combined_tst[,'Point Forecast'], PI=T)
#Something to try... weighted predictions of multiple models
# Need to use the model weights to forcast out... multiply each model's
# forecast for next 52 by the weights, do same with prediction intervals
require(opera)
comb_seasons.MLpol = cbind(fc_seasons[["mean"]], fc_freg[["mean"]], 
                              fc_nn.xregs[["mean"]], fc_comb[["mean"]])
MLpol0 <- mixture(model = 'MLpol', loss.type = 'square')
weights <- predict(MLpol0, comb_seasons.MLpol, oil_freq.val, type = 'weights')
resp <- predict(MLpol0, comb_seasons.MLpol, oil_freq.val, type = 'response')
model <- predict(MLpol0, comb_seasons.MLpol, oil_freq.val, type = 'model')
fc_op_sns <- ts(predict(MLpol0, comb_seasons.MLpol, oil_freq.val, type = 'response'),
                start = end(oil_freq.tr), frequency = 365.25/7)

autoplot(oil_freq.tr) + autolayer(fc_op_sns) + autolayer(oil_freq.val)
accuracy(fc_op_sns, oil_freq.val)
