require(data.table)
require(TSA)
require(forecast)
require(fpp2) 
require(ggplot2)
require(nnfor)


#https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=wgfupus2&f=W
dta <- read.csv("US_OIL.csv")
dta2 <- tail(dta, 260)[,2]

seasons <- read.csv("seasons.csv")
seasons <- tail(seasons, 260)
seasons <- seasons[,c(2,3,4)]
seasons.tr <- seasons[1:208,]
seasons.val <- seasons[209:260,]

seasonsfc <- read.csv("seasonsfc.csv")
seasonsfc <- seasonsfc[,c(2,3,4)]

oil_freq <- ts(dta2, frequency = 365.25/7, start=2014-(46/365.25))
oil_freq.tr <- ts(oil_freq[1:208], freq=365.25/7, start=2014-(46/365.25))
oil_freq.val <- ts(oil_freq[(length(oil_freq)-51):length(oil_freq)],
                   freq=365.25/7, 
                   start=end(oil_freq.tr)+(7/365.25))

oil_round <- ts(dta2, freq=52, start = c(2013, 46))
oil_round.tr <- ts(oil_freq[1:208], freq=52, start=c(2013, 46))
oil_round.val <- ts(oil_freq[209:260], freq=52, start=c(2017, 46))

decom_oil <- decompose(oil_round)

#naive model
fc_naive <- naive(oil_freq.tr, h = 52)
Naive <- fc_naive

#ETS Model
fit_ets <- ets(oil_freq.tr)
fc_ets <- forecast(fit_ets, h=52)
ETS <- fc_ets

#stlm Model
fit_stlm <- stlm(oil_freq.tr)
fc_stlm <- forecast(fit_stlm, h=52)
STLM <- fc_stlm

#Base Arima Model
fit_base <- Arima(oil_round.tr, order = c(4,1,1), seasonal = c(1,1,0))
fc_base <- forecast(fit_base, h = 52)
Base_Arima <- fc_base

#Using period set in ts, find # fourier regressors to use to minimize AIC
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  z <- fourier(oil_freq.tr, K=i)
  fit <- auto.arima(oil_freq.tr, xreg=fourier(oil_freq.tr, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- list(aicc=fit$aicc, k=i, fit=fit)
  else break;
}
fc_freg <- forecast(bestfit$fit, xreg=fourier(oil_freq.tr, K=bestfit$k, h=52))
Fourier_Reg <- fc_freg

#Try finding potential multi-seasonality. Use periodogram to find
#frequencies wih the highest spectral power densities
#(3,1,3) was the best
z1 <- fourier(ts(oil_freq.tr, frequency=54), K=3)
z2 <- fourier(ts(oil_freq.tr, frequency=72), K=1)
z3 <- fourier(ts(oil_freq.tr, frequency=30.85714286), K=3)

shinyfour <- Arima(oil_freq.tr, order = c(0,1,1), xreg = cbind(z1,z2,z3))
fc_fper <- forecast(shinyfour, 
                    xreg=cbind(
                      fourier(ts(oil_freq.tr, frequency=54),
                              K=3, h=length(oil_freq.val)),
                      fourier(ts(oil_freq.tr, frequency=72), K=1,
                              h=length(oil_freq.val)),
                      fourier(ts(oil_freq.tr, frequency=30.85714286),
                              K=3, h=length(oil_freq.val))))
Fourier_Period <- fc_fper

# We can check if a seasons regressor will make forecast better
fit_seasons <- Arima(oil_freq.tr, order = c(2,1,3), seasonal = c(1,0,0), xreg = seasons.tr)
fc_seasons <- forecast(fit_seasons, xreg = seasons.val, h=52)
Base_Arima_Sns <- fc_seasons

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

fc_comb <- forecast(bestfit2$fit, xreg=cbind(fourier(oil_freq.tr, K=bestfit2$k, h=52), seasons.val))
Fourier_Reg_Sns <- fc_comb

#Multiseasonal dataset
wkly <- 365.25 / 7
mthly <- wkly / 12
qtrly <- wkly / 4
oil_msts <- data.frame(dta2)
oil_msts[,c(2,3,4)] <- seasons
oil_msts <- msts(oil_msts, seasonal.periods=c(wkly, mthly, qtrly),
                 ts.frequency = wkly, start=2014-(46/365.25))
oil_msts.tr <-msts(oil_msts[1:208,], seasonal.periods=c(wkly, mthly, qtrly), 
                   ts.frequency = wkly, start=2014-(46/365.25))
oil_msts.val <- msts(oil_msts[(length(oil_freq)-51):length(oil_freq),],
                     seasonal.periods=c(wkly, mthly, qtrly),
                     ts.frequency = wkly,
                     start=end(oil_msts.tr)+(7/365.25))
colnames <- c("barrels", "winter", "spring", "summer")
colnames(oil_msts.tr) <- colnames
colnames(oil_msts.val) <- colnames
colnames(oil_msts) <- colnames


#tbats using msts
fit_tbats.msts <- tbats(oil_msts.tr[, "barrels"], seasonal.periods=c(wkly, mthly, qtrly),
                        use.parallel = TRUE)
fc_tbats.msts <- forecast(fit_tbats.msts, h=52)
MSTS_TBATS <- fc_tbats.msts

#Try to use covariates with msts
fit_arima.msts <- Arima(oil_msts.tr[,"barrels"], order = c(2,1,3), seasonal = c(1,0,0),
                        xreg = oil_msts.tr[, c(2,3,4)])
fc_arima.msts <- forecast(fit_arima.msts, xreg=oil_msts.val[, c(2,3,4)], h=52)
MSTS_Sns <- fc_arima.msts

#Maybe a neural network will be nice
#no xregs
fit_nn <- nnetar(oil_freq.tr, lambda = 0.5)
fc_nn <- forecast(fit_nn, PI=T, h=52)
NN <- fc_nn

#xregs
fit_nn.xregs <- nnetar(oil_freq.tr, xreg = seasons.tr, lambda = 0.5)
fc_nn.xregs <- forecast(fit_nn.xregs, xreg = seasons.val, PI=T, h=52)
NN_Sns <- fc_nn.xregs

#Combine Models
comb_msts <- (fc_arima.msts[["mean"]] + fc_tbats.msts[["mean"]])/2
Com_MSTS <- comb_msts
comb_fourier <- (fc_comb[["mean"]] + fc_freg[["mean"]] +
                   fc_fper[["mean"]])/3
Com_Fourier <- comb_fourier
comb_season <- (fc_seasons[["mean"]] + fc_freg[["mean"]] +
                  + fc_nn.xregs[["mean"]] + fc_comb[["mean"]])/4
Com_Sns <- comb_season
comb_nn <- (fc_nn[["mean"]] + fc_nn.xregs[["mean"]])/2
Com_NN <- comb_nn


#Find best combo
names <- c('fc_naive', 'fc_ets', 'fc_stlm', 'fc_base', 
           'fc_freg', 'fc_fper', 'fc_seasons', 'fc_comb', 
           'fc_arima.msts', 'fc_tbats.msts', 'fc_nn', 'fc_nn.xregs')
model_list <- list(fc_naive[["mean"]], fc_ets[["mean"]], fc_stlm[["mean"]],
                   fc_base[["mean"]], fc_freg[["mean"]], fc_fper[["mean"]], 
                   fc_seasons[["mean"]], fc_comb[["mean"]], fc_arima.msts[["mean"]],
                   fc_tbats.msts[["mean"]], fc_nn[["mean"]], fc_nn.xregs[["mean"]])

bestRMSE <- Inf
for (i in 2:length(model_list)) {
  cmb = combn(model_list, m=i)
  for (c in 1:ncol(cmb)) {
    cmbdf <- data.frame(cmb[,c])
    avg <- rowSums(cmbdf)/i
    ac <- accuracy(avg, oil_freq.val)["Test set","RMSE"]
    if (ac < bestRMSE) {
      bestRMSE <- ac
      bestmodel <- list(RMSE=bestRMSE, i=i, c=c, means=avg)
    }
  }
}
combn(names, bestmodel$i)[,bestmodel$c]

comb_best <- ts(bestmodel$means, frequency = 365.25/7, start = start(oil_freq.val))
Com_Best <- comb_best

#Compare Models
NAIVE_ac <- accuracy(fc_naive, oil_freq.val)
ETS_ac <- accuracy(fc_ets, oil_freq.val)
stlm_ac <- accuracy(fc_stlm, oil_freq.val)
ARIMA.base_ac <- accuracy(fc_base, oil_freq.val)
ARIMA.freg_ac <- accuracy(fc_freg, oil_freq.val)
ARIMA.fper_ac <- accuracy(fc_fper, oil_freq.val)
ARIMA.sns_ac <- accuracy(fc_seasons, oil_freq.val)
ARIMA.comb_ac <- accuracy(fc_comb, oil_freq.val)
ARIMA.msts_ac <-  accuracy(fc_arima.msts, oil_freq.val)
TBATS_ac <- accuracy(fc_tbats.msts, oil_freq.val)
NNETAR_ac <- accuracy(fc_nn, oil_freq.val)
NNETAR.sns_ac <- accuracy(fc_nn.xregs, oil_freq.val)
COMB.fourier_ac <- accuracy(comb_fourier, oil_freq.val)
COMB.msts_ac <- accuracy(comb_msts, oil_freq.val)
COMB.nn_ac <- accuracy(comb_nn, oil_freq.val)
COMB.season_ac <- accuracy(comb_season, oil_freq.val)
COMB.best_ac = accuracy(comb_best, oil_freq.val)


colrmse <- c('Naive',
           'ETS', 
           'STLM',
           'Base_Arima',
           'Base_Arima_Sns',
           'Fourier_Reg',
           'Fourier_Reg_Sns',
           'Fourier_Period', 
           'MSTS_Sns',
           'TBATS',
           'NN',
           'NN_Sns',
           'Com_MSTS',
           'Com_Fourier',
           'Com_Sns',
           'Com_NN',
           'Com_Best')

RMSEs <- data.frame(c(NAIVE_ac = accuracy(fc_naive, oil_freq.val)["Test set","RMSE"],
                 ETS_ac = accuracy(fc_ets, oil_freq.val)["Test set","RMSE"],
                 stlm_ac = accuracy(fc_stlm, oil_freq.val)["Test set","RMSE"],
                 ARIMA.base_ac = accuracy(fc_base, oil_freq.val)["Test set","RMSE"],
                 ARIMA.freg_ac = accuracy(fc_freg, oil_freq.val)["Test set","RMSE"],
                 ARIMA.fper_ac = accuracy(fc_fper, oil_freq.val)["Test set","RMSE"],
                 ARIMA.sns_ac = accuracy(fc_seasons, oil_freq.val)["Test set","RMSE"],
                 ARIMA.comb_ac = accuracy(fc_comb, oil_freq.val)["Test set","RMSE"],
                 ARIMA.msts_ac =  accuracy(fc_arima.msts, oil_freq.val)["Test set","RMSE"],
                 TBATS_ac = accuracy(fc_tbats.msts, oil_freq.val)["Test set","RMSE"],
                 NNETAR_ac = accuracy(fc_nn, oil_freq.val)["Test set","RMSE"],
                 NNETAR.sns_ac = accuracy(fc_nn.xregs, oil_freq.val)["Test set","RMSE"],
                 COMB.fourier_ac = accuracy(comb_fourier, oil_freq.val)["Test set","RMSE"],
                 COMB.msts_ac = accuracy(comb_msts, oil_freq.val)["Test set","RMSE"],
                 COMB.nn_ac = accuracy(comb_nn, oil_freq.val)["Test set","RMSE"],
                 COMB.season_ac = accuracy(comb_season, oil_freq.val)["Test set","RMSE"],
                 COMB.best_ac = accuracy(comb_best, oil_freq.val)["Test set","RMSE"]))
colnames(RMSEs) <- 'RMSE'
RMSEs['Model'] <- colrmse
RMSEs[order(RMSEs$RMSE),]

#Create Comb Best forecast and prediction intervals for validation
tst0 <- data.frame((cbind(fc_stlm$lower, fc_stlm$upper) * (1/6)))
tst1 <-  data.frame((cbind(fc_base$lower, fc_base$upper) * (1/6)))
tst2 <-  data.frame((cbind(fc_fper$lower, fc_fper$upper) * (1/6)))
tst3 <-  data.frame((cbind(fc_seasons$lower, fc_seasons$upper) * (1/6)))
tst4 <-  data.frame((cbind(fc_nn.xregs$lower, fc_nn.xregs$upper) * (1/6)))
tst5 <-  data.frame((cbind(fc_arima.msts$lower, fc_arima.msts$upper) * (1/6)))
combined_tst <- cbind((tst0[,1]+tst1[,1]+tst2[,1]+tst3[,1]+tst4[,1]+tst5[,1]),
                      (tst0[,2]+tst1[,2]+tst2[,2]+tst3[,2]+tst4[,2]+tst5[,2]),
                      (tst0[,3]+tst1[,3]+tst2[,3]+tst3[,3]+tst4[,3]+tst5[,3]),
                      (tst0[,4]+tst1[,4]+tst2[,4]+tst3[,4]+tst4[,4]+tst5[,4]))

int <- list()
int$mean <- comb_best
int$interval <- combined_tst
colnames(int$interval) <- c('Lo 80', 'Lo 95', 'Hi 80', 'Hi 95')

#Create Comb Best forecast and prediction intervals for next 6 months
fit_stlm_for <- stlm(oil_freq, model=fit_stlm)
fc_stlm_for <- forecast(fit_stlm_for, h=26)

fit_base_for <- Arima(oil_freq, model = fit_base)
fc_base_for <- forecast(fit_base_for, h=26)

fit_fper_for <- Arima(oil_freq,
                      model = shinyfour,
                      xreg=cbind(
                        fourier(ts(oil_freq, frequency=54), K=3),
                        fourier(ts(oil_freq, frequency=72), K=1),
                        fourier(ts(oil_freq, frequency=30.85714286), K=3)))

fc_fper_for <- forecast(fit_fper_for, h=26,
                        xreg=cbind(
                          fourier(ts(oil_freq, frequency=54), K=3, h=26),
                          fourier(ts(oil_freq, frequency=72), K=1, h=26),
                          fourier(ts(oil_freq, frequency=30.85714286), K=3, h=26)))

fit_seasons_for <- Arima(oil_freq, xreg = seasons, model = fit_seasons)
fc_seasons_for <- forecast(fit_seasons_for, xreg=seasonsfc, h=26)

fit_arima.msts_for <- Arima(oil_msts[, "barrels"], xreg=oil_msts[, c(2,3,4)], model = fit_arima.msts)
fc_arima.msts_for <- forecast(fit_arima.msts_for, xreg=seasonsfc, h=26)

fit_nn.xregs_for <- nnetar(oil_freq, xreg = seasons)
fc_nn.xregs_for <- forecast(fit_nn.xregs_for, xreg = seasonsfc, PI=T, h=26)

comb_best_for <- (fc_stlm_for[["mean"]] + fc_base_for[["mean"]] +
                    fc_fper_for[["mean"]] + fc_seasons_for[["mean"]]+ 
                    fc_arima.msts_for[["mean"]] + fc_nn.xregs_for[["mean"]])/6


#Confidence intervals for forward forecast
tst.for0 <- cbind(fc_stlm_for$lower, fc_stlm_for$upper) * (1/6)
tst.for1 <- cbind(fc_base_for$lower, fc_base_for$upper) * (1/6)
tst.for2 <- cbind(fc_fper_for$lower, fc_fper_for$upper) * (1/6)
tst.for3 <- cbind(fc_seasons_for$lower, fc_seasons_for$upper) * (1/6)
tst.for4 <- cbind(fc_arima.msts_for$lower, fc_arima.msts_for$upper) * (1/6)
tst.for5 <- cbind(fc_nn.xregs_for$lower, fc_nn.xregs_for$upper) * (1/6)
combined_tst.for <- cbind((tst.for0[,1]+tst.for1[,1]+tst.for2[,1]+tst.for3[,1]+tst.for4[,1]+tst.for5[,1]),
                          (tst.for0[,2]+tst.for1[,2]+tst.for2[,2]+tst.for3[,2]+tst.for4[,2]+tst.for5[,2]),
                          (tst.for0[,3]+tst.for1[,3]+tst.for2[,3]+tst.for3[,3]+tst.for4[,3]+tst.for5[,3]),
                          (tst.for0[,4]+tst.for1[,4]+tst.for2[,4]+tst.for3[,4]+tst.for4[,4]+tst.for5[,4]))
int.for <- list()
int.for$mean <- comb_best_for
int.for$interval <- combined_tst.for
colnames(int.for$interval) <- c('Lo 80', 'Lo 95', 'Hi 80', 'Hi 95')
