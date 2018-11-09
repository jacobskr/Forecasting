require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)
require(dplyr)

#Wikipedia page visits data
data <- fread('webpagevisits.csv')
data <- data[complete.cases(data),]
data <- filter(data, grepl('_en.', data$Page, ignore.case = T))
data <- filter(data, grepl('Main_Page', data$Page, ignore.case = F))
#data <- subset.data.frame(data, data$`7/1/2015`<3000000)
data <- data[,-1]

#Weekend/holiday data
x <- fread('dow_holiday.csv')
x$holiday <- as.factor(as.character(x$holiday))
x$dow <- as.factor(as.character(x$dow))

#group and turn into xts
y <- data.frame(t(colSums(data)))
colnames(y) <- names(data)
y <- data.frame(t(y))
colnames(y) <- "views"
dts = rownames(y)
y['date'] <- dts
y$date <- as.Date(as.character(dts), '%m/%d/%Y')
rownames(y) <- y$date
y <- xts(y[-2], y$date, deltat = 1/7)
autoplot(y)

y.tr <- window(y[450:775])
y.val <- window(y[776:803])
autoplot(y.tr)

# Naive
naive0 <- naive(y.tr, h = length(y.val))
naive0 %>% autoplot(include=150)

#Base Model
fit0 <- auto.arima(y.tr)
summary(fit0)
fit0 %>% forecast(h=length(y.val)) %>% autoplot(include=150)

(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))

# Lets try finding the freqs w/ the highest spectral power densities
p <- periodogram(y.tr)
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:5]
#Well, we can try some of these. 800 and 400 might be too large

#Yep, 800 gave an error because it was bigger than our y.tr
for(i in 1:3) {
  for (j in 1:3){
    z1 <- fourier(ts(y.tr, frequency=7.058824), K=i)
    z2 <- fourier(ts(y.tr, frequency=120), K=j)
    fit <- auto.arima(y.tr, xreg=cbind(z1, z2), seasonal=F)
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit

#forecast with new bestfit
fc <- forecast(bestfit$fit, 
               xreg=cbind(
                 fourier(ts(y, frequency=7.058824), K=bestfit$i, h=60),
                 fourier(ts(y, frequency=120), K=bestfit$j, h=60)))
plot(fc)