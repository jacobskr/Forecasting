require(data.table)
require(TSA)
require(forecast)
require(xts)
require(fpp2)
require(ggplot2)
require(tidyverse)

#Wikipedia page visits data
data <- fread('webpagevisits.csv')
data <- data[complete.cases(data),]
data <- subset.data.frame(data, data$`7/1/2015`<3000000)
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
y$date <- as.Date(as.character(y$date), '%m/%d/%Y')
y <- xts(y[-2], y$date)
autoplot(y)

