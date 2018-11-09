# -*- coding: utf-8 -*-
"""
Created on Wed Nov  7 16:56:59 2018

@author: ejvpaba
"""

import pandas as pd
from pyramid.arima import auto_arima, nsdiffs, ndiffs
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.stattools import adfuller, acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import periodogram
%matplotlib inline
from matplotlib.pylab import rcParams


rcParams['figure.figsize'] = 15, 6

wlmrt = pd.read_csv('Walmart_Sales.csv', parse_dates=True, index_col='Date')
wlmrt.index = pd.DatetimeIndex(wlmrt.index, freq='W-Fri')
wlmrt.Weekly_Sales.plot()

holicat = []
for i in range(0, len(wlmrt)-1):
    if wlmrt.IsHoliday.iloc[i] == True:
       holicat.append(2)
    elif wlmrt.IsHoliday.iloc[i] == False:
        if wlmrt.IsHoliday.iloc[i+1] == True:
            holicat.append(1)
        else:
            holicat.append(0)
holicat.append(0)
wlmrt['holicat'] = holicat
wlmrt['holicat'] = wlmrt.holicat.astype('category')

dummy1 = []
dummy2 = []
for i in range(0, len(wlmrt)):
    if wlmrt.holicat.iloc[i] == 2:
        dummy1.append(1)
        dummy2.append(0)
    elif wlmrt.holicat.iloc[i] == 1:
        dummy1.append(0)
        dummy2.append(1)
    else:
        dummy1.append(0)
        dummy2.append(1)
wlmrt['dummy1'] = dummy1
wlmrt['dummy2'] = dummy2

wlmrt_train = wlmrt.iloc[0:-74]
wlmrt_val = wlmrt.iloc[-74:-39]
wlmrt_test = wlmrt.iloc[-39:]

#Dickey Fuller test and moving average/STD
def test_stationarity(timeseries):
    
    #Determing rolling statistics
    rolmean = timeseries.rolling(window=12).mean()
    rolstd = timeseries.rolling(window=12).std()

    #Plot rolling statistics:
    orig = plt.plot(timeseries, color='blue',label='Original')
    mean = plt.plot(rolmean, color='red', label='Rolling Mean')
    std = plt.plot(rolstd, color='black', label = 'Rolling Std')
    plt.legend(loc='best')
    plt.title('Rolling Mean & Standard Deviation')
    plt.show(block=False)
    
    #Perform Dickey-Fuller test:
    print('Results of Dickey-Fuller Test:')
    dftest = adfuller(timeseries, autolag='AIC')
    dfoutput = pd.Series(dftest[0:4], index=['Test Statistic','p-value','#Lags Used','Number of Observations Used'])
    for key,value in dftest[4].items():
        dfoutput['Critical Value (%s)'%key] = value
    print(dfoutput)    


test_stationarity(wlmrt_train.Weekly_Sales)


#First test passed, but can try more
wlmrt_train_diff = wlmrt_train.Weekly_Sales - wlmrt_train.Weekly_Sales.shift()
plt.plot(wlmrt_train_diff)

wlmrt_train_diff.dropna(inplace=True)
test_stationarity(wlmrt_train_diff)

#Looks better, but not necessary, can also use ndiffs, nsdiffs:
ndiff = ndiffs(wlmrt_train.Weekly_Sales)
nsdiff = nsdiffs(wlmrt_train.Weekly_Sales, m=52)


#ACF and PACF
lag_acf = acf(wlmrt_train.Weekly_Sales, nlags=20)
lag_pacf = pacf(wlmrt_train.Weekly_Sales, nlags=20, method='ols')

plot_acf(wlmrt_train.Weekly_Sales, lags=25)
plot_pacf(wlmrt_train.Weekly_Sales, lags=25)


def auto_arima_checks(data, m_list, d_list, D_list, t_list=['c'], exog=None):
    best_AIC = float('inf')
    #t_list = ['c','t','ct']
    for m in m_list:
        for d in d_list:
            for D in D_list:
                for trend in t_list:
                    try:
                        mod = auto_arima(data, start_p=1, start_q=1,
                                   max_p=3, max_q=3, m=m,
                                   start_P=0, start_Q=0,
                                   max_P=3, max_Q=3,
                                   seasonal=True, d=d,
                                   D=D, trace=True,
                                   trend=trend,
                                   exogenous=exog,
                                   error_action='ignore',  
                                   suppress_warnings=True, 
                                   stepwise=True)
                        AIC = mod.aic()
                        if AIC < best_AIC:
                            best_AIC = AIC
                            stepwise_model = mod
                    except AttributeError:
                        continue
    return stepwise_model

#check different trends through different arima models
arima_terms = auto_arima_checks(wlmrt_train.Weekly_Sales, [52], [0,1], [0,1])
#m0t = auto_arima_checks(wlmrt_train.Weekly_Sales, [52], [0,1], [0,1], t_list=['t'])
#m0ct = auto_arima_checks(wlmrt_train.Weekly_Sales, [52], [0,1], [0,1]), t_list=['ct']
m0c = SARIMAX(wlmrt_train.Weekly_Sales, order=arima_terms.order,
             seasonal_order=arima_terms.seasonal_order,
             trend=arima_terms.trend)

#Same as above with exog Holiday Week bool
n_obs = list(range(1,len(wlmrt_train) + 1))
holiday = list(zip(n_obs, wlmrt_train.IsHoliday))
holi_terms = auto_arima_checks(wlmrt_train.Weekly_Sales, [52], [0,1], [0,1])
m0c_holi = SARIMAX(wlmrt_train.Weekly_Sales, order=holi_terms.order,
             seasonal_order=holi_terms.seasonal_order,
             trend=holi_terms.trend, exogenous=wlmrt_train.IsHoliday)

#Looks like assigning week before holiday would help too
holicat = list(zip(n_obs, wlmrt.dummy1, wlmrt.dummy2))
holicat_terms = auto_arima_checks(wlmrt_train.Weekly_Sales, [52], [0,1], [0,1], exog=holicat)
m0c_holicat = SARIMAX(wlmrt_train.Weekly_Sales, order=holicat_terms.order,
             seasonal_order=holicat_terms.seasonal_order,
             trend=holicat_terms.trend, exogenous=holicat)



#model m0c graphics
m0c_res = m0c.fit()
m0c_res.plot_diagnostics()
pred = m0c_res.get_prediction(start=wlmrt_val.index[0], end=wlmrt_val.index[-1],
                             dynamic=False)
pred_ci = pred.conf_int()

ax = wlmrt.Weekly_Sales.plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#model m0c_holi graphics
m0c_holi_res = m0c_holi.fit()
m0c_holi_res.plot_diagnostics()
pred = m0c_holi_res.get_prediction(start=wlmrt_val.index[0], end=wlmrt_val.index[-1],
                             dynamic=False)
pred_ci = pred.conf_int()

ax = wlmrt.Weekly_Sales.plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#model m0c_holicat graphics
m0c_holicat_res = m0c_holicat.fit()
m0c_holicat_res.plot_diagnostics()
pred = m0c_holicat_res.get_prediction(start=wlmrt_val.index[0], end=wlmrt_val.index[-1],
                             dynamic=False)
pred_ci = pred.conf_int()

ax = wlmrt.Weekly_Sales.plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()






train_pgram = periodogram(wlmrt_train.Weekly_Sales)
train_pgram = pd.DataFrame({'freq':train_pgram[0], 'pxx':train_pgram[1]})
train_pgram = train_pgram.set_index('freq', drop=True)
train_pgram['period'] = 1 / train_pgram.index
train_pgram = train_pgram.sort_values('pxx', ascending=False)
train_toptwo = train_pgram[['period', 'pxx']].iloc[0:2]



train_firstper = train.cnt
train_firstper.index = pd.DatetimeIndex(train_firstper.index, freq=train_toptwo.period.iloc[0])








