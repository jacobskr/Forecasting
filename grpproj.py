# -*- coding: utf-8 -*-
"""
Created on Tue Nov  6 13:24:13 2018

@author: ejvpaba
"""
import pandas as pd
from pyramid.arima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import periodogram

df = pd.read_csv('BikeSharingDaily.csv', parse_dates=True, index_col='dteday')
df = df[['cnt', 'holiday', 'weekday', 'workingday', 'weathersit']]
df.index = pd.DatetimeIndex(df.index, freq='D')
df.cnt.plot()

train = df.loc[:'2012-10-31']
test = df.loc['2012-11-01':]

numlist = list(range(0,len(train)))
numlist_test = list(range(0,len(test)))

holi = list(train.holiday)
holiday = list(zip(numlist, holi))
holiday = np.array(holiday)
holi_test = list(test.holiday)
holiday_test = list(zip(numlist, holi_test))


working = list(train.workingday)
workingday = list(zip(numlist, working))
working_test = list(test.workingday)
workingday_test = list(zip(numlist, working_test))

working_holiday = np.column_stack([numlist, working, holi])
working_holiday_test = np.column_stack([numlist_test,
                                        working_test,
                                        holi_test])


stepwise_model = auto_arima(train.cnt, start_p=1, start_q=1,
                           max_p=10, max_q=10, m=12,
                           start_P=0, seasonal=True,
                           d=1, D=1, trace=True,
                           error_action='ignore',  
                           suppress_warnings=True, 
                           stepwise=True)
print(stepwise_model.aic())

exog_model = auto_arima(train.cnt, start_p=1, start_q=1,
                           max_p=10, max_q=10, m=12,
                           start_P=0, seasonal=True,
                           d=1, D=1, trace=True,
                           exogenous=holiday,
                           error_action='ignore',  
                           suppress_warnings=True, 
                           stepwise=True)

exog_model2 = auto_arima(train.cnt, start_p=1, start_q=1,
                           max_p=10, max_q=10, m=12,
                           start_P=0, seasonal=True,
                           d=1, D=1, trace=True,
                           exogenous=workingday,
                           error_action='ignore',  
                           suppress_warnings=True, 
                           stepwise=True)

exog_model3 = auto_arima(train.cnt, start_p=1, start_q=1,
                           max_p=10, max_q=10, m=12,
                           start_P=0, seasonal=True,
                           d=1, D=1, trace=True,
                           exogenous=working_holiday,
                           error_action='ignore',  
                           suppress_warnings=True, 
                           stepwise=True)



def auto_arima_checks(data, m_list, d_list, D_list, exog=None):
    best_AIC = float('inf')
    tlist = ['c','t','ct']
    for m in m_list:
        for d in d_list:
            for D in D_list:
                for trend in tlist:
<<<<<<< HEAD
                    mod = auto_arima(data, start_p=1, start_q=1,
                               max_p=4, max_q=4, m=m,
                               start_P=0, start_Q=0,
                               max_P=4, max_Q=4,
                               seasonal=True, d=d,
                               D=D, trace=True,
                               trend=trend,
                               exogenous=exog,
                               disp=False,
                               error_action='ignore',  
                               suppress_warnings=True, 
                               stepwise=True)
                    AIC = mod.aic()
                    if AIC < best_AIC:
                        best_AIC = AIC
                        stepwise_model = mod
    return stepwise_model


m0_test = auto_arima_checks(train.cnt, [1, 7, 12], [0, 1, 2], [0, 1, 2])
m0 = SARIMAX(train.cnt, order=(4, 2, 0), seasonal_order=(5, 2, 1, 12),
             trend='ct')
=======
                    try:
                        mod = auto_arima(data, start_p=1, start_q=1,
                                   max_p=4, max_q=4, m=m,
                                   start_P=0, start_Q=0,
                                   max_P=4, max_Q=4,
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


auto_arima_checks(train.cnt, [7, 365.25], [0, 1, 2], [0, 1, 2])
m0 = SARIMAX(train.cnt, order=stepwise_model.order,
             seasonal_order=stepwise_model.seasonal_order,
             trend=stepwise_model.trend)
>>>>>>> 436d1475e057daf96ca87ab749d45b748b0dcc13

m1_test = auto_arima_checks(train.cnt, [1, 7, 12], [0, 1, 2], [0, 1, 2], exog = holiday)
m1 = SARIMAX(train.cnt, exog=holiday, order=stepwise_model.order,
             seasonal_order=stepwise_model.seasonal_order,
             trend=stepwise_model.trend)

m2_test = auto_arima_checks(train.cnt, [1, 7, 12], [0, 1, 2], [0, 1, 2], exog = workingday)
m2 = SARIMAX(train.cnt, exog=workingday, order=stepwise_model.order,
             seasonal_order=stepwise_model.seasonal_order,
             trend=stepwise_model.trend)

auto_arima_checks(train.cnt, [1, 7, 12], [0, 1, 2], [0, 1, 2], exog = working_holiday)
m3 = SARIMAX(train.cnt, exog=working_holiday, order=stepwise_model.order,
             seasonal_order=stepwise_model.seasonal_order,
             trend=stepwise_model.trend)


#m0 graphics
m0_res = m0.fit()
m0_res.plot_diagnostics()
pred = m0_res.get_prediction(start=test.index[0], end=test.index[-1],
                             dynamic=False)
pred_ci = pred.conf_int()

ax = df.loc['2012-07-01':]['cnt'].plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#m1 graphics
m1_res = m1.fit()
m1_res.plot_diagnostics()
pred = m1_res.get_prediction(start=test.index[0], end=test.index[-1],
                             exog=holiday_test, dynamic=False)
pred_ci = pred.conf_int()

ax = df.loc['2012-07-01':]['cnt'].plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#m2 graphics
m2_res = m2.fit()
m2_res.plot_diagnostics()
pred = m2_res.get_prediction(start=test.index[0], end=test.index[-1],
                             exog=workingday_test, dynamic=False)
pred_ci = pred.conf_int()

ax = df.loc['2012-07-01':]['cnt'].plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#m3 graphics
m3_res = m3.fit()
m3_res.plot_diagnostics()
pred = m3_res.get_prediction(start=test.index[0], end=test.index[-1],
                             exog=working_holiday_test, dynamic=False)
pred_ci = pred.conf_int()

ax = df.loc['2012-07-01':]['cnt'].plot(label='Observed')
pred.predicted_mean.plot(ax=ax, label='One-step Ahead Forecast', alpha=.7)

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)
ax.set_xlabel('Date')
ax.set_ylabel('Usage')
plt.legend()
plt.show()

#train_fft = np.fft.fft(train.cnt)
train_pgram = periodogram(train.cnt)
train_pgram = pd.DataFrame({'freq':train_pgram[0], 'pxx':train_pgram[1]})
train_pgram = train_pgram.set_index('freq', drop=True)
train_pgram['period'] = 1 / train_pgram.index
train_pgram = train_pgram.sort_values('pxx', ascending=False)
train_toptwo = train_pgram[['period', 'pxx']].iloc[0:2]

train_firstper = train.cnt
train_firstper.index = pd.DatetimeIndex(train_firstper.index, freq=train_toptwo.period.iloc[0])

SARIMAX(train.cnt, )

