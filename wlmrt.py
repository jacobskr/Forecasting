# -*- coding: utf-8 -*-
"""
Created on Wed Nov  7 16:56:59 2018

@author: ejvpaba
"""

import pandas as pd
from pyramid.arima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import periodogram

wlmrt = pd.read_csv('Walmart_Sales.csv', parse_dates=True, index_col='Date')
wlmrt.index = pd.DatetimeIndex(wlmrt.index, freq='W-Fri')

tst = wlmrt['Weekly_Sales']
tst.plot()
