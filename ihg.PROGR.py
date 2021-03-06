# -*- coding: utf-8 -*-
"""ihg.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1QQXMnI4vmvyTjdMgXQ2yC1FEYIO4kkW4
"""

from google.colab import files
files.upload()

import numpy as np
import pandas as pd
import statsmodels.api as sm

import matplotlib.pyplot as plt
# %matplotlib inline

df = pd.ExcelFile('output.xlsx')

df1 = pd.read_excel(df)

df1.head(13)

df1.set_index('month_year', inplace=True)

df1.tail()

#Creating train and test set 
#Index 10392 marks the end of October 2013 
train=df1[0:36] 
test=df1[36:]

train.tail()

#Plotting data
train.Total_Bookings.plot(figsize=(15,8), title= 'Daily Ridership', fontsize=14)
test.Total_Bookings.plot(figsize=(15,8), title= 'Daily Ridership', fontsize=14)
plt.show()

#naive
dd= np.asarray(train.Total_Bookings)
y_hat = test.copy()
y_hat['naive'] = dd[len(dd)-1]
plt.figure(figsize=(12,8))
plt.plot(train.index, train['Total_Bookings'], label='Train')
plt.plot(test.index,test['Total_Bookings'], label='Test')
plt.plot(y_hat.index,y_hat['naive'], label='Naive Forecast')
plt.legend(loc='best')
plt.title("Naive Forecast")
plt.show()

from sklearn.metrics import mean_squared_error
from math import sqrt
rms = sqrt(mean_squared_error(test.Total_Bookings, y_hat.naive))
print(rms)

y_hat_avg = test.copy()
fit1 = ExponentialSmoothing(np.asarray(train['Total_Bookings']) ,seasonal_periods=8 ,trend='add', seasonal='add',).fit()
y_hat_avg['Holt_Winter'] = fit1.forecast(len(test))
plt.figure(figsize=(16,8))
plt.plot( train['Total_Bookings'], label='Train')
plt.plot(test['Total_Bookings'], label='Test')
plt.plot(y_hat_avg['Holt_Winter'], label='Holt_Winter')
plt.legend(loc='best')
plt.show()

rms = sqrt(mean_squared_error(test.Total_Bookings, y_hat_avg.Holt_Winter))
print(rms)

y_hat_avg = test.copy()
fit1 = sm.tsa.statespace.SARIMAX(train.Total_Bookings, order=(2, 1, 4),seasonal_order=(0,1,1,7),enforce_invertibility=False).fit()
y_hat_avg['SARIMA'] = fit1.predict(start="2019-01", end="2019-03", dynamic=True)
plt.figure(figsize=(16,8))
plt.plot( train['Total_Bookings'], label='Train')
plt.plot(test['Total_Bookings'], label='Test')
plt.plot(y_hat_avg['SARIMA'], label='SARIMA')
plt.legend(loc='best')
plt.show()

df1.describe().transpose()



df1.plot(figsize=(16, 8))
plt.show()

df1.rolling(12).mean().plot(label='12 Month Rolling Mean')
df1.rolling(12).std().plot(label='12 Month Rolling Std')
df1.plot()
plt.legend()

df1.rolling(12).mean().plot(label='12 Month Rolling Mean')
df1.plot()
plt.legend()

from statsmodels.tsa.seasonal import seasonal_decompose
decomposition = seasonal_decompose(df1['Total_Bookings'], freq=12)  
fig = plt.figure()  
fig = decomposition.plot()  
fig.set_size_inches(15, 8)

from statsmodels.tsa.stattools import adfuller

result = adfuller(df1['Total_Bookings'])

print('Augmented Dickey-Fuller Test:')
labels = ['ADF Test Statistic','p-value','#Lags Used','Number of Observations Used']

for value,label in zip(result,labels):
    print(label+' : '+str(value) )
    
if result[1] <= 0.05:
    print("strong evidence against the null hypothesis, reject the null hypothesis. Data has no unit root and is stationary")
else:
    print("weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary ")

# Store in a function for later use!
def adf_check(time_series):
    """
    Pass in a time series, returns ADF report
    """
    result = adfuller(time_series)
    print('Augmented Dickey-Fuller Test:')
    labels = ['ADF Test Statistic','p-value','#Lags Used','Number of Observations Used']

    for value,label in zip(result,labels):
        print(label+' : '+str(value) )
    
    if result[1] <= 0.05:
        print("strong evidence against the null hypothesis, reject the null hypothesis. Data has no unit root and is stationary")
    else:
        print("weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary ")

df1['First']=df1.Total_Bookings-df1.Total_Bookings.shift(1)

adf_check(df1['Total_Bookings'].dropna())

adf_check(df1['First'].dropna())

df1['First'].plot()

#df['Bookings Second Difference'] = df['First'] - df['First'].shift(1)
df1['Second']=df1.First-df1.First.shift(1)

df1['Second'].plot()

df1.head()

adf_check(df1['Second'].dropna())

df1[' Seasonal First diff']=df1.Total_Bookings-df1.Total_Bookings.shift(12)

df1[' Seasonal First diff'].plot()

adf_check(df1[' Seasonal First diff'].dropna())

from statsmodels.graphics.tsaplots import plot_acf,plot_pacf

# Duplicate plots
# Check out: https://stackoverflow.com/questions/21788593/statsmodels-duplicate-charts
# https://github.com/statsmodels/statsmodels/issues/1265
fig_first = plot_acf(df1["First"].dropna())

fig_seasonal_first = plot_acf(df1[" Seasonal First diff"].dropna())

from pandas.plotting import autocorrelation_plot
autocorrelation_plot(df1[' Seasonal First diff'].dropna())

result = plot_pacf(df1[" Seasonal First diff"].dropna())

plot_acf(df1[" Seasonal First diff"].dropna())
plot_pacf(df1[" Seasonal First diff"].dropna())

# For non-seasonal data
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX



# We have seasonal data!
model = SARIMAX(df3.Total_Bookings,order=(1,0,1),seasonal_order=(0,0,0,1))
results = model.fit()
print(results.summary())

pred = results.get_prediction(start=pd.to_datetime('2019-01'),end=pd.to_datetime('2019-03'), dynamic=True)
pred_ci = pred.conf_int()

s_forecasted = pred.predicted_mean
s_forecasted=pd.DataFrame(s_forecasted)
s_forecasted.columns = ['pred_bookings']

s_truth = df1['2019-01':]

df1

s_truth

s_forecasted

def forecast_accuracy(forecast, actual):
    mape = np.mean(np.abs(forecast - actual)/np.abs(actual))  # MAPE
    me = np.mean(forecast - actual)             # ME
    mae = np.mean(np.abs(forecast - actual))    # MAE
    mpe = np.mean((forecast - actual)/actual)   # MPE
    rmse = np.mean((forecast - actual)**2)**.5  # RMSE
    corr = np.corrcoef(forecast, actual)[0,1]   # corr
    mins = np.amin(np.hstack([forecast[:,None], 
                              actual[:,None]]), axis=1)
    maxs = np.amax(np.hstack([forecast[:,None], 
                              actual[:,None]]), axis=1)
    minmax = 1 - np.mean(mins/maxs)             # minmax
#    acf1 = acf(fc-test)[1] 
#    acf1=acf(s_forecasted['pred_bookings']-s_truth['Total_Bookings'])                     # ACF1
    return({'mape':mape, 'me':me, 'mae': mae, 
            'mpe': mpe, 'rmse':rmse, 
            'corr':corr, 'minmax':minmax})

forecast_accuracy(s_forecasted['pred_bookings'],s_truth['Total_Bookings'])

df1.info()



df3.tail()

df3=df1.Total_Bookings[:'2018-12']
df3=pd.DataFrame(df3)

!pip install pmdarima

from statsmodels.tsa.arima_model import ARIMA
import pmdarima as pm

model = pm.auto_arima(df3.Total_Bookings, start_p=1, start_q=1,
                      test='adf',       # use adftest to find optimal 'd'
                      max_p=3, max_q=3, # maximum p and q
                      m=1,              # frequency of series
                      d=0,           # let model determine 'd'
                      seasonal=True,   # No Seasonality
                      start_P=0, 
                      D=0, 
                      trace=True,
                      error_action='ignore',  
                      suppress_warnings=True, 
                      stepwise=True)

print(model.summary())

# HWES example
from statsmodels.tsa.holtwinters import ExponentialSmoothing
statsmodels.tsa.holtwinters.HoltWintersResults
model = ExponentialSmoothing(df1.Total_Bookings)
model_fit = model.fit()
# make prediction
yhat = model_fit.predict(len(df1.Total_Bookings), len(df1.Total_Bookings))
print(yhat)

print(model_fit.summary())

df1.tail()

results.resid.plot()

results.resid.plot(kind='kde')

df1['forecast'] = results.predict(dynamic= True)  
df1[['Total_Bookings','forecast']].plot(figsize=(12,8))

df1

