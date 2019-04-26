# -*- coding: utf-8 -*-
"""
Created on Thu Apr 25 13:18:30 2019

@author: Aniruddha
"""

from dateutil.parser import parse 
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
plt.rcParams.update({'figure.figsize': (12, 9), 'figure.dpi': 120})
import warnings
import itertools
warnings.filterwarnings("ignore")
plt.style.use('fivethirtyeight')
import statsmodels.api as sm
import matplotlib
matplotlib.rcParams['axes.labelsize'] = 14
matplotlib.rcParams['xtick.labelsize'] = 12
matplotlib.rcParams['ytick.labelsize'] = 12
matplotlib.rcParams['text.color'] = 'k'


#2016-2017
xls1 = pd.ExcelFile('C://Users//Aniruddha//OneDrive - Infiniti Research//Documents//New folder//bookings.xlsx')
df1 = pd.read_excel(xls1, '2016-2017 Bookings')
df1=df1.sort_values(by='Day of StayDate')
df1 = df1.reset_index(drop=True)
df1.columns
df1.rename(columns={'Hotel Code':'Mnemonic Code', 'Day of Conf Date':'Day of Confirmation Date','Confirm Number':'Confirmation Number','Room Type Code':'Room Type Booked','Status':'Booking Status','Day of StayDate':'Day of Stay Date','Rate':'RATE'}, inplace=True)
df1=df1[['Mnemonic Code', 'Day of Confirmation Date', 'Confirmation Number',
       'Room Type Booked', 'Booking Status', 'Day of Stay Date', 'RATE']]
df1['Mnemonic Code'] = df1['Mnemonic Code'].replace({'SUR':'MIASS', 'LWP':'LAXWP','INK':'NYCIK','SFD':'SFODR'})
df1['Mnemonic Code'] = df1['Mnemonic Code'].replace({'VRB':'MIAVB', 'EPC':'MIAEP','ANG':'MIAAG','EVT':'NYCAA',
      'MSE':'NTCME','LAP':'LAXWE','HWL':'LAXWM','TMO':'SFOBC'})


#2018-2019
xls2 = pd.ExcelFile('C://Users//Aniruddha//OneDrive - Infiniti Research//Desktop//A//2019.xlsx')
df2 = pd.read_excel(xls2, '2018-2019 Bookings')
df2=df2.sort_values(by='Day of Stay Date')
df2 = df2.reset_index(drop=True)
df2.columns


#Remove below 50 dolar hotel rates
df1=df1.loc[df1['RATE'] >= 50]
df1 = df1.reset_index(drop=True)

df2=df2.loc[df2['RATE'] >= 50]
df2 = df2.reset_index(drop=True)


#2018-2019
df2.columns
df2=df2.drop(['Day of Stay Date','Room Type Booked'], axis=1)
df2_V1=df2.drop_duplicates()
df2_V1['Bookings']=df2_V1.groupby(['Mnemonic Code','Day of Confirmation Date','Booking Status'])['Confirmation Number'].transform('count')
df3=df2_V1[['Mnemonic Code', 'Day of Confirmation Date','Bookings','Booking Status']].drop_duplicates()
list_of_values = ['MIASS','LAXWP','NYCIK','SFODR']
y = df3[df3['Mnemonic Code'].isin(list_of_values)]
y['Week_Number'] = y['Day of Confirmation Date'].dt.week
y['year'] =  y['Day of Confirmation Date'].dt.year
y.to_csv('y_v1.csv')

#Without pilot hotels
z = df3[~df3['Mnemonic Code'].isin(list_of_values)]
z['Week_Number'] = z['Day of Confirmation Date'].dt.week
z['year'] =  z['Day of Confirmation Date'].dt.year






#2017-2018
df1=df1.drop(['Day of Stay Date','Room Type Booked'], axis=1)
df1_V1=df1.drop_duplicates()
df1_V1['Bookings']=df1_V1.groupby(['Mnemonic Code','Day of Confirmation Date','Booking Status'])['Confirmation Number'].transform('count')
df4=df1_V1[['Mnemonic Code', 'Day of Confirmation Date','Bookings','Booking Status']].drop_duplicates()
list_of_values = ['MIASS','LAXWP','NYCIK','SFODR']
y1 = df4[df4['Mnemonic Code'].isin(list_of_values)]
y1['Week_Number'] = y1['Day of Confirmation Date'].dt.week
y1['year'] =  y1['Day of Confirmation Date'].dt.year
y1.to_csv('y1_v1.csv')


#Without pilot hotels
z1 = df4[~df4['Mnemonic Code'].isin(list_of_values)]
z1['Week_Number'] = z1['Day of Confirmation Date'].dt.week
z1['year'] =  z1['Day of Confirmation Date'].dt.year



#merging 
FRAMES=[y,y1]
result_pilot=pd.concat(FRAMES)
result_pilot.to_csv('result_pilot.csv')

FRAMES1=[z,z1]
result_nopilot=pd.concat(FRAMES1)
result_nopilot['Mnemonic Code'] = result_nopilot['Mnemonic Code'].replace({'VRB':'MIAVB', 'EPC':'MIAEP','ANG':'MIAAG','EVT':'NYCAA',
      'MSE':'NTCME','LAP':'LAXWE','HWL':'LAXWM','TMO':'SFOBC'})

result_nopilot.to_csv('result_nopilot1.csv')



#For Pilot Hotel MIASS
import pandas as pd
#result_pilot=pd.read_csv(r"C:\Users\Aniruddha\Files IHG\result_pilot.csv")


result_pilot=result_pilot.sort_values(['Day of Confirmation Date'])
result_pilot['month_year'] = result_pilot['Day of Confirmation Date'].dt.to_period('M') 
result_pilot['Total_Bookings']=result_pilot.groupby(['Mnemonic Code','month_year','Booking Status'])['Bookings'].transform('sum')
result_pilot.info()

Forecast1=result_pilot.drop(['Day of Confirmation Date',
       'Bookings','Week_Number','year'], axis=1)
Forecast1=Forecast1.drop_duplicates()
MIASS=Forecast1[Forecast1['Mnemonic Code'].str.contains("MIASS")]
MIASS_Confirm=MIASS[MIASS['Booking Status'].str.contains("Confirmed")]
MIASS_Confirm=MIASS_Confirm.drop(['Mnemonic Code',
       'Booking Status'], axis=1)
MIASS_Confirm.info()
MIASS_Confirm.set_index('month_year', inplace=True)
MIASS_Confirm.head()

MIASS_Confirm.plot(figsize=(5, 3))
plt.show()
MIASS_Confirm.to_csv('MIASS_Confirm.csv')
MIASS_Confirm_Cut=MIASS_Confirm[:'2018-12']

def stationarise(df,colname):

    result = adfuller(df[colname].dropna())
    print('ADF Statistic: %f' % result[0])
    print('p-value: %f' % result[1])
    dfoutput = pd.Series(result[0:4], index=['Test Statistic','p-value','#Lags Used','Number of Observations Used'])
    for key,value in result[4].items():
        dfoutput['Critical Value (%s)'%key] = value
    print (dfoutput)

    return dfoutput

def plot(df,colname):
    #rolmean = MIASS_Confirm_Cut.rolling(window=3).mean()   
   # rolstd = MIASS_Confirm_Cut.rolling(window=3).std()
    MIASS_Confirm['Total_Bookings'].plot(figsize=(5, 3))
    MIASS_Confirm.rolling(window=3).mean()['Total_Bookings'].plot()
    MIASS_Confirm.rolling(window=3).std()['Total_Bookings'].plot()
plt.show()

ts_log = np.log(MIASS_Confirm.Total_Bookings)
plt.plot(ts_log)

#differencing it to make the time series stationary
stationarise(MIASS_Confirm_Cut,'Total_Bookings')
plot(MIASS_Confirm_Cut,'Total_Bookings')




#if P Value > 0.05 we go ahead with finding the order of differencing
#test_stationarity(timeseries)
#rolmean = MIASS_Confirm_Cut.rolling(window=3).mean()   
#rolstd = MIASS_Confirm_Cut.rolling(window=3).std()






#first difference
#test_stationarity(timeseries)
MIASS_Confirm_Cut['First']=MIASS_Confirm_Cut.Total_Bookings-MIASS_Confirm_Cut.Total_Bookings.shift(1)
stationarise(MIASS_Confirm_Cut,'First')
plot(MIASS_Confirm_Cut,'First')

#second difference
MIASS_Confirm_Cut['Second']=MIASS_Confirm_Cut.Total_Bookings-MIASS_Confirm_Cut.Total_Bookings.shift(2)
stationarise(MIASS_Confirm_Cut,'Second')
plot(MIASS_Confirm_Cut,'Second')










mod = sm.tsa.statespace.SARIMAX(MIASS_Confirm_Cut.Total_Bookings, trend='n', order=(0,1,0), seasonal_order=(1,1,1,12))
results = mod.fit()
print (results.summary())

















#decomposition
from statsmodels.tsa.seasonal import seasonal_decompose 
decomposition = seasonal_decompose(MIASS_Confirm_Cut.Total_Bookings, freq=12)  
fig = plt.figure()  
fig = decomposition.plot()  
fig.set_size_inches(15, 8)


decomposition = sm.tsa.seasonal_decompose(MIASS_Confirm_Cut, freq=12)  
fig = plt.figure()  
fig = decomposition.plot()  
fig.set_size_inches(8, 4)
trend = decomposition.trend
seasonal = decomposition.seasonal 
residual = decomposition.resid 


#ts_log = np.log(MIASS_Confirm_Cut)
#ts_log.plot(figsize=(5, 3))
#plt.show()

from statsmodels.tsa.arima_model import ARIMA
import pmdarima as pm

df = pd.read_csv('https://raw.githubusercontent.com/selva86/datasets/master/wwwusage.csv', names=['value'], header=0)

model = pm.auto_arima(MIASS_Confirm_Cut.Total_Bookings, start_p=1, start_q=1,
                      test='adf',       # use adftest to find optimal 'd'
                      max_p=3, max_q=3, # maximum p and q
                      m=1,              # frequency of series
                      d=None,           # let model determine 'd'
                      seasonal=False,   # No Seasonality
                      start_P=0, 
                      D=0, 
                      trace=True,
                      error_action='ignore',  
                      suppress_warnings=True, 
                      stepwise=True)

print(model.summary())

!pip install pmdarima




p = d = q = range(0, 2)
pdq = list(itertools.product(p, d, q))
seasonal_pdq = [(x[0], x[1], x[2], 12) for x in list(itertools.product(p, d, q))]

print('Examples of parameter combinations for Seasonal ARIMA...')
print('SARIMAX: {} x {}'.format(pdq[1], seasonal_pdq[1]))
print('SARIMAX: {} x {}'.format(pdq[1], seasonal_pdq[2]))
print('SARIMAX: {} x {}'.format(pdq[2], seasonal_pdq[3]))
print('SARIMAX: {} x {}'.format(pdq[2], seasonal_pdq[4]))

for param in pdq:
    for param_seasonal in seasonal_pdq:
        try:
            mod = sm.tsa.statespace.SARIMAX(MIASS_Confirm_Cut.Total_Bookings,
                                            order=param,
                                            seasonal_order=param_seasonal,
                                            enforce_stationarity=False,
                                            enforce_invertibility=False)

            results = mod.fit()

            print('ARIMA{}x{}12 - AIC:{}'.format(param, param_seasonal, results.aic))
        except:
            continue
        
mod = sm.tsa.statespace.SARIMAX(MIASS_Confirm_Cut.Total_Bookings,
                                order=(1, 1, 1),
                                seasonal_order=(1, 1, 0, 12),
                                enforce_stationarity=False,
                                enforce_invertibility=False)

results = mod.fit()
print(results.summary())

results.plot_diagnostics(figsize=(16, 8))
plt.show()

pred = results.get_prediction(start=pd.to_datetime('2019-01'),end=pd.to_datetime('2019-04'), dynamic=True)
pred_ci = pred.conf_int()

ax = MIASS_Confirm_Cut.Total_Bookings['2016':].plot(label='observed')
pred.predicted_mean.plot(ax=ax, label='One-step ahead Forecast', alpha=.7, figsize=(8, 3))

ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.2)

ax.set_xlabel('Date')
ax.set_ylabel('No of Bookings')
plt.legend()

plt.show()



s_forecasted = pred.predicted_mean
s_forecasted.columns = ['pred_bookings']
s_forecasted=pd.DataFrame(s_forecasted)
s_truth = MIASS_Confirm['2019-01':]



from statsmodels.tsa.stattools import acf, pacf

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


# Compute the mean square error
mse = ((s_forecasted['pred_bookings'] - s_truth['Total_Bookings']) ** 2).mean()
print('The Mean Squared Error of our forecasts is {}'.format(round(mse, 2)))

print('The Root Mean Squared Error of our forecasts is {}'.format(round(np.sqrt(mse), 2)))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = check_arrays(y_true, y_pred)

    ## Note: does not handle mix 1d representation
    #if _is_1d(y_true): 
    #    y_true, y_pred = _check_1d_array(y_true, y_pred)

    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
y_true = [3, -0.5, 2, 7]; y_pred = [2.5, -0.3, 2, 8]
mean_absolute_percentage_error(y_true, y_pred)





