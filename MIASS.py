# -*- coding: utf-8 -*-
"""
Created on Fri Apr 26 10:05:31 2019

@author: Aniruddha
"""

import pandas as pd
result_pilot=pd.read_csv(r"C:\Users\Aniruddha\Files IHG\result_pilot.csv")

import date as dt
from datetime import date as dt
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

#decomposition
from pylab import rcParams
rcParams['figure.figsize'] = 10, 4

decomposition = sm.tsa.seasonal_decompose(MIASS_Confirm, model='additive')
fig = decomposition.plot()
plt.show()


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
            mod = sm.tsa.statespace.SARIMAX(MIASS_Confirm,
                                            order=param,
                                            seasonal_order=param_seasonal,
                                            enforce_stationarity=False,
                                            enforce_invertibility=False)

            results = mod.fit()

            print('ARIMA{}x{}12 - AIC:{}'.format(param, param_seasonal, results.aic))
        except:
            continue
        
mod = sm.tsa.statespace.SARIMAX(MIASS_Confirm,
                                order=(1, 1, 1),
                                seasonal_order=(1, 1, 0, 12),
                                enforce_stationarity=False,
                                enforce_invertibility=False)

results = mod.fit()

print(results.summary().tables[1])

results.plot_diagnostics(figsize=(16, 8))
plt.show()

pred = results.get_prediction(start=pd.to_datetime('2018-01'), dynamic=False)
pred_ci = pred.conf_int()

ax = MIASS_Confirm['2016':].plot(label='observed')
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
s_truth = MIASS_Confirm['2018-01':]

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

