# -*- coding: utf-8 -*-
"""
Created on Sun Apr 28 16:10:27 2019

@author: Aniruddha choudhury
"""
import numpy as np
import pandas as pd
import statsmodels.api as sm

import matplotlib.pyplot as plt
result_nopilot = pd.read_csv('result_nopilot1.csv')
result_nopilot['Mnemonic Code'] = result_nopilot['Mnemonic Code'].replace({'VRB':'MIAVB', 'EPC':'MIAEP','ANG':'MIAAG','EVT':'NYCAA',
      'MSE':'NTCME','LAP':'LAXWE','HWL':'LAXWM','TMO':'SFOBC'})
result_nopilot.info()
result_nopilot['Day of Confirmation Date'] = pd.to_datetime(result_nopilot['Day of Confirmation Date'], errors='coerce')

result_nopilot=result_nopilot.sort_values(['Day of Confirmation Date'])
result_nopilot['month_year'] = result_nopilot['Day of Confirmation Date'].dt.to_period('M') 
result_nopilot['Total_Bookings']=result_nopilot.groupby(['Mnemonic Code','month_year','Booking Status'])['Bookings'].transform('sum')
result_nopilot.info()

Forecast1=result_nopilot.drop(['Day of Confirmation Date',
       'Bookings','Week_Number','year'], axis=1)
Forecast1=Forecast1.drop_duplicates()


MIAVB=Forecast1[Forecast1['Mnemonic Code'].str.contains("MIAVB")]
MIAVB_Confirm=MIAVB[MIAVB['Booking Status'].str.contains("Confirmed")]
MIAVB_Confirm=MIAVB_Confirm.drop(['Mnemonic Code',
       'Booking Status'], axis=1)
MIAVB_Confirm.info()
MIAVB_Confirm.set_index('month_year', inplace=True)

MIAVB_Confirm=MIAVB_Confirm.drop(['Unnamed: 0'], axis=1)
MIAVB_Confirm=MIAVB_Confirm.drop(['month'], axis=1)
MIAVB_Confirm['month'] = MIAVB_Confirm.index
MIAVB_Confirm=MIAVB_Confirm.drop_duplicates()


