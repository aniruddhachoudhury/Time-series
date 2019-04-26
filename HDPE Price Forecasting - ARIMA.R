library(readxl)
library(ggplot2)
library(forecast)
library(seasonal)
library(MASS)
library(urca)
library(GGally)
library(xgboost)
library(glmnet)
library(car)
library(lmtest)

setwd("~/Projects/Abbott")
mydata <- read_excel("HDPE Price Forecasting Dataset-VF.xlsx")

mydata_lm <- read_excel("HDPE Price Forecasting Dataset-Production.xlsx")
mydata_exog <- read_excel("HDPE Price Forecasting Dataset-VF.xlsx")

mydata$Date <- as.Date(mydata$Date)


original <- ts(mydata[,2:25], start = c(2014,1), frequency = 12)
train <- ts(mydata[1:56,2:25], start = c(2014,1), frequency = 12)
test <-  ts(mydata[57:59,2:25], start = c(2018,9),frequency = 12)


#Original Dataset
autoplot(original[,10]) +
  ggtitle("HDPE Price US") +
  ylab("$") +
  xlab("Year-Month")

ggseasonplot(original[,10]) +
  ggtitle("HDPE Price US") +
  ylab("Price") +
  xlab("Month")

#Naive
naive_hdpe <- naive(train[,10], h = 3)
autoplot(naive_hdpe)+ autolayer(test[,10],series =  'Test Data')
summary(naive_hdpe)
accuracy(naive_hdpe,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_naive<-sum(abs(na.omit(naive_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_naive<- cbind(test[,10],naive_hdpe$mean)


#Seasonal Naive
snaive_hdpe <- snaive(train[,10], h = 3)
autoplot(snaive_hdpe)+ autolayer(test[,10],series =  'Test Data')
summary(snaive_hdpe)
accuracy(snaive_hdpe,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_snaive<-sum(abs(na.omit(snaive_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_snaive<- cbind(test[,10],snaive_hdpe$mean)


#Drift Method
drift_hdpe <- rwf(train[,10], h = 3, drift=TRUE)
autoplot(drift_hdpe)+ autolayer(test[,10],series =  'Test Data')
summary(drift_hdpe)
accuracy(drift_hdpe,test[,10])
View(cbind(test[,10],drift_hdpe))

#calculating the deviation from actual data to measure accuracy
dev_drift<-sum(abs(na.omit(drift_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_drift<- cbind(test[,10],drift_hdpe$mean)


#Plotting Drift+Naive+SeasonalNaive
autoplot(train[,10])+ autolayer(rwf(train[,10], h = 3,drift = TRUE), series = 'Drift', PI = FALSE)+
  autolayer(naive(train[,10], h=11), series="Naïve", PI=FALSE) +
  autolayer(snaive(train[,10], h=11), series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly HDPE Prices") +
  xlab("Month-Year") + ylab("Prices") +
  guides(colour=guide_legend(title="Forecast"))


#X11 Decomposition
x11_hdpe <-seas(original[,10],x11="")
autoplot(x11_hdpe) +
  ggtitle("X11 decomposition of HDPE Prices - US")

autoplot(original[,10], series="Data") +
  autolayer(trendcycle(x11_hdpe), series="Trend") +
  autolayer(seasadj(x11_hdpe), series="Seasonally Adjusted") +
  xlab("Year") + ylab("Price") +
  ggtitle("HDPE Price - US") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#STL Decomposition
stl_hdpe <- stl(train[,10], s.window="periodic",robust = TRUE)
autoplot(stl_hdpe)

#Forecasting with decomposition
fcast <- stlf(train[,10], h = 3, method = "arima")
autoplot(fcast)
accuracy(fcast,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_stl<-sum(abs(na.omit(fcast$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_stl<- cbind(test[,10],fcast$mean)



#Simple Exponential Smoothening(Doesn't account for trend and seasonality)
ses_hdpe <- ses(train[,10], h = 3)
summary(ses_hdpe)
autoplot(ses_hdpe)
accuracy(ses_hdpe, test[,10])

#Holt's Linear Trend(accoutns for trend)
holt_hdpe <- holt(train[,10],h = 3)
holt_hdpe2 <- holt(train[,10],h = 3, damped = TRUE)
accuracy(holt_hdpe,test[,10])
accuracy(holt_hdpe2,test[,10])


#Holt Winters(Accounts for trend and seasonality)
hw_hdpe <- hw(train[,10], seasonal = "additive", h = 3)
checkresiduals(hw_hdpe)
summary(hw_hdpe)
autoplot(hw_hdpe)
accuracy(hw_hdpe, test[,10])

#calculating the deviation from actual data to measure accuracy
dev_hw<-sum(abs(na.omit(hw_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_hw<- cbind(test[,10],hw_hdpe$mean)


#ETS
ets_hdpe <- ets(train[,10],model = "ZAA")
fct_ets <- forecast(ets_hdpe,h = 3)
autoplot(fct_ets)
summary(ets_hdpe)
checkresiduals(ets_hdpe)
accuracy(fct_ets,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_ets<-sum(abs(na.omit(ets_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_ets<- cbind(test[,10],fct_ets$mean)


#Auto ARIMA
summary(ur.kpss(diff(train,1)))

autoplot(train[,10])
ggAcf(train[,10])
ggPacf(train[,10])

fit <- auto.arima(train[,10],max.p = 5, max.q = 5,max.d = 2
        ,ic = c("aic"),test = c("kpss"),stationary = FALSE,seasonal = FALSE)
summary(fit)
View(cbind(train[,10], fit$fitted, train[,10]-fit$fitted))
checkresiduals(fit)
arima_hdpe <- forecast(fit, h = 3)
autoplot(arima_hdpe)
View(cbind(test[,10], arima_hdpe))
accuracy(arima_hdpe,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_arima1<-sum(abs(na.omit(fit$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_arima1<- cbind(test[,10],arima_hdpe$mean)


#Manual ARIMA
ggAcf(diff(train[,10]))
ggPacf(diff(train[,10]))
fit1 <- Arima(train[,10],order = c(2,1,1))
checkresiduals(fit1)
fit1_predict <- forecast(fit1,h = 3)
View(cbind(test[,10], fit1_predict))
summary(fit1)
accuracy(fit1_predict, test[,10])
autoplot(fit1_predict)

#calculating the deviation from actual data to measure accuracy
dev_arima2<-sum(abs(na.omit(fit1$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_arima2<- cbind(test[,10],fit1_predict$mean)


#Seasonal ARIMA
sarima_fit <- Arima(train[,10], order=c(2,1,2), seasonal=c(0,1,1))
summary(sarima_fit)
sarima_predict <- forecast(sarima_fit,h = 3)
checkresiduals(sarima_fit)
autoplot(sarima_predict)
accuracy(sarima_predict, test[,10])
View(cbind(test[,10], sarima_predict))

#calculating the deviation from actual data to measure accuracy
dev_sarima<-sum(abs(na.omit(sarima_fit$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_sarima<- cbind(test[,10],sarima_predict$mean)


#BATS
bats_model<-bats(train[,10], use.box.cox = TRUE, use.trend = TRUE, use.damped.trend = TRUE,
                 use.arma.errors = TRUE)
fcst_bats<-forecast(bats_model,h=3)
checkresiduals(bats_model)
summary(bats_model)
autoplot(fcst_bats)
accuracy(fcst_bats, test[,10])


#TBATS
tbats_hdpe <- tbats(train[,10],use.box.cox = TRUE, use.trend = TRUE,
                    use.damped.trend = TRUE, seasonal.periods = TRUE,
                    use.arma.errors = TRUE)
fcst_tbats <- forecast(tbats_hdpe, h = 3)
autoplot(fcst_tbats)
checkresiduals(tbats_hdpe)
accuracy(fcst_tbats,test[,10])  


#croston model
croston_model<-croston(train[,10])
forecast(croston_model, h = 3)

#cubic spline prediction
cspline_model<- splinef(train[,10], h = 3, level = c(80, 95), fan = FALSE)


#Thetha model
thetha_model<-thetaf(train[,10],h=3)
autoplot(forecast(thetha_model, h = 3))

forecast(train[,10], h =3 )


#STEPWISE LINEAR REGRESSION
full.model <- lm(mydata_lm$`HDPE - US (USD / metric ton)` ~ . ,data = mydata_lm)
step <- stepAIC(full.model, direction="both")
summary(step)

####Time Series with Exogenous Variables####

#creating exegenous variables for forecast
exog<- cbind(crudevol = mydata_exog$`WTI Crude(Dollars per Barrel)`,
             polyprop = mydata_exog$`Polyproylene - Europe(USD/metric ton)`,
             interestrate = mydata_exog$`Short Term Interest Rates`,
             ethuylene_US = mydata_exog$`Ethylene - US(USD/metric ton)`,
             ethylene_A = mydata_exog$`Ethylene - Asia(USD/metric ton)`,
             HDPE_WE    = mydata_exog$`HDPE - Western Europe ( Euro/ton)`,
             proprice = mydata_exog$`Producer Price Index`)

xreq <- ts(exog[1:56,], start = c(2014,1), frequency = 12)

xreq_test <- ts(exog[57:59,],start = c(2018,9),frequency = 12)

#S-ARIMAX
sarimax_fit <- arima(train[,10],order=c(2,1,1), seasonal=c(0,1,1),xreg = xreq)
summary(sarimax_fit)
checkresiduals(sarimax_fit)
sarimax_hdpe <- forecast(sarimax_fit, h = 3,xreg = xreq_test)
autoplot(sarimax_hdpe)
View(cbind(test[,10], sarimax_hdpe))
accuracy(sarimax_hdpe,test[,10])

#calculating the deviation from actual data to measure accuracy
dev_sarimax<-sum(abs(na.omit(sarimax_fit$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_sarimax<- cbind(test[,10],sarimax_hdpe$mean)


#Uni Nodal Neural Network Models
neur_model<- nnetar(train[,10], repeats = 100,
                    lambda = BoxCox.lambda(train[,10]),
                    xreg = xreq)

#Forecasting next 3 months using Neural Networks Models
fcst_neural<-forecast(neur_model,h=3, 
                      lambda = BoxCox.lambda(train[,10]),
                      xreg = xreq_test, bootstrap = TRUE)

#calculating the deviation from actual data to measure accuracy
dev_neural<- sum(abs(na.omit(neur_model$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_neural<- cbind(test,fcst_neural$mean)


#Plotting the error in the key models:
autoplot(train[,10]-fit$fitted, series="AUTO ARIMA",) +
  autolayer(train[,10]-fit1$fitted, series="MANUAL ARIMA") +
  autolayer(train[,10]-hw_hdpe$fitted, series="Holt Winter") +
  autolayer(sarimax_fit$residuals, series="SARIMAX")+
  ggtitle("Plotting the errors") + xlab("Year") +
  ylab("Error")+ 
  guides(colour=guide_legend(title="Forecast"))+
  scale_color_manual(name = "Model", labels = c("AUTO ARIMA", "Holt Winter","MANUAL ARIMA", "SARIMAX"), values= c("blue", "green", "pink", "red"))

##########################CAUSAL MODELS##############################

#Preaparing the dataset for causal modelling
causal <- na.omit(read_excel("HDPE Price Forecasting Dataset-VF - CausalProd.xlsx"))
training_set <- causal[1:54,]

test_set <- causal[55:57,]

#Check for multicollinearity
x <- cor(causal)
#Check whether the data is normal or not for all variables
qqnorm(causal$`HDPE - Western Europe ( Euro/ton)`)
hist(causal$`Ethylene - US(USD/metric ton)`,breaks = 10)
pairs(~causal$HDPEUS+causal$`Ethylene - US(USD/metric ton)`)
pairs(~causal$HDPEUS+causal$`Ethylene - US(USD/metric ton)`+causal$`Consumer Price Index`+causal$`Producer Price Index`+causal$`Capacity Utilization`)

#Removing outliers
training_set <- training_set[-c(23,35,43),]
training_set <- training_set[-c(15,47,37),]
training_set <- training_set[-c(43),]

#--------------------Multiple Linear Regression------------------#
#--------------------StepWise Regression-------------------------#
full.model1 <- lm(causal$`HDPEUS` ~ . ,data = causal,na.action=na.omit)
step1 <- stepAIC(full.model1, direction="both",na.action=na.exclude)
summary(step1)
#Checking VIF for multicollinearity
vif(step1)

#Building the multiple regression model
l_m <- lm(HDPEUS ~  `Ethylene - US(USD/metric ton)` + 
            `Consumer Price Index` + HDPEUS_Lag1, data = training_set)
summary(l_m)
checkresiduals(l_m)

#Checking VIF for multicollinearity
vif(l_m)

#Check for autocorrelation 
dwtest(l_m)

#Check for heterskedasticity, normal distribution of error terms, linearity and additive nature

l_m_predict<- predict(l_m,newdata = test_set)
l_m_input <- test_set$HDPEUS

accuracy(l_m_predict,l_m_input)
#---------------------------XGBOOST---------------------------# 

#defining the hyper parameters of the model

param <- list(  objective           = "reg:linear",
                booster = "gbtree",
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7) 


#model equation
model_xgb<-xgboost(data = data.matrix(training_set[,-1]),
                   label = data.matrix(training_set[,1]),
                   missing = NA, 
                   weight = NULL,
                   params = param, 
                   nrounds = 300,
                   verbose = 1,
                   print_every_n = 1L)

#Testing the prediction accuracy
pred.response <- predict(model_xgb, data.matrix(test_set[,-1]))
input.response <- test_set$HDPEUS

accuracy(pred.response,input.response)
#Calculating the R2 of the model
R2 <- 1 - (sum((input.response-pred.response )^2)/
             sum((input.response-mean(input.response))^2))

#-----------------------------Lasso Regression------------------------------#
model_lasso<-glmnet(data.matrix(training_set[,-1]),
                    data.matrix(training_set[,1]),
                    family = "gaussian", 
                    alpha = 1,
                    standardize = TRUE,
                    intercept = TRUE,
                    type.gaussian="naive")

summary(model_lasso)

cv.out <- cv.glmnet(data.matrix(training_set[,-1]),data.matrix(training_set[,1]), alpha = 1,family = "gaussian")
bestlam <- cv.out$lambda.min


#Testing the prediction accuracy
pred.response1 <- predict(model_lasso, data.matrix(test_set[,-1]),s = bestlam)
input.response1 <- test_set$HDPEUS


#Calculating the R2 of the model
R2 <- 1 - (sum((input.response1-pred.response1 )^2)/
             sum((input.response1-mean(input.response1))^2))

mse <- mean((input.response1 - pred.response1)^2)


#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#

#-------------------Checking out accuracy across 6 months----------#

train1 <- ts(mydata[1:51,2:25], start = c(2014,1), frequency = 12)
test1 <-  ts(mydata[52:57,2:25], start = c(2018,4),frequency = 12)

#TBATS
tbats_hdpe <- tbats(train[,10],use.box.cox = TRUE, use.trend = TRUE,
                    use.damped.trend = TRUE, seasonal.periods = TRUE,
                    use.arma.errors = TRUE)
fcst_tbats <- forecast(tbats_hdpe, h = 6)
autoplot(fcst_tbats)
checkresiduals(tbats_hdpe)
accuracy(fcst_tbats,test[,10])  

#Holt's Winters
hw_hdpe1 <- hw(train1[,10], seasonal = "additive", h = 6)
checkresiduals(hw_hdpe1)
summary(hw_hdpe1)
autoplot(hw_hdpe1)
accuracy(hw_hdpe1, test1[,10])

#calculating the deviation from actual data to measure accuracy
dev_hw1<-sum(abs(na.omit(hw_hdpe1$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_hw1<- cbind(test1[,10],hw_hdpe1$mean)



#Manual ARIMA
ggAcf(diff(train1[,10]))
ggPacf(diff(train1[,10]))
fit2 <- Arima(train1[,10],order = c(2,1,1))
checkresiduals(fit2)
fit2_predict <- forecast(fit2,h = 6)
View(cbind(test1[,10], fit2_predict))
summary(fit2)
accuracy(fit2_predict, test1[,10])
autoplot(fit2_predict)

#calculating the deviation from actual data to measure accuracy
dev_arima2<-sum(abs(na.omit(fit1$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_arima2<- cbind(test[,10],fit1_predict$mean)

#creating exegenous variables for forecast
xreq1 <- ts(exog[1:51,], start = c(2014,1), frequency = 12)

xreq_test1 <- ts(exog[52:57,],start = c(2018,4),frequency = 12)


#Seasonal ARIMA
sarima_fit1 <- Arima(train1[,10], order=c(2,1,2), seasonal=c(0,1,1))
summary(sarima_fit1)
sarima_predict1 <- forecast(sarima_fit1,h = 6)
checkresiduals(sarima_fit1)
autoplot(sarima_predict1)
accuracy(sarima_predict1, test1[,10])
View(cbind(test1[,10], sarima_predict1))

#calculating the deviation from actual data to measure accuracy
dev_sarima1<-sum(abs(na.omit(sarima_fit1$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_sarima1<- cbind(test1[,10],sarima_predict1$mean)



#S-ARIMAX
sarimax_fit1 <- arima(train1[,10],order=c(2,1,1), seasonal=c(0,1,1),xreg = xreq1)
summary(sarimax_fit1)
checkresiduals(sarimax_fit1)
sarimax_hdpe1 <- forecast(sarimax_fit1, h = 6,xreg = xreq_test1)
autoplot(sarimax_hdpe1)
View(cbind(test1[,10], sarimax_hdpe1$mean))
accuracy(sarimax_hdpe1,test1[,10])


#---------------------------XGBOOST---------------------------# 

#defining the hyper parameters of the model

param <- list(  objective           = "reg:linear",
                booster = "gbtree",
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7) 


#model equation
model_xgb<-xgboost(data = data.matrix(train1[,-10]),
                   label = data.matrix(train1[,10]),
                   missing = NA, 
                   weight = NULL,
                   params = param, 
                   nrounds = 200,
                   verbose = 1,
                   print_every_n = 1L)

#Testing the prediction accuracy
pred.response <- predict(model_xgb, data.matrix(test1[,-10]))
input.response <- test1[,10]  

accuracy(pred.response,input.response)
#Calculating the R2 of the model
R2 <- 1 - (sum((input.response-pred.response )^2)/
             sum((input.response-mean(input.response))^2))



#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#-------------------6 months forecast ends here--------------------#


##############################Running Forecast#######################################
#Changing the months for every run#
#First run 'x1' is considering Dec,Jan,Feb as the test dataset. 'x2' is containing Jan,Feb,March
#as the test dataset and so and so forth
x1 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x2 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x3 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x4 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x4_1 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x5 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x6 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x8 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x9 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)
x10 <- cbind(test1[,10],hw_hdpe1$mean,fit2_predict$mean,sarima_predict1$mean,sarimax_hdpe1$mean)

write.csv(final,'test1.csv')

final <- rbind(x1,x2,x3,x4,x4_1,x5,x6,x8,x9,x10)


#------------------------Month Wise Best Model----------------------------#
train3 <- ts(mydata[1:57,2:25], start = c(2014,1), frequency = 12)
test3 <-  ts(mydata[58:59,2:25], start = c(2018,10),frequency = 12)

#Holt Winters(Accounts for trend and seasonality)
hw_hdpe <- hw(train3[,10], seasonal = "additive", h = 3)
checkresiduals(hw_hdpe)
summary(hw_hdpe)
autoplot(hw_hdpe)
accuracy(hw_hdpe, test3[,10])

#calculating the deviation from actual data to measure accuracy
dev_hw<-sum(abs(na.omit(hw_hdpe$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_hw<- cbind(test3[,10],hw_hdpe$mean)


#Manual ARIMA
ggAcf(diff(train3[,10]))
ggPacf(diff(train3[,10]))
fit1 <- Arima(train3[,10],order = c(1,1,1))
checkresiduals(fit1)
fit1_predict <- forecast(fit1,h = 3)
View(cbind(test3[,10], fit1_predict))
summary(fit1)
accuracy(fit1_predict, test3[,10])
autoplot(fit1_predict)

#calculating the deviation from actual data to measure accuracy
dev_arima2<-sum(abs(na.omit(fit1$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_arima2<- cbind(test3[,10],fit1_predict$mean)


#Seasonal ARIMA
sarima_fit <- Arima(train3[,10], order=c(1,1,0), seasonal=c(1,0,0))
summary(sarima_fit)
sarima_predict <- forecast(sarima_fit,h = 3)
checkresiduals(sarima_fit)
autoplot(sarima_predict)
accuracy(sarima_predict, test3[,10])
View(cbind(test3[,10], sarima_predict))

#calculating the deviation from actual data to measure accuracy
dev_sarima<-sum(abs(na.omit(sarima_fit$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_sarima<- cbind(test3[,10],sarima_predict$mean)


#TBATS
tbats_hdpe <- tbats(train3[,10],use.box.cox = TRUE, use.trend = TRUE,
                    use.damped.trend = TRUE, seasonal.periods = TRUE,
                    use.arma.errors = TRUE)
fcst_tbats <- forecast(tbats_hdpe, h = 3)
autoplot(fcst_tbats)
checkresiduals(tbats_hdpe)
accuracy(fcst_tbats,test3[,10])  

####Time Series with Exogenous Variables####

#creating exegenous variables for forecast
exog<- cbind(polyprop = mydata_exog$`Polyproylene - Europe(USD/metric ton)`,
             interestrate = mydata_exog$`Short Term Interest Rates`,
             ethuylene_US = mydata_exog$`Ethylene - US(USD/metric ton)`,
             ethylene_A = mydata_exog$`Ethylene - Asia(USD/metric ton)`,
             HDPE_WE    = mydata_exog$`HDPE - Western Europe ( Euro/ton)`,
             proprice = mydata_exog$`Producer Price Index`)

xreq3 <- ts(exog[1:57,], start = c(2014,1), frequency = 12)

xreq_test3 <- ts(exog[58:59,],start = c(2018,10),frequency = 12)

#S-ARIMAX
sarimax_fit <- arima(train3[,10],order=c(1,1,0), seasonal=c(1,0,0),xreg = xreq3)
summary(sarimax_fit)
checkresiduals(sarimax_fit)
sarimax_hdpe <- forecast(sarimax_fit, h = 2,xreg = xreq_test3)
autoplot(sarimax_hdpe)
View(cbind(test3[,10], sarimax_hdpe))
accuracy(sarimax_hdpe,test3[,10])

#calculating the deviation from actual data to measure accuracy
dev_sarimax<-sum(abs(na.omit(sarimax_fit$residuals)))
#combining Actuals & Predicted to compare across models
act_pred_sarimax<- cbind(test3[,10],sarimax_hdpe$mean)


#---------------------------XGBOOST---------------------------# 

#defining the hyper parameters of the model

param <- list(  objective           = "reg:linear",
                booster = "gbtree",
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7) 


#model equation
model_xgb<-xgboost(data = data.matrix(train3[,-10]),
                   label = data.matrix(train3[,10]),
                   missing = NA, 
                   weight = NULL,
                   params = param, 
                   nrounds = 200,
                   verbose = 1,
                   print_every_n = 1L)

#Testing the prediction accuracy
pred.response <- predict(model_xgb, data.matrix(test3[,-10]))
input.response <- test3[,10]

accuracy(pred.response,input.response)
#Calculating the R2 of the model
R2 <- 1 - (sum((input.response-pred.response )^2)/
             sum((input.response-mean(input.response))^2))

#------------------------Month wise Best model ends here -----------------
