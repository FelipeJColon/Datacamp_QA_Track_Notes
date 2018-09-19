#In this course, package astsa is used 

# Data Play
# View a detailed description of AirPassengers
help(AirPassengers)
# Plot AirPassengers
plot(AirPassengers)
# Plot the DJIA daily closings
plot(djia$Close)
# Plot the Southern Oscillation Index
plot(soi)

# heteroscedasticity, seasonality and trend are the characteristics of dataset AirPassenger
# Plot globtemp and detrended globtemp
# Differencing is a great way to remove trends from your data.
# Detrending Data, coercing otherwise nonstationary data to stationarity.
par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))
# Plot cmort and detrended cmort
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))

#growth rate pt is often calculated as diff(log(x))
# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))
# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

# arima.sim(model, n, ...)
# model is a list with order of the model as c(p, d, q) and the coefficients
# p = order of AR, q = order of MA
# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0,0,0)), n = 200)
plot(WN)
# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0,0,1), ma= 0.9), n = 200)
plot(MA)
# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2,0,0), ar= c(1.5, -0.75)), n = 200)
plot(AR)

#Fitting an AR(1) Model
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 
# Plot the generated data 
plot(x)
# Plot the sample P/ACF pair
acf2(x)
# Fit an AR(1) to the data and examine the t-table
sarima(x, p=1, d=0, q =0)


# AR + MA Model ARMA(2,1)
# Fitting an ARMA model
# Xt=Xt−1−.9Xt−2+Wt+.8Wt−1,
# In an ARMA(p,q) model, the ACF and PACF both tail off. 
# astsa is preloaded
# Plot 
plot(x)
# Plot the sample P/ACF of x
acf2(x)
# Fit an ARMA(2,1) to the data and examine the t-table
sarima(x,p=2,d=0,q=1)

# AIC (Akaike information criterion)
# BIC (Baysian information criterion)
# Two common methods to choose models
# AIC and BIC measure the error and penalize differently for adding parameters
# you want to retain the model with the smallest AIC and/or BIC value.
# parsimony principle, 
# which is basic to all science and tells you to choose the simplest scientific explanation that fits the evidence.

# Residual analysis: residual should be white guassian noise

# (1) the standardized residual: The standardized residuals should behave as a white noise sequence with mean zero and variance one
# (2) the sample ACF of the residuals: The sample ACF of the residuals should look like that of white noise
# (3) a normal Q-Q plot: Normality is an essential assumption when fitting ARMA models. Below the blue line, 
# (4) the p-values corresponding to the Box-Ljung-Pierce Q-statistic:  Above the blue line

# differencing the data in your ARIMA(1,1,0) model makes it stationary and allows for further analysis.

#EX: Simulated ARIMA
# Plot sample P/ACF of differenced data and determine model
plot(diff(x))
acf2(diff(x))
# Estimate parameters and examine output
sarima(x, p=2,d=1,q=0)

# TO have a successful model fit, increase the coefficient and try to overfit the model. It the difference is insignificant, then it is a good model

#EX: 
# Plot P/ACF pair of differenced data 
acf2(diff(x)) # identify the right model to fit 
# Fit model - check t-table and diagnostics
sarima(x, p = 1, d=1, q = 0) # fit the model 
# Forecast the data 20 time periods ahead
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0)  #to the forcast
lines(y)  # compare the data with actual price

#EX: Fit a Pure Seasonal Model
# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)
# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12) # p,d,q refers to nonseasonal components, and P,D,Q refers to seasonal components

# EX: Fit a Mixed Seasonal Model
# Plot sample P/ACF pair to lag 60 and compare to actual
# Seasonal: Look at lags 12,24,36,
# Non-Seasonal: Look at 1,2,3,4,5
acf2(x)
acf2(x, max.lag=60)
# Fit the seasonal model to x
sarima(x, p =0,d=0,q=1,P=0,D=0,Q=1,S=12)
# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
acf2(d_birth, max.lag=60)
# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)
acf2(dd_birth, max.lag=60)
# Fit SARIMA(0,1,1)x(0,1,1)_12. What happens?
sarima(birth, p=0,d=1,q=1,P=0,D=1,Q=1,S=12)
# Add AR term and conclude
sarima(birth, p=1,d=1,q=1,P=0,D=1,Q=1,S=12)

#Ex: How Hard is it to Forecast Commodity Prices?
# Fit the chicken model again and check diagnostics
sarima(chicken,2,1,0, 1,0,0,12)
# Forecast the chicken data 5 years into the future
sarima.for(chicken,n.ahead=60, 2,1,0, 1,0,0,12)



