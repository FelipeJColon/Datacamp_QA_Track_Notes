#EX: Exploring raw time series
# Print the Nile dataset
print(Nile)
# List the number of observations in the Nile dataset
length(Nile)
# Display the first 10 elements of the Nile dataset
head(Nile, n =10)
# Display the last 12 elements of the Nile dataset
tail(Nile, n =12)

#EX: Basic time series plots
# Plot the Nile data
plot(Nile)
# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")
# Plot the Nile data with xlab, ylab, main, and type arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})", main = "Annual River Nile Volume at Aswan, 1871-1970", type = "b")

#EX: What does the time index tell us?
# Plot the continuous_series using continuous time indexing
par(mfrow=c(2,1)) # Determines the number of graphs
plot(continuous_time_index,continuous_series, type = "b")
# Make a discrete time index using 1:20 
discrete_time_index <- c(1:20)
# Now plot the continuous_series using discrete time indexing
plot(discrete_time_index,continuous_series, type = "b")

#Ex: Identifying the sampling frequency
# Plot AirPassengers
AirPassengers
plot(AirPassengers)
# View the start and end dates of AirPassengers
start(AirPassengers)
end(AirPassengers)
# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

#EX: Missing values
# Plot the AirPassengers data
plot(AirPassengers)
# Compute the mean of AirPassengers
mean(AirPassengers, na.rm = TRUE)
# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)
# Generate another plot of AirPassengers
plot(AirPassengers)
# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)

#Ex: Testing whether an object is a time series
# Check whether data_vector and time_series are ts objects
is.ts(data_vector)
is.ts(time_series)
# Check whether Nile is a ts object
is.ts(Nile)
# Check whether AirPassengers is a ts object
is.ts(AirPassengers)

#Ex: Plotting a time series object
# Check whether eu_stocks is a ts object
is.ts(eu_stocks)
# View the start, end, and frequency of eu_stocks
start(eu_stocks)
end(eu_stocks)
frequency(eu_stocks)
# Generate a simple plot of eu_stocks
plot(eu_stocks)
# Use ts.plot with eu_stocks
ts.plot(eu_stocks, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")
# Add a legend to your ts.plot
legend("topleft", colnames(eu_stocks), lty = 1, col = 1:4, bty = "n")

#Ex: Removing trends in variability via the logarithmic transformation
# Log rapid_growth
linear_growth <- log(rapid_growth)
# Plot linear_growth using ts.plot()
ts.plot(linear_growth, lty= 1)

#Ex: Removing trends in level by differencing
# Generate the first difference of z
dz <- diff(z)
# Plot dz
ts.plot(dz)
# View the length of z and dz, respectively
length(z)
length(dz)

#Ex: Removing seasonal trends with seasonal differencing
# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x, lag = 4)
# Plot dx
ts.plot(dx)
# View the length of x and dx, respectively 
length(x)
length(dx)

# EX: Simulate the white noise model 
# Simulate a WN model with list(order = c(0, 0, 0))
white_noise <- arima.sim(model = list(order = c(0,0,0)), n = 100)
# Plot your white_noise data
ts.plot(white_noise)
# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order = c(0,0,0)), n = 100, mean = 100, sd = 10)
ts.plot(white_noise_2)

#Ex: Estimate the white noise model
#arima() function estimates are very close to the sample mean and variance estimates, in fact identical for the mean.
# Fit the WN model to y using the arima command
arima(y, order= c(0,0,0))
# Calculate the sample mean and sample variance of y
mean(y)
var(y)

#Ex: As you can see, the first difference of your random_walk data is white noise data. This is because a random walk is simply recursive white noise data. 
#By removing the long-term trend, you end up with simple white noise.
# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100)
# Plot random_walk
ts.plot(random_walk)
# Calculate the first difference series
random_walk_diff <- diff(random_walk, difference = 1)
# Plot random_walk_diff
ts.plot(random_walk_diff)

#Ex: Simulate the random walk model with a drift
# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 1)
# Plot rw_drift
ts.plot(rw_drift)
# Calculate the first difference series
rw_drift_diff <- diff(rw_drift)
# Plot rw_drift_diff
ts.plot(rw_drift_diff)

#Ex: Estimate the random walk model
# Difference your random_walk data
rw_diff <- diff(random_walk, difference  = 1)
# Plot rw_diff
ts.plot(rw_diff)
# Now fit the WN model to the differenced data
model_wn <- arima(rw_diff, order = c(0,0,0))
# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef
# Plot the original random_walk data
ts.plot(random_walk)
# Use abline(0, ...) to add time trend to the figure 
# abline = add straight line
abline(a= 0, b= int_wn)

#Stationary process => 1. Mean reverting. 2. does not show periodicity
#Ex: Are the white noise model or the random walk model stationary?
# Use arima.sim() to generate WN data
white_noise <- arima.sim(model = list(order = c(0,0,0)), n = 100)
# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)
# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model = list(order = c(0,0,0)), n = 100, mean =0.4) 
# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)
# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))

#Ex: Asset prices vs. asset returns
# Plot eu_stocks
plot(eu_stocks)
# Use this code to convert prices to returns
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1
# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)
# Plot returns
plot(returns)
# Use this code to convert prices to log returns
logreturns <- diff(log(eu_stocks))
# Plot logreturns
plot(logreturns)

#Ex: Characteristics of financial time series
# Generate means from eu_percentreturns
colMeans(eu_percentreturns)
# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)
# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)
# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")
# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)

#Ex: Calculating sample covariances and correlations
# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)
# Use cov() with logreturns
cov(logreturns)
# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)
# Use cor() with logreturns
cor(logreturns)

#Ex: Calculating autocorrelations
# Define x_t0 as x[-1]
x_t0 <- x[-1]
# Define x_t1 as x[-n]
x_t1 <- x[-n]
# Confirm that x_t0 and x_t1 are (x[t], x[t-1]) pairs  
head(cbind(x_t0, x_t1))
# Plot x_t0 and x_t1
plot(x_t0, x_t1)
# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)
# Use acf with x
acf(x, lag.max = 1, plot = FALSE)
# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n

#Ex: The autocorrelation function
# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)

#Ex: Compare the random walk (RW) and autoregressive (AR) models
# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar=0.9), n = 200)
ts.plot(x)
acf(x)
# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(ar=0.98), n = 200)
ts.plot(y)
acf(y)
# Simulate and plot RW model
z <- arima.sim(model = list(order = c(0,1,0)), n = 200)
ts.plot(z)
acf(z)

#Ex: Simple forecasts from an estimated AR model
# Fit an AR model to Nile
AR_fit <-arima(Nile, order  = c(1,0,0))
print(AR_fit)
# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit, n.ahead=1)
# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]
# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)
# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)

#Ex: Simulate the simple moving average model
Note that the MA model can only produce a 1-step forecast. For additional forecasting periods, 
the predict() command simply extends the original 1-step forecast. This explains the unexpected horizontal lines after 1971.
# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma = 0.5), n = 100)
# Generate MA model with slope 0.9
y <- arima.sim(model = list(ma = 0.9), n = 100)
# Generate MA model with slope -0.5
z <- arima.sim(model = list(ma = -0.5), n = 100)
# Plot all three models together
plot.ts(cbind(x, y, z))

#Ex: Simple forecasts from an estimated MA model
# Note that the MA model can only produce a 1-step forecast. For additional forecasting periods, 
# the predict() command simply extends the original 1-step forecast. This explains the unexpected horizontal lines after 1971.
# Make a 1-step forecast based on MA
predict_MA <- predict(MA, n.ahead = 1)
# Obtain the 1-step forecast using $pred[1]
predict_MA$pred[1]
# Make a 1-step through 10-step forecast based on MA
predict(MA, n.ahead = 10)
# Plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)

#AR vs MA models
# To determine model fit, you can measure the Akaike information criterion (AIC) and Bayesian information criterion (BIC) 
# for each model. While the math underlying the AIC and BIC is beyond the scope of this course, 
# for your purposes the main idea is these these indicators penalize models with more estimated parameters, 
# to avoid overfitting, and smaller values are preferred. All factors being equal, 
# a model that produces a lower AIC or BIC than another model is considered a better fit.
# Find correlation between AR_fit and MA_fit
cor(AR_fit, MA_fit)
# Find AIC of AR
AIC(AR)
# Find AIC of MA
AIC(MA)
# Find BIC of AR
BIC(AR)
# Find BIC of MA
BIC(MA)


Plot A shows autocorrelation for the first lag only, 
which is consistent with the expectations of the MA model. 
Plot B shows dissipating autocorrelation across several lags, 
consistent with the AR model. Plot C is consistent with a RW model with considerable autocorrelation for many lags. 
Finally. Plot D shows virtually no autocorrelation with any lags, consistent with a WN model.
