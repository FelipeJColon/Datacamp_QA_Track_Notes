# EX: Box Cox Transformations for time series
# Plot the series
autoplot(a10)
# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda = 0.0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()
# Compare with BoxCox.lambda()
BoxCox.lambda(a10)

# EX: Non-seasonal differencing for stationarity
# Plot the US female murder rate
autoplot(wmurders)
# Plot the differenced murder rate
autoplot(diff(wmurders))
# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))

# Automatic ARIMA models for non-seasonal time series
# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)
# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE
# Summarize the model
summary(fit)
# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1
# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()

# EX: Forecasting with ARIMA models
# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2, 1, 3), include.constant = TRUE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0, 0, 1), include.constant = TRUE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0, 2, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h = 1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima() %>% forecast(h=10) %>% autoplot()

# EX: Comparing auto.arima() and ets() on non-seasonal data
# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}
# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h = 1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h = 1)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
# Plot 10-year forecasts using the best model class
austa %>% farima(h=10) %>% autoplot()

# EX: Automatic ARIMA models for seasonal time series
# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()
# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda =0)
# Summarize the fitted model
summary(fit)
# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1
# Plot 2-year forecasts
fit %>% forecast(h=24) %>% autoplot()

# EX:Exploring auto.arima() options
# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)
# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)
summary(fit1)
summary(fit2)
# AICc of better model
AICc <- 68.39
# Compute 2-year forecasts from better model
fit2 %>% forecast(h=8) %>% autoplot()

# Comparing auto.arima() and ets() on seasonal data
# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = c(1988,1), end = c(2007, 4))
# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)
# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)
# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)
# Use accuracy() to find better model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)
bettermodel <- fit2


