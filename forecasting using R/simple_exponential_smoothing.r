# Simple exponential smoothing
# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)
# Use summary() to see the model parameters
summary(fc)
# Use autoplot() to plot the forecasts
autoplot(fc)
# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))
# SES vs naive
# Create a training set using subset.ts()
train <- subset.ts(marathon, end = length(marathon) - 20)
# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)
# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)
# Save the best forecasts as fcbest
fcbest <- fcnaive

#Holt-Winters with monthly data
# Plot the data
autoplot(a10)
# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)
# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE
# Plot forecasts
autoplot(fc)

# Create training data with subset()
train <- subset.ts(hyndsight, end = length(hyndsight) - 28)
# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)
# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)
# Find better forecasts with accuracy()
accuracy(fcsn, hyndsight)
accuracy(fchw, hyndsight)
autoplot(fchw)

#Automatic forecasting with exponential smoothing
# Fit ETS model to austa in fitaus
fitaus <- ets(austa)
# Check residuals
checkresiduals(fitaus)
# Plot forecasts
autoplot(forecast(fitaus))
# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))
# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE

# ETS vs seasonal naive
# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}
# Apply tsCV() for both methods
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)
# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm=TRUE)
