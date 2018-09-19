# Creating time series objects in R
# Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)

# EX: Time series plots
# Plot the data with facetting
autoplot(myts, facets = TRUE)
# Plot the data without facetting
autoplot(myts, facets = FALSE)
# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
# Find the outlier in the gold series
goldoutlier <- which.max(gold)
goldoutlier
# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

# EX: Seasonal plots
# Load the fpp2 package
library(fpp2)
# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)
# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)
# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start = 1992)
# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)

# Ex: Autocorrelation of seasonal and cyclic time series
# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)
# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1
# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)
# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

# Stock prices and white noise
# Plot the original series
autoplot(goog)
# Plot the differenced series
autoplot(diff(goog))
# ACF of the differenced series
ggAcf(diff(goog))
# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")

# EX: Naive forecasting methods
# Use naive() to forecast the goog series
fcgoog <- naive(goog, h =20)
# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)
# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h =16)
# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

# EX: Checking time series residuals
# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()
# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE
# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()
# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE

# EX: Evaluating forecast accuracy of non-seasonal methods
# Create the training data as train
train <- subset.ts(gold, end = 1000)
# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 108)
# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h =108)
# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)
# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc

# Ex: Evaluating forecast accuracy of seasonal methods
# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))
# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)
# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

# EX: Time-series cross validation
# Compute cross-validated errors for up to 8 steps ahead
e <- matrix(NA_real_, nrow = 1000, ncol = 8)
for (h in 1:8)
  e[, h] <- tsCV(goog, forecastfunction = naive, h = h)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()