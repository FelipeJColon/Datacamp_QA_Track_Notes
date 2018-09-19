# EX: Encoding your flight data
# Load the xts package
library(xts)
# Convert date column to a time-based class
flights$date <- as.Date(flights$date)
# Convert flights to an xts object using as.xts
flights_xts <- as.xts(flights [ , -5], order.by = flights$date)
# Check the class of flights_xts
class(flights_xts)
# Examine the first five lines of flights_xts
head(flights_xts)

#Ex: Visualize flight data
# Use plot.xts() to view total monthly flights into BOS over time
plot.xts(flights_xts$total_flights)
# Use plot.xts() to view monthly delayed flights into BOS over time
plot.xts(flights_xts$delay_flights)
# Use plot.zoo() to view all four columns of data in their own panels
plot.zoo(flights_xts, plot.type = "multiple", ylab = labels)
# Use plot.zoo() to view all four columns of data in one panel
plot.zoo(flights_xts, plot.type = "single", lty = lty)
legend("right", lty = lty, legend = labels)

#EX: Calculate time series trends
# Calculate percentage of flights delayed each month: pct_delay
flights_xts$pct_delay <- (flights_xts$delay_flights / flights_xts$total_flights) * 100
# Use plot.xts() to view pct_delay over time
plot.xts(flights_xts$pct_delay)
# Calculate percentage of flights cancelled each month: pct_cancel
flights_xts$pct_cancel <- (flights_xts$cancel_flights / flights_xts$total_flights) * 100
# Calculate percentage of flights diverted each month: pct_divert
flights_xts$pct_divert <- (flights_xts$divert_flights / flights_xts$total_flights) * 100
# Use plot.zoo() to view all three trends over time
plot.zoo(flights_xts[, c("pct_delay", "pct_cancel", "pct_divert")])

#EX: Saving time - I
# Save your xts object to rds file using saveRDS
saveRDS(object = flights_xts, file = "flights_xts.rds")
# Read your flights_xts data from the rds file
flights_xts2 <- readRDS("flights_xts.rds")
# Check the class of your new flights_xts2 object
class(flights_xts2)
# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, n= 5)

#EX:Saving time - II
# Export your xts object to a csv file using write.zoo
write.zoo(flights_xts, file = "flights_xts.csv", sep = ",")
# Open your saved object using read.zoo
flights2 <- read.zoo("flights_xts.csv", sep = ",", FUN = as.Date, header = TRUE, index.column = 1)
# Encode your new object back into xts
flights_xts2 <- as.xts(flights2)
# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, n = 5)

# Merging using rbind()
# Confirm that the date column in each object is a time-based class
class(temps_1)
class(temps_2)
# Encode your two temperature data frames as xts objects
temps_1_xts <- as.xts(temps_1[, -4], order.by = temps_1$date)
temps_2_xts <- as.xts(temps_2[, -4], order.by = temps_2$date)
# View the first few lines of each new xts object to confirm they are properly formatted
head(temps_1_xts)
head(temps_2_xts)
# Use rbind to merge your new xts objects
temps_xts <- rbind(temps_1_xts, temps_2_xts)
# View data for the first 3 days of the last month of the first year in temps_xts
first(last(first(temps_xts, "1 year"), "1 month"), "3 days")

# EX:Visualizing Boston winters
# Identify the periodicity of temps_xts
periodicity(temps_xts)
# Generate a plot of mean Boston temperature for the duration of your data
plot.xts(temps_xts$mean)
# Generate a plot of mean Boston temperature from November 2010 through April 2011
plot.xts(temps_xts$mean["201011/201104"])
# Use plot.zoo to generate a single plot showing mean, max, and min temperatures during the same period 
plot.zoo(temps_xts["201011/201104"], plot.type = "single", lty = lty)

# EX: Subsetting and adjusting periodicity
# Subset your temperature data to include only 2010 through 2015: temps_xts_2
temps_xts_2 <- temps_xts["2010/2015"]
# Use to.period to convert temps_xts_2 to monthly periodicity
temps_monthly <- to.period(temps_xts_2, period = "months", OHLC = FALSE, indexAt = "firstof")
# Compare the periodicity and duration of temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)

# EX: Using merge() and plotting over time
# Split temps_xts_2 into separate lists per month
monthly_split <- split(temps_xts_2$mean , f = "months")
# Use lapply to generate the monthly mean of mean temperatures
mean_of_means <- lapply(monthly_split, FUN = mean)
# Use as.xts to generate an xts object of average monthly temperature data
temps_monthly <- as.xts(as.numeric(mean_of_means), order.by = index)
# Compare the periodicity and duration of your new temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)

# Ex: Using merge() and plotting over time
# Use merge to combine your flights and temperature objects
flights_temps <- merge(flights_xts, temps_monthly)
# Examine the first few rows of your combined xts object
head(flights_temps)
# Use plot.zoo to plot these two columns in a single panel
plot.zoo(flights_temps[,c("pct_delay", "temps_monthly")], plot.type = "single", lty = lty)
legend("topright", lty = lty, legend = labels, bg = "white")

# Expanding your data
# Confirm the periodicity and duration of the vis and wind data
periodicity(vis)
periodicity(wind)
# Merge vis and wind with your existing flights_temps data
flights_weather <- merge(flights_temps, vis, wind)
# View the first few rows of your flights_weather data
head(flights_weather)

# EX：Exploring economic data
# Get a summary of your GDP data
summary(gdp)
# Convert GDP date column to time object
gdp$date <- as.yearqtr(gdp$date)
# Convert GDP data to xts
gdp_xts <- as.xts(gdp[, -1], order.by = gdp$date)
# Plot GDP data over time
plot.xts(gdp_xts)

# EX:Replace missing data - I
# Fill NAs in gdp_xts with the last observation carried forward
gdp_locf <- na.locf(gdp_xts)

# Fill NAs in gdp_xts with the next observation carried backward 
gdp_nocb <- na.locf(gdp_xts,fromLast =TRUE)

# Produce a plot for each of your new xts objects
par(mfrow = c(2,1))
plot.xts(gdp_locf, major.format = "%Y")
plot.xts(gdp_nocb, major.format = "%Y")

# EX:Replace missing data - I
# Fill NAs in gdp_xts using linear approximation
gdp_approx <- na.approx(gdp_xts)
# Plot your new xts object
plot.xts(gdp_approx, major.format = "%Y")
# Query for GDP in 1993 in gdp_approx
gdp_approx["1993"]

# EX: Add a discrete rolling sum to GDP data
# Add a quarterly difference in gdp
gdp$quarterly_diff <- diff(gdp$gdp, lag = 4, differences = 1)
# Split gdp$quarterly_diff into years
gdpchange_years <- split(gdp$quarterly_diff, f = "years")
# Use lapply to calculate the cumsum each year
gdpchange_ytd <- lapply(gdpchange_years, FUN = cumsum)
# Use do.call to rbind the results
gdpchange_xts <- do.call(rbind, gdpchange_ytd)
# Plot cumulative year-to-date change in GDP
plot.xts(gdpchange_xts, type = "h")

# EX: Add a discrete rolling sum to GDP data
# Add a quarterly difference in gdp
gdp$quarterly_diff <- diff(gdp$gdp, lag = 1, differences = 1)
head(gdp)
# Split gdp$quarterly_diff into years
gdpchange_years <- split(gdp$quarterly_diff, f = "years")
# Use lapply to calculate the cumsum each year
gdpchange_ytd <- lapply(gdpchange_years, FUN = cumsum)
# Use do.call to rbind the results
gdpchange_xts <- do.call(rbind, gdpchange_ytd)
# Plot cumulative year-to-date change in GDP
plot.xts(gdpchange_xts, type = "h")

# Ex: Add a continuous rolling average to unemployment data
# Use rollapply to calculate the rolling yearly average US unemployment
unemployment$year_avg <- rollapply(unemployment$us, width = 12, FUN = mean)
# Plot all columns of US unemployment data
plot.zoo(unemployment[, c("us", "year_avg")], plot.type = "single", lty = lty, lwd = lwd)

# EX: Manipulating MA unemployment data
# Add a one-year lag of MA unemployment
unemployment$ma_yearlag <- lag(unemployment$ma,k=12)
head(unemployment$ma_yearlag, n=15)
# Add a six-month difference of MA unemployment
unemployment$ma_sixmonthdiff <- diff(unemployment$ma, lag = 6, difference =1)
# Add a six-month rolling average of MA unemployment
unemployment$ma_sixmonthavg <- rollapply(unemployment$ma, width=6, FUN=mean)
# Add a yearly rolling maximum of MA unemployment
unemployment$ma_yearmax <- rollapply(unemployment$ma, width=12, FUN=max)
# View the last year of unemployment data
tail(unemployment, n=12)

# EX: Encoding and plotting Red Sox data
# View summary information about your redsox data
summary(redsox)
# Convert the date column to a time-based format
redsox$date<- as.Date(redsox$date)
# Convert your red sox data to xts
redsox_xts <- as.xts(redsox[,-1], order.by = redsox$date)
# Plot the Red Sox score and the opponent score over time
plot.zoo(redsox_xts[, c("boston_score", "opponent_score")])

# EX: Calculate a closing average
# Generate a new variable coding for red sox wins
redsox_xts$win_loss <- ifelse(redsox_xts$boston_score > redsox_xts$opponent_score, 1, 0)
# Identify the date of the last game each season
close <- endpoints(redsox_xts, on = "years")
# Calculate average win/loss record at the end of each season
period.apply(redsox_xts[, "win_loss"], INDEX=close, FUN=mean)

# EX: Calculate and plot a seasonal average
# Split redsox_xts win_loss data into years 
redsox_seasons <- split(redsox_xts$win_loss, f = "years")
# Use lapply to calculate the cumulative mean for each season
redsox_ytd <- lapply(redsox_seasons, cummean)
# Use do.call to rbind the results
redsox_winloss <- do.call(rbind, redsox_ytd)
# Plot the win_loss average for the 2013 season
plot.xts(redsox_winloss["2013"], ylim = c(0, 1))

# EX: Calculate and plot a rolling average
# Select only the 2013 season
redsox_2013 <- redsox_xts["2013"]
# Use rollapply to generate the last ten average
lastten_2013 <- rollapply(redsox_2013$win_loss, width = 10, FUN = mean)
# Plot the last ten average during the 2013 season
plot.xts(lastten_2013, ylim = c(0, 1))

# EX: Extract weekend games
# Extract the day of the week of each observation
weekday <- .indexwday(sports)
head(weekday)
# Generate an index of weekend dates
weekend <- which(.indexwday(sports) == 0 | .indexwday(sports) == 6)
# Subset only weekend games
weekend_games <- sports[weekend]
head(weekend_games)

# EX: Calculate a rolling average across all sports
# Generate a subset of sports data with only homegames
homegames <- sports[sports$homegame == 1]
# Calculate the win/loss average of the last 20 home games
homegames$win_loss_20 <- rollapply(homegames$win_loss, width = 20, FUN = mean)
# Calculate the win/loss average of the last 100 home games
homegames$win_loss_100 <- rollapply(homegames$win_loss, width = 100, FUN = mean)
homegames
# Use plot.xts to generate
plot.zoo(homegames[, c("win_loss_20", "win_loss_100")], plot.type = "single", lty = lty, lwd = lwd)

