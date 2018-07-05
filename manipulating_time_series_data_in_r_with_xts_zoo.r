#Creating first xts object
data <- rnorm(5)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")

# Use xts() to create smith
smith <- xts(x = data, order.by = dates)
smith
# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")

hayek <- xts(x = data, order.by = dates, born = bday)

#Extract the core data which is the matrix that stores the data and index which is the time series for the data
hayek_core <- coredata(hayek)
class(hayek_core)
hayek_index <- index(hayek)
class(hayek_index)

# Create five dates starting from 2016-01-01
dates <- as.Date("2016-01-01") + 0:4
ts_a <- xts(x = c(1,2,3,4,5), order.by = dates)
ts_b <- xts(x = c(1,2,3,4,5), order.by = as.POSIXct(dates))
ts_a[index(ts_b)]
ts_b[index(ts_a)]	

#Converting to xts and matrix objects
au <- as.xts(austres)
am <- as.matrix(au)
head(am)
am2 <- as.matrix(austres)
head(am2)

# Create dat by reading tmp_file
dat <- read.csv(tmp_file)
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))
#Read tmp_file using read.zoo and convert it to  xts
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
dat_xts <- as.xts(dat_zoo)

#write and read zoo and convert to xts
sunspots_xts <- as.xts(sunspots)
write.zoo(sunspots_xts,file = "tmp",sep=",")
sun <- read.zoo("tmp", sep=",",FUN=as.yearmon)
sun_xts <- as.xts(sun)

#Xts uses a forward slash (/) to denote ranges, so this can't be used as a separator between date columns.
# Select all of 2016 from x
x_2016 <- x["2016"]
# Select January 1, 2016 to March 22, 2016
jan_march <- x["2016-01-01/2016-03-22"]
# Extract all data from irreg between 8AM and 10AM
morn_2010 <- irreg["T08:00/T10:00"] 

# Replace the values in x contained in the dates vector with NA
x[dates] <- NA
# Replace all values in x for dates starting June 9, 2016 with 0
x["2016-06-09/"] <- 0
# Verify that the value in x for June 11, 2016 is now indeed 0
#x[as.POSIXct("2016-06-11")]
x["2016-06-11"]

# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "week")
# # Print the last 2 observations in lastweek
last(lastweek, "2 days")
# # Extract all but the first two days of lastweek
first(lastweek, "-2 days")

# Add a to b, and fill all missing rows of b with 0
# a has 3 rows and b has only one
a + merge(b, index(a), fill = 0)
# Add a to b and fill NAs with the last observation
a + merge(b, index(a), fill = na.locf)

#Example of a left join
merge(a, b, join = "left", fill = 0)
#Temps
temps3 <- rbind(temps2, temps_july17, temps_july18)

#Example of na.locf
temps_next = na.locf(temps, fromLast= TRUE)

#Example of extrapolating NA data
na.approx(AirPass)

#A leading object
lead_x <- lag(x, k = -1)
#Create a lagging object called lag_x
lag_x <- lag(x, k = 1)

# Calculate the first difference of AirPass and assign to diff_by_hand
diff_by_hand <- AirPass - lag(AirPass)

# Use merge to compare the first parts of diff_by_hand and diff(AirPass)
merge(head(diff_by_hand), head(diff(AirPass)))

# Calculate the first order 12 month difference of AirPass
diff(AirPass, lag = 12, differences = 1)

#Find the position of the end of period in the data, you can find Kth period
#Last value returned will always be the length of the data set, even if it doesn't correspond to a skipped interval.
endpoints(temps, on = "weeks", k = 2)

# Calculate the weekly endpoints
ep <- endpoints(temps, on = "weeks")
# # Now calculate the weekly mean and display the results
period.apply(temps[, "Temp.Mean"], INDEX = ep, FUN = mean)
endpoints(temps, on = "weeks", k = 2)

# Split temps by week
temps_weekly <- split(temps, f = "weeks")
# Create a list of weekly means, temps_avg, and print this list
temps_avg <- lapply(X = temps_weekly, FUN = mean)
temps_avg

#Get a list of last days of the weeks
#1 Use the proper combination of split, lapply and rbind
temps_1 <- do.call(rbind, lapply(split(temps, "weeks"), function(w) last(w, n = "1 day")))

#2 Create last_day_of_weeks using endpoints()
ep = endpoints(temps, on = "weeks")
last_day_of_weeks <-period.apply(temps, INDEX = ep,FUN=last, n="1 day")  # or simply temps_2 <- temps[ep]

#Aggregate time series data to OHLC, by default OHLC is TRUE, change to false would turn the data into a univariate set
usd_eur_yearly <- to.period(usd_eur, period = "years", OHLC = TRUE)

# Convert eq_mkt to quarterly OHLC
mkt_quarterly <- to.period(eq_mkt, period = "quarters")
# Convert eq_mkt to quarterly using shortcut function
mkt_quarterly2 <- to.quarterly(eq_mkt, name ="edhec_equity", indexAt = "firstof")

#calculate cummulative annual return 
#Split edhec into years
edhec_years <- split(edhec , f = "years")

#Use lapply to calculate the cumsum for each year in edhec_years
edhec_ytd <- lapply(edhec_years, FUN = cumsum)

#Use do.call to rbind the results
edhec_xts <- do.call(rbind, edhec_ytd)

#View Index Format, 
index(temps)[1:3]

# Get the index class of temps
indexClass(temps)

# Get the timezone of temps
indexTZ(temps)

# Change the format of the time display
indexFormat(temps) <- "%b-%d-%Y"

# Construct times_xts with tzone set to America/Chicago
times_xts <- xts(1:10, order.by = times, tzone = "America/Chicago")

# Change the time zone of times_xts to Asia/Hong_Kong
tzone(times_xts) <- "Asia/Hong_Kong"
  
# Extract the current time zone of times_xts
tzone(times_xts)

# Calculate the periodicity of edhec_yearly - Yearly periodicity from 1997-12-31 to 2009-08-31
periodicity(edhec_yearly)

# Count the months quarters years
ndays(edhec)
nquarters(edhec)
nyears(edhec)

# Explore underlying units of temps in two commands: .index() and .indexwday()
.index(temps)
.indexwday(temps)

# Create an index of weekend days using which()
index <- which(.indexwday(temps) == 6 | .indexwday(temps) == 0)

# Select the index
temps[index]

# Make z have unique timestamps
z_unique <- make.index.unique(z, eps = 1e-4)

# Remove duplicate times in z
z_dup <- make.index.unique(z, drop = TRUE)

# Round observations in z to the next hour - n is the number of hours
z_round <- align.time(z, n = 3600)
