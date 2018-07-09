# Load the quantmod package
library(quantmod)

# Import QQQ data from Yahoo! Finance, QQQ is set to the env
getSymbols(Symbols = "QQQ", auto.assign = TRUE, src="yahoo")
# Look at the structure of the object getSymbols created
str(QQQ)
# auto.assign = FALSE or env = NULL both returns the data into a variable
spy <- getSymbols(Symbols = "SPY", auto.assign = FALSE, src="yahoo")

# Quandl vs getSymbos
# 1. Quandl always return the data 
# 2. Quandl returns data.frame by default, getSymbols return xts object by default

#Using Quandl to get data
library(Quandl)

gdp <- Quandl(code = "FRED/GDP")

str(gdp)
#gdp_xts <- Quandl(code = "FRED/GDP", type = "zoo")
gdp_xts <- Quandl(code = "FRED/GDP", type = "xts")

#Get FX data from oanda since 190 days ago 
getSymbols("GBP/CAD", from = Sys.Date() - 190, to = Sys.Date(), src = "oanda")

# Single Column extractor function 
#Op(), Hi(), Lo(), Cl(), Vo(), Ad()
# Extract the close column
dc_close <- Cl(DC)

# Download CME data for CL and BZ as an xts object
oil_data <- Quandl(code = c("CME/CLH2016", "CME/BZH2016"), type = "xts")

# Extract the Open price for CLH2016
cl_open <- getPrice(oil_data, symbol = "CLH2016", prefer = "Open$")
# Look at January, 2016 using xts' ISO-8601 subsetting
cl_open["2016-01"]

# Download quarterly CL and BZ returns
qtr_return = Quandl(collapse = "quarterly", type="xts", code= quandl_codes,transform = "rdiff")

# View the settle price returns for both series
getPrice(qtr_return, prefer = "Settle")

#EX：Combine many instruments into one object

# Call head on each object in data_env using eapply
data_list <- eapply(data_env, FUN = head)

# Merge all the list elements into one xts object
data_merged <- do.call(merge, data_list)

# Ensure the columns are ordered: open, high, low, close
data_ohlc <- OHLC(data_merged)

#EX： Extract the Close column from many instruments
symbols <- c("AAPL", "MSFT", "IBM")
data_env <- new.env()
getSymbols(Symbols = symbols, env = data_env, src = "yahoo")
# Extract the close column from each object and combine into one xts object
close_data <- do.call(merge, eapply(data_env, Cl))
# View the head of close_data
head(close_data)

#EX: Set a default data source
# Set the default to pull data from Alpha Vantage
setDefaults(getSymbols, src = "av")
# Get GOOG data
getSymbols(Symbols= "GOOG",auto.assign = TRUE)
str(GOOG)

#EX: Set default arguments for a getSymbols source
# Look at getSymbols.yahoo arguments
args(getSymbols.yahoo)
setDefaults(getSymbols.yahoo, from = "2000-01-01")
getDefaults("getSymbols.yahoo")

#EX: Set default data source for one symbol
head(CP)
setSymbolLookup(CP = list(src = "FRED"))
getSymbols("CP")
head(CP)

#EX: Save and load symbol lookup table
# Save symbol lookup table
saveSymbolLookup("my_symbol_lookup.rda")
# Set default source for CP to "yahoo"
setSymbolLookup(CP = list(src = "yahoo"))
# Verify the default source is "yahoo"
getSymbolLookup(Symbols = "CP")
# Load symbol lookup table
loadSymbolLookup("my_symbol_lookup.rda")

#EX: Access the object using get() or backticks
# Load BRK-A data
getSymbols("BRK-A")
# Use backticks and head() to look at the loaded data
head(`BRK-A`)
# Use get() to assign the BRK-A data to an object named BRK.A
BRK.A <- get("BRK-A")

#EX: Create valid name for one instrument
# Create BRK.A object
BRK.A <- getSymbols(Symbols="BRK-A",auto.assign = FALSE)
# Create col_names object with the column names of BRK.A
col_names <- colnames(BRK.A)
# Set BRK.A column names to syntactically valid names
colnames(BRK.A) <- make.names(col_names)

#EX: Create valid names for multiple instruments
# Set name for BRK-A to BRK.A
setSymbolLookup(BRK.A = list(name = "BRK-A"))

# Set name for T (AT&T) to ATT
setSymbolLookup(ATT = list(name = "T"))

# Load BRK.A and ATT data
getSymbols(c("BRK.A", "ATT"))

#EX: Create a zero-width and regular xts object
# Extract the start date of the series
start_date <- start(irregular_xts)

# Extract the end date of the series
end_date <- end(irregular_xts)

# Create a regular date sequence
regular_index <- seq(from = start_date,
    to = end_date,
    by = "day")

# Create a zero-width xts object
regular_xts <- xts(, order.by = regular_index)

#EX: Use merge to make an irregular index regular
# Merge irregular_xts and regular_xts
merged_xts <- merge(irregular_xts, regular_xts)
# Look at the first few rows of merged_xts
head(merged_xts)
# Use the fill argument to fill NA with their previous value
merged_filled_xts <- merge(merged_xts, fill = na.locf)
# Look at the first few rows of merged_filled_xts
head(merged_filled_xts)

#EX: Aggregate daily data and merge with monthly data
# Aggregate DFF to monthly
monthly_fedfunds <- apply.monthly(DFF, mean,na.rm=TRUE)
# Convert index to yearmon
 index(monthly_fedfunds) <- as.yearmon(index(monthly_fedfunds))
# Merge FEDFUNDS with the monthly aggregate
merged_fedfunds <- merge(FEDFUNDS, monthly_fedfunds)

#EX: Align series to first and last day of month
# Fill NA forward
merged_fedfunds_locf <- na.locf(merged_fedfunds)
# Extract index values containing last day of month
aligned_last_day <- merged_fedfunds_locf[index(monthly_fedfunds)]
# Fill NA backward
merged_fedfunds_locb <- na.locf(merged_fedfunds, fromLast=TRUE)
# Extract index values containing first day of month
aligned_first_day <- merged_fedfunds_locb[index(FEDFUNDS)]

#EX: Aggregate to weekly, ending on Wednesdays
# Extract index weekdays
index_weekdays <- .indexwday(DFF)
# Find locations of Wednesdays
wednesdays <- which(index_weekdays == 3)
# Create custom end points
end_points <- c(0, wednesdays, nrow(DFF))
# Calculate weekly mean using custom end points
period.apply(DFF, INDEX=end_points, FUN = mean)

#EX: Combine data that have timezones
# Create merged object with a Europe/London timezone
tz_london <- merge(london, chicago)
# Look at tz_london structure
str(tz_london)
# Create merged object with a America/Chicago timezone
tz_chicago <- merge(chicago, london)
# Look at tz_chicago structure
str(tz_chicago)

#EX: Make irregular intraday-day data regular
# Create a regular date-time sequence
regular_index <- seq(as.POSIXct("2010-01-04 09:00"), as.POSIXct("2010-01-08 16:00"), by = "30 min")
# Create a zero-width xts object
regular_xts <- xts(, order.by = regular_index)
# Merge irregular_xts and regular_xts, filling NA with their previous value
merged_xts <- merge(irregular_xts, regular_xts, fill = na.locf)
# Subset to trading day (9AM - 4PM)
trade_day <- merged_xts["T09:00/T16:00"]


#EX: Aggregate irregular intraday-day data
# Convert raw prices to 5-second prices
xts_5sec <- to.period(intraday_xts, period = "seconds", k = 5)
# Convert raw prices to 10-minute prices
xts_10min <- to.period(intraday_xts, period = "minutes", k = 10)
# Convert raw prices to 1-hour prices
xts_1hour <- to.period(intraday_xts, period = "hours", k = 1)


#EX: Import well-formatted daily OHLC data
# Load AMZN.csv
getSymbols(Symbols = 'AMZN',src='csv')


#EX： Import text files in other formats
# Import AMZN.csv using read.zoo
amzn_zoo <- read.zoo("AMZN.csv", sep = ",", header = TRUE)
# Convert to xts
amzn_xts <- as.xts(amzn_zoo)
# Look at the first few rows of amzn_xts
head(amzn_xts)
