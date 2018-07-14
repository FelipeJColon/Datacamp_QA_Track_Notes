# Date

today <- Sys.Date()
class(today)

# There are a number of different formats you can specify, here are a few of them:

# %Y: 4-digit year (1982)
# %y: 2-digit year (82)
# %m: 2-digit month (01)
# %d: 2-digit day of the month (13)
# %A: weekday (Wednesday)
# %a: abbreviated weekday (Wed)
# %B: month (January)
# %b: abbreviated month (Jan)

# A few examples
as.Date("08,30,1930", format = "%m,%d, %Y")
as.Date("Aug 30,1930", format = "%b %d, %Y")
as.Date("30aug1930", format = "%d%b%Y")
# using format
char_dates <- c("1jan17", "2jan17", "3jan17", "4jan17", "5jan17")

dates <- as.Date(char_dates, format = "%d%b%y")
format(dates, format = "%b %d, %y")
format(dates, format = "%m,%d,%Y")

#Find out the number of days since origin
dates <- as.Date(c("2017-01-01", "2017-01-02", "2017-01-03"))
origin <- as.Date("1970-01-01")
as.numeric(dates)

#Extract month quarters and dates
dates <- as.Date(c("2017-01-02", "2017-05-03", "2017-08-04", "2017-10-17"))
months(dates)
quarters(dates)
dates2 <- as.Date(c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05"))
names(dates2) <- weekdays(dates2)

#Operator comparison
stocks$ibm_buy <- stocks$ibm <175
stocks$panera_sell <- stocks$panera > 213 
stocks$ibm_vs_panera <- stocks$ibm>stocks$panera

#Specify the price range and dates to trade
stocks$ibm_buy_range <- stocks$ibm < 176 & stocks$ibm >171
stocks$panera_spike <- stocks$panera < 213.2 | stocks$panera > 216.5
stocks$good_dates <- as.Date(stocks$date) > as.Date("2017-01-21") & as.Date(stocks$date) < as.Date("2017-01-25")

#Examples of using subset and logical operator to select data
subset(stocks, panera > 216)
subset(stocks, date == as.Date("2017-01-23"))
subset(stocks, ibm < 175 & panera < 216.5)

#Filter out N/A data
stocks$weekday <- weekdays(stocks$date)
stocks_no_NA <- subset(stocks, !is.na(apple))
subset(stocks_no_NA, apple >117 | micr >63)

#If else example
micr <- 57.44

if( micr < 55 ) {
    print("Buy!")
} else {
    print("Do nothing!")
}
# Ternary ifelse statement, ifelse() strips the date of its attribute before returning it, so it becomes a numeric.
stocks$apple_date <- ifelse(test = stocks$apple >117, yes = stocks$date, no = NA)
# Change the class() of apple_date.
class(stocks$apple_date) <- "Date"

#Example of a loop
stock_price <- 126.34
repeat {
  stock_price <- stock_price * runif(1, .985, 1.01)
  print(stock_price)
 
  if(stock_price < 125) {
    print("Stock price is below 124.5! Buy it while it's cheap!")
    break
  }
}

#Example of a while loop
debt <- 5000    # initial debt
i <- 0          # x axis counter
x_axis <- i     # x axis
y_axis <- debt  # y axis

plot(x_axis, y_axis, xlim = c(0,10), ylim = c(0,5000))

while (debt > 0) {

  debt <- debt - 500
  i <- i + 1
  x_axis <- c(x_axis, i)
  y_axis <- c(y_axis, debt)
  
  plot(x_axis, y_axis, xlim = c(0,10), ylim = c(0,5000))
}

# For Loop 
# Loop over stock rows
for (row in 1:nrow(stock)) {
    price <- stock[row, "apple"]
    date  <- stock[row, "date"]

    if(price > 116) {
        print(paste("On", date, 
                    "the stock price was", price))
    } else {
        print(paste("The date:", date, 
                    "is not an important day!"))
    }
}

# Nested For loop
corr # 3 X 3 matrix

for(row in 1:nrow(corr)) {
    for(col in 1:ncol(corr)) {
        print(paste(colnames(corr)[col], "and", rownames(corr)[row], 
                    "have a correlation of", corr[row,col]))
    }
}

#tidyquant to get stock price data
library(tidyquant)

# Pull Apple stock data
apple <- tq_get("AAPL", get = "stock.prices", 
                from = "2007-01-03", to = "2017-06-05")

head(apple)

plot(apple$date, apple$adjusted, type = "l")

apple <- tq_mutate(data = apple,
                   ohlc_fun = Ad,
                   mutate_fun = dailyReturn)

sorted_returns <- sort(apple$daily.returns)

plot(sorted_returns)

#Example of lapply
#stock_return is a list and percent_to_decimal is a function
lapply(stock_return, FUN = percent_to_decimal)


#stock_return is a dataframe
stock_return 
lapply(stock_return, FUN = mean)

sharpe <- function(returns) {
    (mean(returns) - .0003) / sd(returns)
}

lapply(stock_return, FUN = sharpe)

#Example of sapply
sapply(stock_return, FUN = sharpe)
sapply(stock_return,FUN = sharpe, simplify = FALSE, USE.NAMES = FALSE)

#Example of annoymous function
vapply(stock_return, 
       FUN = function(x) { c(max(x),min(x)) }, 
       FUN.VALUE = numeric(2))
