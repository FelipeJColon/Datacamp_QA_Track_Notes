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

# dates
dates <- as.Date(c("2017-01-02", "2017-05-03", "2017-08-04", "2017-10-17"))

months(dates)
quarters(dates)
dates2 <- as.Date(c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05"))
names(dates2) <- weekdays(dates2)

dates2