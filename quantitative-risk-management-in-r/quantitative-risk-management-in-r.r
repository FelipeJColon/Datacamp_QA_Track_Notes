# Load DJ index
data("DJ")

# Show head() and tail() of DJ index
head(DJ)
tail(DJ)

# Plot DJ index
plot(DJ)

# Extract 2008-2009 and assign to dj0809
dj0809 <- DJ["2008/2009"]

# Plot dj0809
plot(dj0809)

# Exploring risk-factor time series: individual equities
# Load DJ constituents data
data("DJ_const")

# Apply names() and head() to DJ_const
names(DJ_const)
head(DJ_const)

# Extract AAPL and GS in 2008-09 and assign to stocks
stocks <- DJ_const["2008/2009",c("AAPL","GS")]

# Plot stocks with plot.zoo()
plot.zoo(stocks)

# Exploring risk-factor data: exchange rates
# Load exchange rate data
data("GBP_USD")
data("EUR_USD")

# Plot the two exchange rates
plot(GBP_USD)
plot(EUR_USD)

# Plot a USD_GBP exchange rate
plot(1/GBP_USD)

# Merge the two exchange rates GBP_USD and EUR_USD
fx <- merge(GBP_USD, EUR_USD, all = TRUE)

# Extract 2010-15 data from fx and assign to fx0015
fx0015 <- fx["2010/2015", ]

# Plot the exchange rates in fx0015
plot.zoo(fx0015)

# Exploring return series
# Compute the log-returns of dj0809 and assign to dj0809_x
dj0809_x <- diff(log(dj0809))

# Plot the log-returns
plot(dj0809_x)

# Compute the log-returns of djstocks and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot the two share returns
plot.zoo(djstocks_x)

# Compute the log-returns of GBP_USD and assign to erate_x
erate_x <- diff(log(GBP_USD))

# Plot the log-returns
plot(erate_x)

# Different ways of plotting risk-factor and return series
# Plot djstocks in four separate plots
plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks, plot.type = "single", col = c(1:4))
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x, type = "h")


# Aggregating log-return series
# Plot djx
plot(djx)

# Plot weekly log-returns of djx
plot(apply.weekly(djx,sum), type = "h")

# Plot monthly log-returns of djx
plot(apply.monthly(djx,sum), type = "h")

# Plot djreturns
plot.zoo(djreturns)

# Plot monthly log-returns of djreturns
plot.zoo(apply.monthly(djreturns, colSums),type="h")

# Interest-rate data
# Compute log-returns as zcb_x and simple returns as zcb_x2
zcb_x <- diff(log(zcb))
zcb_x2 <- diff(zcb)

# Plot zcb_x for 1, 5 and 10-year maturities
plot.zoo(zcb_x[,yield_cols])

# Plot zcb_x2 for 1, 5 and 10-year maturities
plot.zoo(zcb_x2[,yield_cols])

# Plot the yield curve for the first day of zcb
plot(maturity, zcb[1,], ylim = range(zcb), type = "l", ylab = "yield (%)", col = "red")

# Add a line for the last day of zcb
lines(maturity, zcb[nrow(zcb),])