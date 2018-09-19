# Computing a bond's future value
# Create pv
pv <- 100

# Create r
r <- 0.1

# Calculate fv1
fv1 <- pv * (1 + r)

# Calculate fv2
fv2 <- fv1 * (1 + r)

# Computing a bond's present value
# Calculate pv1
pv1 <- fv1 / (1 + r)

# Calculate pv2
pv2 <- fv2/ (1+r) ** 2

#Laying out cash flow
# Create vector of cash flows
cf <- c(5,5,5,5,105)

# Convert to data frame
cf <- data.frame(cf)

# Discounting bond cash flows with a known yield
# Add column t to cf
cf$t <- as.numeric(rownames(cf))

# Calculate pv_factor
cf$pv_factor <- 1 / (1 + 0.06)^cf$t

# Calculate pv
cf$pv <- cf$cf * cf$pv_factor

# Calculate the bond price
sum(cf$pv)

# Convert your code into a bond valuation function
# Create function
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

# Verify prior result
bondprc(95.79, 0.05, 5, 0.06)

# Load Quandl package
library(Quandl)

# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")

# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$Date == "2016-09-30")

# Convert yield to decimals and view
baa_yield <- baa_yield$Value / 100

# Plotting the Price/Yield relationship
# Generate prc_yld
prc_yld <- seq(0.02, 0.4, 0.01)

# Convert prc_yld to data frame
prc_yld <- data.frame(prc_yld)

# Calculate bond price given different yields
for (i in 1:nrow(prc_yld)) {
     prc_yld$price[i] <- bondprc(100, 0.10, 20, prc_yld$prc_yld[i])  
}

# Plot P/YTM relationship
plot(prc_yld,
     type = "l",
     col = "blue",
     main = "Price/YTM Relationship")

# Plotting US Treasury yields
# Load quantmod package
library(quantmod)

# Obtain Treasury yield data
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)

# Subset data
t10yr <- t10yr["2006-01/2016-09"]

# Plot yields
plot(x = index(t10yr),
    y = t10yr$DGS10,
    xlab = "Date",
    ylab = "Yield (%)",
    type = "l",
    col = "red",
    main = "10-Year US Treasury Yields")

# Plotting the investment grade spread
# Examine first and last six elements in spread
head(spread)
tail(spread)

# Calculate spread$diff
spread$diff <- (spread$baa - spread$aaa) * 100

# Plot spread
plot(x = spread$date,
    y = spread$diff,
    type = "l",
    xlab = "Date",
    ylab = "Spread (bps)",
    col = "red",
    main = "Baa - Aaa Spread")


# Finding a Bond's Yield
# Value bond using 5% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.05)

# Value bond using 7% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.07)

# Value bond using 6% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.06)

# Use uniroot function to find YTM
# Create cash flow vector
cf <- c(-95.79, 5, 5, 5, 5, 105)

# Create bond valuation function
bval <- function(i, cf,
     t=seq(along = cf))
sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
    uniroot(bval, c(0, 1), cf = cf)$root
}

ytm(cf)

# Calculate the PV01
bondprc(100, 0.1, 20, 0.1)
abs(bondprc(100, 0.1, 20, 0.1001) - bondprc(100, 0.1, 20, 0.1))

# Approximate Duration
# Calculate bond price today
px <- bondprc(p = 100, r = 0.1, ttm = 20, y = 0.1)
px

# Calculate bond price if yields increase by 1%
px_up <- bondprc(p = 100, r = 0.1, ttm = 20, y = 0.11)
px_up

# Calculate bond price if yields decrease by 1%
px_down <- bondprc(p = 100, r = 0.1, ttm = 20, y = 0.09)
px_down

# Calculate approximate duration
duration <- (px_down - px_up) / (2 * px * 0.01)

# Estimating effect on bond price using duration
# Estimate percentage change
duration_pct_change <- -8.545937 * -0.01

# Estimate dollar change
duration_dollar_change <- duration_pct_change * px

# Calculate approximate convexity for a bond
# Calculate approximate convexity
convexity <- (px_down + px_up - 2 * px) / (px * (0.01)^2)

# Chapter 4: Find AAA bond yields as of September 30, 2016
# Load Quandl package
library(Quandl)

# Obtain Moody's Aaa yield
aaa <- Quandl("FED/RIMLPAAAR_N_M")

# identify yield on September 30, 2016
aaa_yield <- subset(aaa, aaa$Date == "2016-09-30")

# Convert yield into decimals
aaa_yield <- as.numeric(aaa_yield$Value)/100
aaa_yield

# Bond valuation
# Layout the bond's cash flows
cf <- c(3, 3, 3, 3, 3, 3, 3,103)

# Convert to data.frame
cf <- data.frame(cf)

# Add time indicator
cf$t <- seq(1, 8, 1)

# Calculate PV factor
cf$pv_factor <- 1 / (1 + aaa_yield)^cf$t

# Calculate PV
cf$pv <- cf$cf * cf$pv_factor

# Price bond
sum(cf$pv)

# Alternative Cash Flow Vector Code
# Code cash flow function
alt_cf <- function(r, p, ttm) {
  c(rep(p * r, ttm - 1), p * (1 + r))
}

# Generate cf vector
alt_cf(r = 0.03, p = 100, ttm = 8)

# Calculate bond price when yield increases
px_up <- bondprc(p = 100, r = 0.03, ttm = 8, y = aaa_yield + 0.01)

# Calculate bond price when yield decreases
px_down <- bondprc(p = 100, r = 0.03, ttm = 8, y = aaa_yield - 0.01)

# Calculate duration
duration <- (px_down - px_up) / (2 * px * 0.01)

# Calculate percentage effect of duration on price
duration_pct_change <- -duration * 0.01
duration_pct_change

# Calculate dollar effect of duration on price
duration_dollar_change <- duration_pct_change * px
duration_dollar_change

# Calculate convexity measure
# Calculate convexity measure
convexity <- (px_up + px_down - 2 * px)/(px * 0.01^2)

# Calculate percentage effect of convexity on price
convexity_pct_change <- 0.5 * convexity * 0.01^2
convexity_pct_change

# Calculate dollar effect of convexity on price
convexity_dollar_change <- convexity_pct_change * px
convexity_dollar_change

# Estimate price_change
price_change <- convexity_dollar_change + duration_dollar_change
price_change

# Estimate new_price
new_price <- convexity_dollar_change + duration_dollar_change + px
new_price





