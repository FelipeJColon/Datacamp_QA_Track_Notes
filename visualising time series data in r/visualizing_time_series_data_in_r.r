# plot() function - basic parameters
# Display the first few lines of the data
head(data)

# Display the column names of the data
colnames(data)

# Plot yahoo data and add title
plot(data$yahoo,main="yahoo")

# Replot yahoo data with labels for X and Y axes
plot(data$yahoo, main="yahoo", xlab = "date", ylab="price")


# plot() function - basic parameters (2)
# Plot the second time series and change title
plot(data$microsoft, main = "microsoft")

# Replot with same title, add subtitle, use bars
plot(data$microsoft, main = "microsoft", sub = "Daily closing price since 2015", type = "h")

# Change line color to red
lines(data$microsoft, col = "red")


# Control graphic parameters
# Plot two charts on same graphical window
par(mfrow =c(2,1))
plot(data$yahoo,main="yahoo")
plot(data$microsoft, main = "microsoft")

# Replot with reduced margin and character sizes
par(mfrow = c(2, 1), mex =0.6, cex = 0.8)
plot(data$yahoo,main="yahoo")
plot(data$microsoft, main = "microsoft")


# Adding an extra series to an existing chart
# Plot the "microsoft" series
plot(data$microsoft, main="Stock prices since 2015")

# Add the "dow_chemical" series in red
lines(data$dow_chemical, col="red")

# Add a Y axis on the right side of the chart
axis(side = 4, at = pretty(data$dow_chemical))

# Add a legend in the bottom right corner
legend(x ="bottomright", legend = c("microsoft","dow_chemical"),
col=c("black", "red"), lty = c(1,1))

# Highlighting events in a time series
# Plot the "citigroup" time series
plot(data$citigroup, main="Citigroup")

# Create vert_line to identify January 4th, 2016 in citigroup
vert_line <- which(index(data$citigroup) == as.Date("2016-01-04"))

vert_line

# Add a red vertical line using vert_line
abline(v = .index(data$citigroup)[vert_line], col = "red")

# Create hori_line to identify average price of citigroup
hori_line <- mean(data$citigroup)

# Add a blue horizontal line using hori_line
abline(h = hori_line, col = "blue")

# Highlighting a specific period in a time series
# Create period to hold the 3 months of 2015
period = c("2015-01/2015-03")

# Highlight the first three months of 2015 
chart.TimeSeries(data$citigroup, period.areas=period)

# Highlight the first three months of 2015 in light grey
chart.TimeSeries(data$citigroup, period.areas=period, period.color="lightgrey")

# A fancy stock chart
# Plot the microsoft series
plot(data$microsoft, main="Dividend date and amount")

# Add the citigroup series
lines(data$citigroup, col="orange", lwd=2)

# Add a new y axis for the citigroup series
axis(side = 4,at =pretty(data$citigroup), col = "orange")

# A fancy stock chart (2)
# Same plot as the previous exercise
plot(data$microsoft, main = "Dividend date and amount")
lines(data$citigroup, col = "orange", lwd = 2)
axis(side = 4, at = pretty(data$citigroup), col = "orange")

# Create the two legend strings
micro <- paste0("Microsoft div. of ", "$0.39"," on ", "15 Nov. 2016")
citi <- paste0("Citigroup div. of ", "$0.16"," on ", "13 Nov. 2016")

# Create the legend in the bottom right corner
legend(x = "bottomright", legend = c(micro, citi), col = c("black", "orange"), lty = c(1, 1))

# ROC() is the function to give daily return on stock 
# Plot Apple's stock price 
plot(data, main="Apple stock price")

# Create a time series called rtn
rtn = ROC(data)

# Plot Apple daily price and daily returns 
par(mfrow =c(1,2))
plot(data)
plot(rtn)

# Histogram of returns
# Create a histogram of Apple stock returns
hist(rtn, main= "Apple stock return distribution", probability=TRUE)

# Add a density line
lines(density(rtn))

# Redraw a thicker, red density line
lines(density(rtn), col ="red", lwd =2)

# Boxplot
# Draw box and whisker plot for the Apple returns
boxplot(rtn)

# Draw a box and whisker plot of a normal distribution
boxplot(rnorm(1000))

# Redraw both plots on the same graphical window
par(mfrow = c(2,1))

boxplot(rtn, horizontal = TRUE)
boxplot(rnorm(1000), horizontal = TRUE)

# ACF indicates the relationship of point n to the previous points
# Draw autocorrelation plot
acf(rtn, main="Apple return autocorrelation")

# Redraw with a maximum lag of 10
acf(rtn, main="Apple return autocorrelation", lag.max=10)

#If the data is normally distributed, the points in the q-q plot follow a straight diagonal line. 
# Create q-q plot
qqnorm(rtn, main = "Apple return QQ-plot")
# Add a red line showing normality
qqline(rtn, col = "red")

# A comprehensive time series diagnostic
# Draw histogram and add red density line
hist(rtn, probability = TRUE)
lines(density(rtn), col = "red")

# Draw box and whisker plot
boxplot(rtn)

# Draw autocorrelogram
acf(rtn)

# Draw q-q plot and add a red line for normality
qqnorm(rtn, col ="red")
qqline(rtn, col ="red")


# Barplot with stacked and grouped chart
# Plot stacked barplot
barplot(portfolio)

# Plot grouped barplot
barplot(portfolio, beside = TRUE)

# Visualizing bivariate relationships
# Draw the scatterplot
plot(x = sp500, y = citi)

# Draw a regression line
lm(citi ~ sp500)
abline(reg= lm(citi ~ sp500), col="red", lwd=2)


# Correlation matrix
# Create correlation matrix using Pearson method
cor(my_data)

# Create correlation matrix using Spearman method
cor(my_data, method="spearman")

# Create scatterplot matrix
pairs(my_data)

# Create upper panel scatterplot matrix
pairs(my_data, lower.panel = NULL)

# Correlation plot
# Create correlation matrix
corrplot(cor_mat)

# Create correlation matrix with numbers
corrplot(cor_mat, method ="number")

# Create correlation matrix with colors
corrplot(cor_mat, method="color")

# Create upper triangle correlation matrix
corrplot(cor_mat, method ="number", type="upper")

# Correlation matrix as heatmap
# Draw heatmap of cor_mat
corrplot(cor_mat, method = "color")

# Draw upper heatmap
corrplot(cor_mat, method = "color", type="upper")

# Draw the upper heatmap with hclust
corrplot(cor_mat, method = "color", type="upper", order ="hclust")

# Current portfolio description
# Plot the portfolio value
plot(data$value, main="Portfolio Value")

# Plot the portfolio return
plot(data$return, main="Portfolio Return")

# Plot a histogram of portfolio return 
hist(data$return, probability =TRUE)

# Add a density line
lines(density(data$return),col="red", lwd=2)

# New stocks description (2)
# Coca-Cola seems to provide the most diversification benefit based on low correlation to the portfolio.
# Draw the scatterplot of gs against the portfolio
plot(x = gs, y= portfolio)

# Add a regression line in red
abline(reg=lm(gs ~ portfolio), col="red", lwd=2)

# Plot scatterplots and regression lines to a 2x2 window
par(mfrow = c(2,2))
plot(x=gs, y=portfolio)
abline(reg=lm(gs ~ portfolio), col="red", lwd=2)
plot(x=ko, y=portfolio)
abline(reg=lm(ko ~ portfolio), col="red", lwd=2)
plot(x=dis, y=portfolio)
abline(reg=lm(dis ~ portfolio), col="red", lwd=2)
plot(x=cat, y=portfolio)
abline(reg=lm(cat ~ portfolio), col="red", lwd=2)


# The new portfolio seems to have less variation based on the density lines.
# Plot new and old portfolio values on same chart
plot(old.vs.new.portfolio$old.portfolio.value)
lines(old.vs.new.portfolio$new.portfolio.value, col="red")

# Plot density of the new and old portfolio returns on same chart
plot(density(old.vs.new.portfolio$old.portfolio.rtn))
lines(density(old.vs.new.portfolio$new.portfolio.rtn), col="red")


# A more accurate comparison of portfolios
# Draw value, return, drawdowns of old portfolio
charts.PerformanceSummary(old.vs.new.portfolio$old.portfolio.rtn)

# Draw value, return, drawdowns of new portfolio
charts.PerformanceSummary(old.vs.new.portfolio$new.portfolio.rtn)

# Draw both portfolios on same chart
charts.PerformanceSummary(old.vs.new.portfolio[,c(3,4)])

