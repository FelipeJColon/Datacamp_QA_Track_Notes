# Load the package
library(PortfolioAnalytics)

# Load the data
data(indexes)

# Subset the data
index_returns = indexes[, 1:4]

# Print the head of the data
head(index_returns)

# Solve a simple portfolio optimization problem
# Create the portfolio specification
# Academically, risk and gain are defined as standard deviation and mean return. 
# In practice, many other measures of risk and gain are also used!
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns, portfolio = port_spec, optimize_method = "ROI")

# Visualize results
# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Chart the optimal weights
chart.Weights(opt)

# Maximize quadratic utility function
# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to maximize portfolio mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add an objective to minimize portfolio variance , ES = portfolio expected shortfall
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Solve the optimization problem
opt <- optimize.portfolio(R = index_returns, portfolio = port_spec, optimize_method = "ROI")

# Create a portfolio specification
# Get the column names of the returns data
asset_names <- colnames(asset_returns)

# Create a portfolio specification object using asset_names
port_spec <- portfolio.spec(assets= asset_names)

# Get the class of the portfolio specification object
class(port_spec)

# Print the portfolio specification object
print(port_spec)

# Add constraints
# Add the weight sum constraint
port_spec <- add.constraint(portfolio = port_spec, type = "weight_sum", min_sum = 1, max_sum = 1)

# Add the box constraint
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = c(0.1,0.1,0.1,0.1,0.1,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05), max = 0.4)

# Add the group constraint
port_spec <- add.constraint(portfolio = port_spec, type = "group", groups = list(c(1,5,7,9,10,11), c(2,3,4,6,8,12)), group_min = 0.4, group_max = 0.6)

# Print the portfolio specification object
print(port_spec)

# Add objectives
# Add a return objective to maximize mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add a risk objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Add a risk budget objective
port_spec <- add.objective(portfolio = port_spec, type = "risk_budget", name = "StdDev", min_prisk = 0.05, max_prisk = 0.1)

# Print the portfolio specification object
print(port_spec)

# Single-Period optimization
# Run a single period optimization using random portfolios as the optimization method
opt <- optimize.portfolio(R = asset_returns , portfolio = port_spec, optimize_method = "random", rp = rp, trace = TRUE)

# Print the output of the single-period optimization
print(opt)

# Optimization with periodic rebalancing
# Run the optimization backtest with quarterly rebalancing
opt_rebal <- optimize.portfolio.rebalancing(R = asset_returns, portfolio = port_spec, optimize_method = "random", rp = rp, trace = TRUE, search_size = 1000, rebalance_on = "quarters", training_period = 60, rolling_window = 60)

# Print the output of the optimization backtest
print(opt_rebal)

# Objective measure values
# Extract the objective measures for the single period optimization
extractObjectiveMeasures(opt)

# Extract the objective measures for the optimization backtest
extractObjectiveMeasures(opt_rebal)

# Sample moment estimates
# Add a return objective with "mean" as the objective name
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Calculate the sample moments
moments <- set.portfolio.moments(R = asset_returns, portfolio = port_spec)

# Check if moments$mu is equal to the sample estimate of mean returns
moments$mu == colMeans(asset_returns)

# Add a risk objective with "StdDev" as the objective name
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Calculate the sample moments using set.portfolio.moments. Assign to a variable named moments.
moments <- set.portfolio.moments(R = asset_returns, portfolio = port_spec)

# Check if moments$sigma is equal to the sample estimate of the variance-covariance matrix
moments$sigma == cov(asset_returns)

# Advanced moment estimates
# Print the portfolio specification object
print(port_spec)

# Fit a statistical factor model to the asset returns
fit <- statistical.factor.model(R = asset_returns, k = 3)

# Estimate the portfolio moments using the "boudt" method with 3 factors
moments_boudt <- set.portfolio.moments(R = asset_returns, portfolio = port_spec, method = "boudt", k = 3)

# Check if the covariance matrix extracted from the model fit is equal to the estimate in `moments_boudt`
moments_boudt$sigma == extractCovariance(fit)

# Define a custom moment function
# Define custom moment function
moments_robust <- function(R, portfolio){
  out <- list()
  out$sigma <- cov.rob(R, method = "mcd")$cov
  out
}

# Estimate the portfolio moments using the function you just defined 
moments <- moments_robust(R = asset_returns, portfolio = port_spec)

# Compute the variance-covariance matrix  directly using cov.rob()
# Check the moment estimate
cov.rob(asset_returns, method = "mcd")$cov == moments$sigma

# Optimization with custom moment function
# Run the optimization with custom moment estimates
opt_custom <- optimize.portfolio(R = asset_returns, portfolio = port_spec, optimize_method = "random", rp = rp, momentFUN = "moments_robust")

# Print the results of the optimization with custom moment estimates
print(opt_custom)

# Run the optimization with sample moment estimates
opt_sample <- optimize.portfolio(R = asset_returns, portfolio = port_spec, optimize_method = "random", rp = rp)

# Print the results of the optimization with sample moment estimates
print(opt_sample)

# Custom objective function
# Custom annualized portfolio standard deviation
pasd <- function(R, weights, sigma, scale = 12){
  sqrt(as.numeric(t(weights) %*% sigma %*% weights)) * sqrt(scale)
}

# Real world example
# Define the portfolio optimization problem
# Create the portfolio specification
port_spec <- portfolio.spec(colnames(asset_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name ="StdDev")

# Print the portfolio specification
print(port_spec)


# Backtest with periodic rebalancing
# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                 portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 60,
                                                 rolling_window = 60)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)

# Compute the portfolio returns
returns_base <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base"

# Refine constraints and objectives
# Add a risk budge objective
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = 0.05, 
                           max_prisk = 0.1)

# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = asset_returns, 
                                               portfolio = port_spec, 
                                               optimize_method = "random", rp =rp,
                                               trace = TRUE,
                                               rebalance_on = "quarters", 
                                               training_period = 60,
                                               rolling_window = 60)

# Chart the weights
chart.Weights(opt_rebal_rb)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_rb))
colnames(returns_rb) <- "risk_budget"