# Plot the data as a line graph, by default it is dot
plot(apple_stock, type = "l")

#head and tail to show the first/last few rows
head(ranking, n = 5)
tail(ranking, n = 3)

#Assign name to the vector
names(ret) <- c("Jan", "Feb")

# cbind the vectors together by column
cbind_stocks <- cbind(apple, ibm,micr) 

# rbind the vectors together by row
rbind_stocks <- rbind(apple, ibm, micr)

#find out correlation between Apple and IBM
cor(apple, ibm)

# cor() of all three stocks, 
# cor() func cant analyse more than two vectors, 
# need to bind them into a matrix first
stocks <- cbind(apple, micr, ibm) 
cor(stocks)


# Create a dataframe with different variable types
company <- c("A", "A", "A", "B", "B", "B", "B")
cash_flow <- c(1000, 4000, 550, 1500, 1100, 750, 6000)
year <- c(1, 3, 4, 1, 2, 4, 5)

cash <- data.frame(company, cash_flow, year)

#choose a column in a year
cash$year

# creating a new column quarter_cash based on current column cash_flow
cash$quarter_cash <- cash$cash_flow * 0.25

# preset value - general formula
present_value <- cash_flow * (1 + interest / 100) ^ -periods

# Present value of all cash flows element-wise calculation for all cash flows
cash$present_value <- cash$cash_flow * (1 + 0.05) ^ -cash$year

# Factor 
# Create a Factor 
ans <- c("stock", "bond", "bond", "stock")
investment <- factor(ans)

#find out the type of integer
class(investment)

#print out the integer represetation of the factor
as.integer(investment)

#print out the string represetation
levels(investment)

# Rename the levels of credit_factor
levels(credit_factor) <- c("2A", "3A", "1B", "2B","3C")

#plot a bar graph base on factor
plot(credit_factor)

# Create 4 buckets for AAA_rank using cut()
# Rename the levels 
# Plot AAA_factor
AAA_factor <- cut(x = AAA_rank, breaks = c(0,25,50,75,100))

levels(AAA_factor) <- c("low","medium","high","very_high")

plot(AAA_factor)

#Created ordered factor
credit_factor_ordered <- factor(credit_rating, ordered = TRUE, levels = c("AAA","AA","BB","B","CCC"))
#Drop it completely from the levels
drop_level <- credit_factor[c(-3,-7), drop = TRUE]
plot(drop_level)

#stringsAsFactors
credit_rating <- c("AAA", "A", "BB")
bond_owners <- c("Dan", "Tom", "Joe")
bonds <- data.frame(credit_rating, bond_owners, stringsAsFactors = FALSE)
bonds$credit_factor <- factor(bonds$credit_rating, ordered = TRUE, levels = c("AAA","A","BB"))

#Get item from the list 
portfolio[c(2,3)]
portfolio$correlation

#Modifying value from the list
portfolio$weight <- c(apple = 0.3, ibm = 0.7)

#Remove value from the list
portfolio$microsoft <- NULL

#split list based on grouping, it's called split-apply-combine
grouping <- cash$year
split_cash <- split(cash,grouping)
split_cash$A$cash_flow <- 0
cash_no_A <- unsplit(split_cash, grouping)

#concatanate string
quandl_code = paste("FRED", series_name, sep="/")