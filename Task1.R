library(ggplot2)
library(xts) # For time series manipulation

#windows()  # On Windows

#Task 1: Preliminary data analysis

#read customer shopping data 
data <- read.csv("./customer_shopping_data.csv")

#check for missing data
missing_data <- colSums(is.na(data))
print(missing_data)
#no missing data found


### Time series plots ###

# Convert invoice_date to datetime
data$invoice_date <- as.Date(data$invoice_date, format = "%d/%m/%y")
data <- data[order(data$invoice_date), ]

# Create an xts time series objectts_data <- ts(data, order.by = data$invoice_date)
ts_data <- ts(data, start=c(2020, 1, 1), frequency = 365)

plot(ts_data)

# Extract the dates from the time series object
dates <- time(ts_data)

# Create the time series plot with date labels
plot(dates, ts_data, type = "l", xlab = "Date", ylab = "Value")

# Optionally, you can format the date labels for better readability
axis.Date(side = 1, at = dates, format = "%Y-%m-%d")

#ts_data <- ts(ts_values, start = c(year(data$invoice_date[1]), month(data$invoice_date[1])), frequency = 1)

#ndvi_series <- xts(data$ndvi_value, order.by = data$invoice_date)

plot.xts(ts_data, main = "Invoice Date Time Series", xlab = "Date", ylab = "Value", type = "l")

#total accumulated price and quantity
data$accumulated_price <- cumsum(data$price)
data$total_sale_quantity <- cumsum(data$quantity)

#Summary Statistics
summary(data)

# Time series plot of total sales quantity (y)
plot(data$invoice_date, data$total_sales_quantity, xlab="Date", ylab="Total Sales Quantity",  main="Time Series Plot of Total Sales Quantity")

# Time series plot of quantity
plot(data$invoice_date, data$quantity, xlab="Date", ylab="Quantity", main="Time Series Plot of Quantity")


# Time series plot of price
plot(data$invoice_date, data$accumulated_price, xlab = "Date", ylab = "Price", main = "Time Series Plot of Price")


X <- data$quantity
y <- data$price
XTX <- t(X) %*% X
XTX_inverse <- solve(XTX)
XT_y <- t(X) %*% y
theta_hat <- XTX_inverse %*% XT_y
print(theta_hat)


# quantity as matrix
X <- ts_data[, 6]

# Select all columns except the 7th column
X <- ts_data[, -7]

#price as matrix
y <- ts_data[, 7]

XTX <- t(X) %*% X
XTX_inverse <- solve(XTX)
XT_y <- t(X) %*% y
theta_hat_series <- XTX_inverse %*% XT_y

# Plot or print the result
print(theta_hat_series)

plot(theta_hat_series)