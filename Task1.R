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

ts_data_1 <- ts_data[1]

plot(ts_data[, 9], ts_data[, 7], type="hist",)
hist(ts_data[, 7], xlab= "Price", main = "Frequesncy of price")
hist(ts_data[, 6], xlab= "Quantity", main = "Frequesncy of quantity")
hist(ts_data[, 6], xlab= "Produuuct category", main = "Frequesncy of product category")

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


# Load required libraries
library(dplyr)
library(tidyr)

# Create a data frame with your x and y data
data <- data.frame(
  x1 = x,
  x2 = x,
  x3 = x,
  x4 = x,
  y = y
)

# Define the candidate models with estimator variables
model1 <- lm(y ~ poly(x4, 4, raw = TRUE) * θ1 + poly(x1, 2, raw = TRUE) * θ2 +
               poly(x1, 3, raw = TRUE) * θ3 + poly(x2, 4, raw = TRUE) * θ4 +
               poly(x1, 4, raw = TRUE) * θbias + ε, data = data)

model2 <- lm(y ~ poly(x4, 4, raw = TRUE) * θ1 + poly(x1, 3, raw = TRUE) * θ2 +
               poly(x3, 4, raw = TRUE) * θ3 + ε, data = data)

model3 <- lm(y ~ poly(x3, 3, raw = TRUE) * θ1 + poly(x3, 4, raw = TRUE) * θ2 + ε, data = data)

model4 <- lm(y ~ poly(x2, 2, raw = TRUE) * θ1 + poly(x1, 3, raw = TRUE) * θ2 +
               poly(x3, 4, raw = TRUE) * θ3 + ε, data = data)

model5 <- lm(y ~ poly(x4, 4, raw = TRUE) * θ1 + poly(x1, 2, raw = TRUE) * θ2 +
               poly(x1, 3, raw = TRUE) * θ3 + poly(x3, 4, raw = TRUE) * θ4 +
               θbias + ε, data = data)

# Create a data frame to store the coefficients for each model
coefficients_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  θ1 = c(coef(model1)["poly(x4, 4, raw = TRUE)θ1"], coef(model2)["poly(x4, 4, raw = TRUE)θ1"], coef(model3)["poly(x3, 3, raw = TRUE)θ1"], coef(model4)["poly(x2, 2, raw = TRUE)θ1"], coef(model5)["poly(x4, 4, raw = TRUE)θ1"]),
  θ2 = c(coef(model1)["poly(x1, 2, raw = TRUE)θ2"], coef(model2)["poly(x1, 3, raw = TRUE)θ2"], coef(model3)["poly(x3, 3, raw = TRUE)θ2"], coef(model4)["poly(x1, 3, raw = TRUE)θ2"], coef(model5)["poly(x1, 2, raw = TRUE)θ2"]),
  θ3 = c(coef(model1)["poly(x1, 3, raw = TRUE)θ3"], coef(model2)["poly(x3, 4, raw = TRUE)θ3"], NA, coef(model4)["poly(x3, 4, raw = TRUE)θ3"], coef(model5)["poly(x1, 3, raw = TRUE)θ3"]),
  θ4 = c(coef(model1)["poly(x2, 4, raw = TRUE)θ4"], NA, NA, NA, coef(model5)["poly(x3, 4, raw = TRUE)θ4"]),
  θbias = c(coef(model1)["poly(x1, 4, raw = TRUE)θbias"], coef(model2)["(Intercept)"], coef(model3)["(Intercept)"], coef(model4)["(Intercept)"], coef(model5)["θbias"])
)

# Calculate RSS for each model
rss_values <- c(
  sum(model1$residuals^2),
  sum(model2$residuals^2),
  sum(model3$residuals^2),
  sum(model4$residuals^2),
  sum(model5$residuals^2)
)

# Create a data frame to store the RSS for each model
rss_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RSS = rss_values
)

# Print the RSS for each model
print(rss_df)


# Calculate R-squared values for each model
rsquared_values <- c(
  summary(model1)$r.squared,
  summary(model2)$r.squared,
  summary(model3)$r.squared,
  summary(model4)$r.squared,
  summary(model5)$r.squared
)

# Print the coefficients table
print(coefficients_df)

