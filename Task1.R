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
ts_data_1 <- ts_data[, 1]

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


# Define a function to calculate the log-likelihood for a given model
calculate_log_likelihood <- function(model) {
  n <- length(model$residuals)
  sigma_sq <- sum(model$residuals^2) / (n - length(model$coefficients))
  log_likelihood <- -n/2 * log(2 * pi * sigma_sq) - sum(model$residuals^2) / (2 * sigma_sq)
  return(log_likelihood)
}

# Calculate log-likelihood for each model
log_likelihood_values <- c(
  calculate_log_likelihood(model1),
  calculate_log_likelihood(model2),
  calculate_log_likelihood(model3),
  calculate_log_likelihood(model4),
  calculate_log_likelihood(model5)
)

# Create a data frame to store the log-likelihood for each model
log_likelihood_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  LogLikelihood = log_likelihood_values
)

# Print the log-likelihood for each model
print(log_likelihood_df)


# Calculate AIC for each model
aic_values <- c(
  AIC(model1),
  AIC(model2),
  AIC(model3),
  AIC(model4),
  AIC(model5)
)

# Create a data frame to store the AIC for each model
aic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  AIC = aic_values
)

# Print the AIC for each model
print(aic_df)


# Calculate BIC for each model
bic_values <- c(
  BIC(model1),
  BIC(model2),
  BIC(model3),
  BIC(model4),
  BIC(model5)
)

# Create a data frame to store the BIC for each model
bic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  BIC = bic_values
)

# Print the BIC for each model
print(bic_df)

# Make predictions using each model
predictions1 <- predict(model1)
predictions2 <- predict(model2)
predictions3 <- predict(model3)
predictions4 <- predict(model4)
predictions5 <- predict(model5)

# Calculate prediction errors
errors1 <- y - predictions1
errors2 <- y - predictions2
errors3 <- y - predictions3
errors4 <- y - predictions4
errors5 <- y - predictions5

# Create histograms to visualize the distribution of errors
histogram1 <- ggplot(data.frame(Errors = errors1), aes(x = Errors)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Model 1 Prediction Errors", x = "Prediction Errors")

histogram2 <- ggplot(data.frame(Errors = errors2), aes(x = Errors)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Model 2 Prediction Errors", x = "Prediction Errors")

histogram3 <- ggplot(data.frame(Errors = errors3), aes(x = Errors)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Model 3 Prediction Errors", x = "Prediction Errors")

histogram4 <- ggplot(data.frame(Errors = errors4), aes(x = Errors)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Model 4 Prediction Errors", x = "Prediction Errors")

histogram5 <- ggplot(data.frame(Errors = errors5), aes(x = Errors)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Model 5 Prediction Errors", x = "Prediction Errors")

# Display the histograms
grid.arrange(histogram1, histogram2, histogram3, histogram4, histogram5, ncol = 2)

