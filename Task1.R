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
#ts_data_1 <- ts_data[, 1]

# Assuming you have a dataframe named 'data' with the variables 'x1', 'x2', 'x3', 'x4', and 'y'
correlation_matrix <- cor(ts_data)

# Print the correlation matrix
print(correlation_matrix)

library(gplots)

# Create a heatmap of the correlation matrix with values
heatmap.2(correlation_matrix, 
          col = colorRampPalette(c("blue", "white", "red"))(20), # Choose a color palette
          trace = "none", # Turn off row/column labels
          main = "Correlation Heatmap with Values",
          key.title = NA, # Turn off the legend title
          key = TRUE, # Display the color key
          density.info = "none", # Turn off density plot
          cellnote = round(correlation_matrix, 2), # Display correlation values
          notecol = "black", # Set text color for correlation values
          margins = c(10, 10) # Add margins to the plot
)

correlation_matrix <- cor(ts_data)

# Print the correlation matrix
print(correlation_matrix)

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
df <- data.frame(
  x1 = ts_data[, "age"],
  x2 = ts_data[, "category"],
  x3 = ts_data[, "price"],  # You mentioned "price" in your original code, so I included it here as an example
  x4 = ts_data[, "payment_method"],  # You mentioned "payment_method" in your original code
  y = ts_data[, "quantity"]  # You mentioned "quantity" in your original code
)

# If ts_data is a data frame
#summary(df)

#modals
# Define the candidate models
model1 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = df)

model2 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model4 <- lm(y ~ poly(x2, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model5 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x3, 4, raw = TRUE), data = df)

### Task 2.1 ###

# Create a list to store the estimated parameters for each model
estimated_parameters_list <- list(
  Model1 = coef(model1),
  Model2 = coef(model2),
  Model3 = coef(model3),
  Model4 = coef(model4),
  Model5 = coef(model5)
)

# Create a function to extract specific coefficients
extract_coefficients <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x4, 4, raw = TRUE)1"]
  coef_list$θ2 <- parameters["poly(x1, 3, raw = TRUE)1"]
  coef_list$θ3 <- parameters["poly(x3, 4, raw = TRUE)1"]
  coef_list$θ4 <- parameters["poly(x2, 2, raw = TRUE)1"]
  coef_list$θbias <- parameters["(Intercept)"]
  return(coef_list)
}

# Create a DataFrame to store coefficients for each model
coefficients_df <- data.frame(
  Model = character(0),
  θ1 = numeric(0),
  θ2 = numeric(0),
  θ3 = numeric(0),
  θ4 = numeric(0),
  θbias = numeric(0)
)

# Loop through each model's estimated parameters
for (model_name in names(estimated_parameters_list)) {
  parameters <- estimated_parameters_list[[model_name]]
  coefficients <- extract_coefficients(parameters)
  
  # Add coefficients to the DataFrame
  coefficients_df <- rbind(coefficients_df, cbind(Model = model_name, as.data.frame(t(coefficients))))
}

# Print the coefficients DataFrame
print(coefficients_df)



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

#task 2.4
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

