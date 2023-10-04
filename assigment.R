library(ggplot2)
library(dplyr)


#### Task 1 #####

#read customer shopping data 
data <- read.csv("./customer_shopping_data.csv")

#print if there are any missing data
print(colSums(is.na(data)))

# Convert invoice_date to datetime
data$invoice_date <- as.Date(data$invoice_date, format = "%d/%m/%Y")
data <- data[order(data$invoice_date), ]


#convert data into a time-series
ts_data <- ts(data, start=c(2020, 1, 1), frequency = 365)


#### Task 2 ####



#task 2: Regression – modeling the relationship between sales data
# Create a data frame with your x and y data
df <- data.frame(
  x1 = ts_data[, "age"],
  x2 = ts_data[, "category"],
  x3 = ts_data[, "price"],
  x4 = ts_data[, "shopping_mall"],
  y = ts_data[, "quantity"]
)
plot(df)

cor(df)

#create 5 modal formula

model1 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = df)

model2 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model4 <- lm(y ~ poly(x2, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model5 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x3, 4, raw = TRUE), data = df)


#Task 2.1: Estimate modal parameters 𝜃for every candidate modal using least squre

# Estimate model parameters using the least squares formula
#theta_hat_model1 <- solve(t(model.matrix(model1)) %*% model.matrix(model1)) %*% t(model.matrix(model1)) %*% df$y
#theta_hat_model2 <- solve(t(model.matrix(model2)) %*% model.matrix(model2)) %*% t(model.matrix(model2)) %*% df$y
#theta_hat_model3 <- solve(t(model.matrix(model3)) %*% model.matrix(model3)) %*% t(model.matrix(model3)) %*% df$y
#theta_hat_model4 <- solve(t(model.matrix(model4)) %*% model.matrix(model4)) %*% t(model.matrix(model4)) %*% df$y
#theta_hat_model5 <- solve(t(model.matrix(model5)) %*% model.matrix(model5)) %*% t(model.matrix(model5)) %*% df$y

##Alternatve

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
plot(coefficients_df)



### Task 2.2 ###
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


### Task 2.3 ###

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
plot(log_likelihood_df)

### task 2.3 ###


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
plot(bic_values)

### task 2.4 ###

# Make predictions using each model
predictions1 <- predict(model1)
predictions2 <- predict(model2)
predictions3 <- predict(model3)
predictions4 <- predict(model4)
predictions5 <- predict(model5)

# Calculate prediction errors
errors1 <- df$y - predictions1
errors2 <- df$y - predictions2
errors3 <- df$y - predictions3
errors4 <- df$y - predictions4
errors5 <- df$y - predictions5

# Create a list of error vectors
error_list <- list(errors1, errors2, errors3, errors4, errors5)

# Create a function to generate Q-Q plots
plot_qq <- function(errors, model_name) {
  qqnorm(errors, main = paste("Q-Q Plot for", model_name))
  qqline(errors, col = "red")
}

# Create a layout for the plots
layout(matrix(1:5, nrow = 1))

# Generate and display Q-Q plots for each model
for (i in 1:5) {
  plot_qq(error_list[[i]], model_name = paste("Model", i))
}


### Task 2.5 ###

# Calculate means of errors for each model
mean_errors <- c(mean(errors1), mean(errors2), mean(errors3), mean(errors4), mean(errors5))

# Create a table
result_table <- data.frame(
  Model = paste("Model", 1:5),
  Mean_Error = mean_errors,
  AIC = aic_values,
  BIC = bic_values
)

# Print the result table
print(result_table)

### Task 2.6 ###
# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing datasets (70% train, 30% test)
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit the "best" model (Model 4) using the training data
best_model <- lm(y ~ poly(x2, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = train_data)

# Predictions on the testing data
predictions <- predict(best_model, newdata = test_data, interval = "prediction", level = 0.95)

# Create a data frame to store results
results <- data.frame(
  x1 = test_data$x1,
  x2 = test_data$x2,
  x3 = test_data$x3,
  y_true = test_data$y,
  y_pred = predictions[, 1],  # Predicted values
  lower_bound = predictions[, 2],  # Lower bound of the prediction interval
  upper_bound = predictions[, 3]   # Upper bound of the prediction interval
)

plot(results)

# Create a scatterplot of the testing data points with prediction intervals
ggplot(results, aes(x = x1, y = y_true)) +
  geom_point() +
  geom_line(aes(x = x1, y = y_pred), color = "blue", size = 1) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "red", size = 1) +
  ggtitle("Model 4: Testing Data vs. Predictions with 95% Prediction Intervals") +
  xlab("x1 (Age)") +
  ylab("Total Sales Quantity")
