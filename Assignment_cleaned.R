#library used
library(ggplot2) # for visualization
library(gridExtra) # For visualize multiple charts
library(dplyr) # For summarize dataset
library(e1071) # For graph calculation
library(reshape2)  # For data transformation

options(scipen = 999)  # Setting scipen to a high value to prevent scientific notation

#read customer shopping data 
data <- read.csv("./customer_shopping_data.csv")

### EDA ###

#print if there are any missing data
print(colSums(is.na(data)))

# Convert invoice_date to datetime
data$invoice_date <- as.Date(data$invoice_date, format = "%d/%m/%Y")
data <- data[order(data$invoice_date), ]



#remove unique values
data$invoice_no <- NULL
data$customer_id <- NULL

## convert data into unique time series ##
daily_data <- data %>%
  group_by(invoice_date) %>%
  summarize(
    age = median(age),                         # median age among all ages
    categories = n_distinct(category),          # number of distinct categories
    price = sum(price),                         # sum of price for all sales
    shopping_mall = n_distinct(shopping_mall),  # number of distinct shopping malls
    total_quantity = sum(quantity)             # total quantities
  )

#changing data set into time series
ts_data <- ts(daily_data, start = c(2020, 1, 1), frequency = 365)


summary(daily_data)

### Task 1: Preliminary data analysis ###
## Task 1.1 ##

# Plots for each features
age_plt <- ggplot(daily_data, aes(invoice_date, age)) +
  geom_line(aes(y = age), color = "purple") +
  theme_minimal() +
  labs(x = NULL, y = "Age")


categories_plt <- ggplot(daily_data, aes(invoice_date, categories)) +
  geom_line(aes(y = categories), color = "chocolate3") +
  theme_minimal() +
  labs( x = NULL, y = "Categories")

price_plt <- ggplot(daily_data, aes(invoice_date, price)) +
  geom_line(aes(y = price), color = "purple") +
  theme_minimal() +
  labs(title = "Price Plot", x = NULL, y = "Price")

shopping_mall_plt <- ggplot(daily_data, aes(invoice_date, shopping_mall)) +
  geom_line(aes(y = shopping_mall), color = "chocolate3") +
  theme_minimal() +
  labs(x = NULL, y = "Shopping Mall")

total_quantity_plt <- ggplot(daily_data, aes(invoice_date, total_quantity)) +
  geom_line(aes(y = total_quantity), color = "purple") +
  theme_minimal() +
  labs( x = NULL, y = "Total Quantity")

# Arrange the plots in a 1x5 grid with titles and axis labels
grid.arrange(age_plt, categories_plt, price_plt, shopping_mall_plt, total_quantity_plt, ncol = 1, nrow = 5)

## Task 1.2: Distribution for each sales data ##

# Create and save individual distribution plots for each variable

plots <- lapply(variables, function(var) {
  p <- ggplot(daily_data, aes(x = get(var), )) +
    geom_histogram(fill = "purple", color = "chocolate3", bins = 30) +
    scale_fill_manual(values = c("A" = "blue", "B" = "red", "C" = "green"), name = "Groups") +
    labs(title = paste("Distribution of", var),
         x = var, y = "Frequency") +
    theme_minimal()
  
  # Calculate mean and median
  mean_val <- mean(daily_data[[var]], na.rm = TRUE)
  median_val <- median(daily_data[[var]], na.rm = TRUE)
  # Add mean and median lines with custom colors and show legend
  p <- p +
    geom_vline(
      aes(xintercept = mean_val, color = "Mean Line"),
      linetype = "dashed", size = 1, show.legend = TRUE
    ) +
    geom_vline(
      aes(xintercept = median_val, color = "Median Line"),
      linetype = "dashed", size = 1, show.legend = TRUE
    ) +
    annotate("text", x = 0, y = max(hist(daily_data[[var]])$count), label = "")
    
  
  return(p)
})

# Arrange the plots in a grid (e.g., in a 1x5 grid)
grid.arrange(grobs = plots, ncol = 2, nrow = 3)


## Task 1.3: Correlation and scatter plots ##

### correlation ###
correlation_matrix <- cor(ts_data)
correlation_df <- melt(correlation_matrix)
# Convert the correlation matrix to a data frame suitable for plotting
correlation_df <- melt(correlation_matrix)

# Create a ggplot for the correlation matrix
heatmap_plot <- ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap")

# Add numbers to the heatmap
heatmap_plot +
  geom_text(aes(label = round(value, 2)), vjust = 1)  # Adjust 'vjust' for label position



### scatterplot ###

# Scatter plot between age and predicted sales quantity
age_plot <- ggplot(data = daily_data, aes(x = age, y = total_quantity)) +
  geom_point(fill = "purple", color = "chocolate3") +
  labs(title = "Scatter Plot of Age vs.  Sales Quantity",
       x = "Age",
       y = " Sales Quantity") +
  theme_minimal()

# Scatter plot between Categories and predicted sales quantity
categories_plot <- ggplot(data = daily_data, aes(x = categories, y = total_quantity)) +
  geom_point(fill = "purple", color = "chocolate3") +
  labs(title = "Scatter Plot of Price vs.  Sales Quantity",
       x = "Categories ",
       y = "Sales Quantity") +
  theme_minimal()

# Scatter plot between price and predicted sales quantity
price_plot <- ggplot(data = daily_data, aes(x = price, y = total_quantity)) +
  geom_point(fill = "purple", color = "chocolate3") +
  labs(title = "Scatter Plot of Price vs. Sales Quantity",
       x = "Price",
       y = "Sales Quantity") +
  theme_minimal()


# Scatter plot between price and predicted sales quantity
shopping_malls_plot <- ggplot(data = daily_data, aes(x = shopping_mall, y = total_quantity)) +
  geom_point(fill = "purple", color = "chocolate3") +
  labs(title = "Scatter Plot of shopping malls vs. Sales Quantity",
       x = "Shopping Malls",
       y = "Sales Quantity") +
  theme_minimal()

# Arrange the scatter plots in a grid (e.g., in a 2x2 grid)
grid.arrange(age_plot, categories_plot, price_plot, shopping_malls_plot, ncol = 2, nrow = 2)


#task 2: Regression – modeling the relationship between sales data
# Create a data frame with your x and y data
df <- data.frame(
  x1 = ts_data[, "age"],
  x2 = ts_data[, "categories"],
  x3 = ts_data[, "price"],
  x4 = ts_data[, "shopping_mall"],
  y = ts_data[, "total_quantity"]
)


#create 5 modal formula

model1 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = df)

model2 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model4 <- lm(y ~ poly(x2, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model5 <- lm(y ~ poly(x4, 1, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x3, 4, raw = TRUE), data = df)


#Task 2.1: Estimate modal parameters 𝜃for every candidate modal using least squre

# Create a list to store the estimated parameters for each model
estimated_parameters_list <- list(
  Model1 = coef(model1),
  Model2 = coef(model2),
  Model3 = coef(model3),
  Model4 = coef(model4),
  Model5 = coef(model5)
)



# Function to extract coefficients for Model 1
extract_coefficients_model1 <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x4, 1, raw = TRUE)"]
  coef_list$θ2 <- parameters["poly(x1, 2, raw = TRUE)1"]
  coef_list$θ3 <- parameters["poly(x1, 3, raw = TRUE)3"]
  coef_list$θ4 <- parameters["poly(x2, 4, raw = TRUE)1"]
  coef_list$θ5 <- parameters["poly(x1, 4, raw = TRUE)4"]
  coef_list$θbias <- parameters["(Intercept)"]
  return(coef_list)
}

# Function to extract coefficients for Model 2
extract_coefficients_model2 <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x4, 1, raw = TRUE)"]
  coef_list$θ2 <- parameters["poly(x1, 3, raw = TRUE)1"]
  coef_list$θ3 <- parameters["poly(x3, 4, raw = TRUE)1"]
  coef_list$θ4 <- NA
  coef_list$θ5 <- NA
  coef_list$θbias <- parameters["(Intercept)"]
  return(coef_list)
}
# Function to extract coefficients for Model 3
extract_coefficients_model3 <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x3, 3, raw = TRUE)1"]
  coef_list$θ2 <- parameters["poly(x3, 4, raw = TRUE)4"]
  coef_list$θ3 <- NA
  coef_list$θ4 <- NA
  coef_list$θ5 <- NA
  coef_list$θbias <- parameters["(Intercept)"]
  return(coef_list)
}

# Function to extract coefficients for Model 4
extract_coefficients_model4 <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x2, 1, raw = TRUE)"]
  coef_list$θ2 <- parameters["poly(x1, 3, raw = TRUE)1"]
  coef_list$θ3 <- NA
  coef_list$θ4 <- parameters["poly(x3, 4, raw = TRUE)1"]
  coef_list$θ5 <- NA
  coef_list$θbias <- parameters["(Intercept)"]
  return(coef_list)
}

# Function to extract coefficients for Model 5
extract_coefficients_model5 <- function(parameters) {
  coef_list <- list()
  coef_list$θ1 <- parameters["poly(x4, 1, raw = TRUE)"]
  coef_list$θ2 <- parameters["poly(x1, 2, raw = TRUE)1"]
  coef_list$θ3 <- parameters["poly(x1, 3, raw = TRUE)3"]
  coef_list$θ4 <- parameters["poly(x3, 4, raw = TRUE)1"]
  coef_list$θ5 <- NA
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
  θ5 = numeric(0),
  θbias = numeric(0)
)


# Loop through each model's estimated parameters
for (model_name in names(estimated_parameters_list)) {
  parameters <- estimated_parameters_list[[model_name]]
  
  # Extract coefficients based on the model
  if (model_name == "Model1") {
    coefficients <- extract_coefficients_model1(parameters)
  } else if (model_name == "Model2") {
    coefficients <- extract_coefficients_model2(parameters)
  }else if (model_name == "Model3") {
    coefficients <- extract_coefficients_model3(parameters)
  }else if (model_name == "Model4") {
    coefficients <- extract_coefficients_model4(parameters)
  }else if (model_name == "Model5") {
    coefficients <- extract_coefficients_model5(parameters)
  }
  
  # Add coefficients to the DataFrame
  coefficients_df <- rbind(coefficients_df, cbind(Model = model_name, as.data.frame(t(coefficients))))
}

# Print the coefficients DataFrame
print(coefficients_df)


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

### q-q plot ###

# Create Q-Q plots for each model's errors
par(mfrow = c(2, 3))  # Arrange plots in a 2x3 grid

# Q-Q plot for errors1 (Model 1)
qqnorm(errors1, main = "Q-Q Plot - Model 1")
qqline(errors1, col = 2)

# Q-Q plot for errors2 (Model 2)
qqnorm(errors2, main = "Q-Q Plot - Model 2")
qqline(errors2, col = 2)

# Q-Q plot for errors3 (Model 3)
qqnorm(errors3, main = "Q-Q Plot - Model 3")
qqline(errors3, col = 2)

# Q-Q plot for errors4 (Model 4)
qqnorm(errors4, main = "Q-Q Plot - Model 4")
qqline(errors4, col = 2)

# Q-Q plot for errors5 (Model 5)
qqnorm(errors5, main = "Q-Q Plot - Model 5")
qqline(errors5, col = 2)

par(mfrow = c(1, 1))  # Reset plotting parameters to default



### Task 2.5 ###

# Calculate means of errors for each model
mean_errors <- c(mean(errors1), mean(errors2), mean(errors3), mean(errors4), mean(errors5))

skews <- c(skewness(errors1), skewness(errors2), skewness(errors3), skewness(errors4), skewness(errors5))

kurts <- c(kurtosis(errors1), kurtosis(errors2), kurtosis(errors3), kurtosis(errors4), kurtosis(errors5))

sds <- c(sd(errors1), sd(errors2), sd(errors3), sd(errors4), sd(errors5))


# Create a table
result_table <- data.frame(
  Model = paste("Model", 1:5),
  Mean_Error = mean_errors,
  AIC = aic_values,
  BIC = bic_values,
  skews = skews,
  kurts = kurts,
  sds = sds,
  log_likelihood = log_likelihood_values
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



### Success rate ###

# Set the success threshold (e.g., 10 units)
threshold <- 1000

# Calculate the absolute error between predicted and true quantities
results$absolute_error <- abs(results$y_pred - results$y_true)

# Determine if predictions are successful based on the threshold
results$successful_prediction <- results$absolute_error <= threshold

# Calculate the success rate (percentage)
success_rate <- mean(results$successful_prediction) * 100

# Print the success rate
cat("Success Rate on Predictions:", success_rate, "%\n")



######## Task 3 #########

### Task 3.1 ###


