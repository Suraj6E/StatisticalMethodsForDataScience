#library used
library(ggplot2) # for visualization
library(gridExtra) # For visualize multiple charts
library(dplyr) # For summarize dataset
library(e1071) # For graph calculation
library(reshape2)  # For data transformation
library(abc) # for task 3


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

print(estimated_parameters_list$Model1)


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
best_model <- lm(y ~ poly(x2, 1, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

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

# Create a scatterplot for training data
train_plot <- ggplot(train_data, aes(x = x1, y = y)) +
  geom_point(color = "blue") +
  labs(title = "Training Data", x = "x1", y = "y")

# Create a scatterplot for test data
test_plot <- ggplot(test_data, aes(x = x1, y = y)) +
  geom_point(color = "red") +
  labs(title = "Test Data", x = "x1", y = "y")

# Create a scatterplot for predicted vs. true data with prediction intervals
results_plot <- ggplot(results, aes(x = x1, y = y_pred)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.2, color = "blue") +
  geom_point(data = results, aes(x = x1, y = y_true), color = "red", size = 3) +
  labs(title = "Predicted vs. True Data with Prediction Intervals", x = "x1", y = "y")

results_plot
grid.arrange(train_plot, test_plot, results_plot, ncol = 2)


# Create a scatterplot of the testing data points with prediction intervals

ggplot(results, aes(x = x1)) +
  geom_point(aes(y = y_true, color = "True Data"), size = 2) +
  geom_point(aes(y = y_pred, color = "Predicted Data"), size = 2) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = "Prediction Intervals"), 
                width = 0.1, size = 1) +
  scale_color_manual(values = c("True Data" = "black", "Predicted Data" = "blue", "Prediction Intervals" = "red"),
                     labels = c("True Data", "Predicted Data", "Prediction Intervals")) +
  ggtitle("Testing Data vs. Predictions of Modal 4") +
  xlab("Test Data") +
  ylab("Values") +
  theme(legend.title = element_blank())




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
# Task 3: Approximate Bayesian Computation (ABC)


# Task 3.1: Compute 2 parameter posterior distributions
# Define the likelihood function for a normal distribution

# Initialize MCMC settings
n_iterations <- 10000
burn_in <- 1000
n_parameters <- 2  # Number of parameters to estimate (theta_bias and theta_2)

# Initialize parameter values for the two parameters of interest and fix others
theta_current <- list(theta_bias = 34100.456, theta_2 = 3342.672)
# Fix other parameters based on estimated values
theta_1_fixed <- 29.39313
theta_3_fixed <- -1.949769
theta_4_fixed <- 36.0437
theta_5_fixed <- 0.01168091

# Create empty lists to store samples
theta_samples <- vector("list", length = n_iterations)
acceptance <- rep(0, n_iterations)

# Metropolis-Hastings MCMC targeting only theta_bias and theta_2
for (iteration in 1:n_iterations) {
  # Propose new values for theta_bias and theta_2
  theta_bias_proposed <- theta_current$theta_bias + rnorm(1, mean = 0, sd = 100)  # Adjust step size as needed
  theta_2_proposed <- theta_current$theta_2 + rnorm(1, mean = 0, sd = 100)  # Adjust step size as needed
  
  # Calculate likelihood and prior for current and proposed parameters
  likelihood_current <- likelihood(df$y, theta_current$theta_bias + theta_current$theta_2 * df$x4, 5)
  likelihood_proposed <- likelihood(df$y, theta_bias_proposed + theta_2_proposed * df$x4, 5)
  prior_current <- prior(theta_current$theta_bias, theta_1_fixed, 10000) + prior(theta_current$theta_2, theta_2_fixed, 10000)
  prior_proposed <- prior(theta_bias_proposed, theta_1_fixed, 10000) + prior(theta_2_proposed, theta_2_fixed, 10000)
  
  # Calculate acceptance probability
  acceptance_prob <- exp(likelihood_proposed + prior_proposed - likelihood_current - prior_current)
  
  # Accept or reject the proposal
  if (runif(1) < acceptance_prob) {
    theta_current$theta_bias <- theta_bias_proposed
    theta_current$theta_2 <- theta_2_proposed
    acceptance[iteration] <- 1
  }
  
  # Store the current parameter values
  theta_samples[[iteration]] <- theta_current
}

# Remove burn-in samples
theta_samples <- theta_samples[-(1:burn_in)]
acceptance_rate <- mean(acceptance[-(1:burn_in)])


# Now 'theta_samples' contains samples from the posterior distributions of theta_bias and theta_2,
# while keeping all other parameters fixed as constants.
# 'acceptance_rate' gives you the acceptance rate of the Metropolis-Hastings algorithm.


# Extract the posterior samples for theta_bias and theta_2
theta_bias_samples <- sapply(theta_samples, function(sample) sample$theta_bias)
theta_2_samples <- sapply(theta_samples, function(sample) sample$theta_2)

# Create a data frame for plotting
posterior_df <- data.frame(
  theta_bias = theta_bias_samples,
  theta_2 = theta_2_samples
)

# Create a density plot
ggplot(posterior_df, aes(x = theta_bias)) +
  geom_density(fill = "blue", alpha = 0.6) +
  geom_vline(xintercept = theta_current$theta_bias, color = "red", linetype = "dashed") +
  labs(title = "Posterior Distribution of theta_bias") +
  theme_minimal()

# Create a density plot for theta_2
ggplot(posterior_df, aes(x = theta_2)) +
  geom_density(fill = "blue", alpha = 0.6) +
  geom_vline(xintercept = theta_current$theta_2, color = "red", linetype = "dashed") +
  labs(title = "Posterior Distribution of theta_2") +
  theme_minimal()


### task 3.2 ###

prior_range_theta_bias <- c(20000, 50000)  # Adjust as needed
prior_range_theta_2 <- c(-5000, -2000)  # Adjust as needed

# Create a data frame for plotting
posterior_df <- data.frame(
  theta_bias = theta_bias_samples,
  theta_2 = theta_2_samples
)

### task 3.3 ###

# Create a density plot for theta_bias with the uniform prior
plot_theta_bias <- ggplot(posterior_df, aes(x = theta_bias, fill = "Posterior")) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = theta_current$theta_bias, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(prior_range_theta_bias), color = "green", linetype = "dashed") +
  labs(title = "Posterior Distribution of theta_bias with Uniform Prior") +
  theme_minimal() +
  scale_fill_manual(values = c("Posterior" = "blue"), guide = guide_legend(title = "Legend"))

# Create a density plot for theta_2 with the uniform prior
plot_theta_2 <- ggplot(posterior_df, aes(x = theta_2, fill = "Posterior")) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = theta_current$theta_2, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(prior_range_theta_2), color = "blue", linetype = "dashed") +
  labs(title = "Posterior Distribution of theta_2 with Uniform Prior") +
  theme_minimal() +
  scale_fill_manual(values = c("Posterior" = "green"), guide = guide_legend(title = "Legend"))

# Print the two density plots side by side (you can adjust the layout as needed)
grid.arrange(plot_theta_bias, plot_theta_2, ncol = 2)


### task 3.4 ###

# Create a joint scatterplot matrix for theta_bias and theta_2
joint_plot <- ggplot(posterior_df, aes(x = theta_bias, y = theta_2, color = "Data Points")) +
  geom_point(alpha = 0.4) +
  labs(title = "Joint Posterior Distribution of theta_bias and theta_2") +
  theme_minimal() +
  scale_color_manual(values = "chocolate3")

joint_plot

# Create a marginal histogram for theta_bias with color
marginal_theta_bias <- ggplot(posterior_df, aes(x = theta_bias, fill = "theta_bias")) +
  geom_histogram(binwidth = 500, alpha = 0.6) +
  labs(title = "Marginal Posterior Distribution of theta_bias") +
  theme_minimal() +
  scale_fill_manual(values = c("theta_bias" = "blue"), guide = guide_legend(title = "Parameter"))

# Create a marginal histogram for theta_2 with color
marginal_theta_2 <- ggplot(posterior_df, aes(x = theta_2, fill = "theta_2")) +
  geom_histogram(binwidth = 100, alpha = 0.6) +
  labs(title = "Marginal Posterior Distribution of theta_2") +
  theme_minimal() +
  scale_fill_manual(values = c("theta_2" = "green"), guide = guide_legend(title = "Parameter"))

# Arrange and print the plots using grid.arrange
grid.arrange(marginal_theta_bias, marginal_theta_2, ncol = 2)




