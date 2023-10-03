# Load required libraries
library(dplyr)
library(tidyr)



# Task 2
data <- data.frame(
  x1 = x,
  x2 = x,
  x3 = x,
  x4 = x,
  y = y
)

model1 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x2, 4, raw = TRUE) + poly(x1, 4, raw = TRUE), data = df)

model2 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model3 <- lm(y ~ poly(x3, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model4 <- lm(y ~ poly(x2, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) + poly(x3, 4, raw = TRUE), data = df)

model5 <- lm(y ~ poly(x4, 4, raw = TRUE) + poly(x1, 2, raw = TRUE) + poly(x1, 3, raw = TRUE) +
               poly(x3, 4, raw = TRUE), data = df)

# Obtain the estimated coefficients for each model
coefficients_model1 <- coef(model1)
coefficients_model2 <- coef(model2)
coefficients_model3 <- coef(model3)
coefficients_model4 <- coef(model4)
coefficients_model5 <- coef(model5)

# Create a dataframe to display the coefficients
coefficients_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  θ1 = c(coefficients_model1["poly(x4, 4, raw = TRUE)θ1"],
         coefficients_model2["poly(x4, 4, raw = TRUE)θ1"],
         coefficients_model3["poly(x3, 3, raw = TRUE)θ1"],
         coefficients_model4["poly(x2, 2, raw = TRUE)θ1"],
         coefficients_model5["poly(x4, 4, raw = TRUE)θ1"]),
  θ2 = c(coefficients_model1["poly(x1, 2, raw = TRUE)θ2"],
         coefficients_model2["poly(x1, 3, raw = TRUE)θ2"],
         NA,
         coefficients_model4["poly(x1, 3, raw = TRUE)θ2"],
         coefficients_model5["poly(x1, 2, raw = TRUE)θ2"]),
  θ3 = c(coefficients_model1["poly(x1, 3, raw = TRUE)θ3"],
         coefficients_model2["poly(x3, 4, raw = TRUE)θ3"],
         NA,
         coefficients_model4["poly(x3, 4, raw = TRUE)θ3"],
         coefficients_model5["poly(x1, 3, raw = TRUE)θ3"]),
  θ4 = c(coefficients_model1["poly(x2, 4, raw = TRUE)θ4"],
         NA,
         NA,
         NA,
         coefficients_model5["poly(x3, 4, raw = TRUE)θ4"]),
  θbias = c(coefficients_model1["poly(x1, 4, raw = TRUE)θbias"],
            coefficients_model2["(Intercept)"],
            coefficients_model3["(Intercept)"],
            coefficients_model4["(Intercept)"],
            coefficients_model5["θbias"])
)

# Display the dataframe
coefficients_df

# Task 2.2
rss_values <- c(
  sum(model1$residuals^2),
  sum(model2$residuals^2),
  sum(model3$residuals^2),
  sum(model4$residuals^2),
  sum(model5$residuals^2)
)

# Task 2.3
calculate_log_likelihood <- function(model) {
  n <- length(model$residuals)
  sigma_sq <- sum(model$residuals^2) / (n - length(model$coefficients))
  log_likelihood <- -n/2 * log(2 * pi * sigma_sq) - sum(model$residuals^2) / (2 * sigma_sq)
  return(log_likelihood)
}

log_likelihood_values <- c(
  calculate_log_likelihood(model1),
  calculate_log_likelihood(model2),
  calculate_log_likelihood(model3),
  calculate_log_likelihood(model4),
  calculate_log_likelihood(model5)
)

# Task 2.4
aic_values <- c(
  AIC(model1),
  AIC(model2),
  AIC(model3),
  AIC(model4),
  AIC(model5)
)

bic_values <- c(
  BIC(model1),
  BIC(model2),
  BIC(model3),
  BIC(model4),
  BIC(model5)
)

# Task 2.5
predictions1 <- predict(model1)
predictions2 <- predict(model2)
predictions3 <- predict(model3)
predictions4 <- predict(model4)
predictions5 <- predict(model5)

errors1 <- y - predictions1
errors2 <- y - predictions2
errors3 <- y - predictions3
errors4 <- y - predictions4
errors5 <- y - predictions5