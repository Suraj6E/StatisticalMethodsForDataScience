#imports
library(ggplot2)
library(xts) # For time series manipulation
library(dplyr)
library(tidyr)


#read customer shopping data 
data <- read.csv("./customer_shopping_data.csv")

#print if there are any missing data
print(colSums(is.na(data)))

# Convert invoice_date to datetime
data$invoice_date <- as.Date(data$invoice_date, format = "%d/%m/%y")
data <- data[order(data$invoice_date), ]


#convert data into a time-series
ts_data <- ts(data, start=c(2020, 1, 1), frequency = 365)


#Task 1: Preliminary data analysis



#task 2: Regression – modeling the relationship between sales data
# Create a data frame with your x and y data
df <- data.frame(
  x1 = ts_data[, "age"],
  x2 = ts_data[, "category"],
  x3 = ts_data[, "price"],  # You mentioned "price" in your original code, so I included it here as an example
  x4 = ts_data[, "payment_method"],  # You mentioned "payment_method" in your original code
  y = ts_data[, "quantity"]  # You mentioned "quantity" in your original code
)

plot(df)