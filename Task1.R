library(ggplot2)
library(ts)  # For creating time series objects
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

#convert invoice_date into a time series object
ndvi_series <- xts(data$ndvi_value, order.by = data$invoice_date)

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