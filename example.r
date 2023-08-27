# Sample dataset: Exam scores
scores <- c(85, 90, 78, 92, 88, 75, 82, 95, 68, 98)

# Calculate mean
mean_score <- mean(scores)

# Calculate median
median_score <- median(scores)

# Calculate range
range_score <- max(scores) - min(scores)

# Calculate variance and standard deviation
variance_score <- var(scores)
std_deviation <- sd(scores)

# Calculate quartiles and interquartile range
q1 <- quantile(scores, 0.25)
q3 <- quantile(scores, 0.75)
iqr <- q3 - q1

# Create a box plot
boxplot(scores, main = "Exam Scores Distribution", ylab = "Scores")

# Print the calculated statistics
cat("Mean:", mean_score, "\n")
cat("Median:", median_score, "\n")
cat("Range:", range_score, "\n")
cat("Variance:", variance_score, "\n")
cat("Standard Deviation:", std_deviation, "\n")
cat("Q1:", q1, "\n")
cat("Q3:", q3, "\n")
cat("IQR:", iqr, "\n")
