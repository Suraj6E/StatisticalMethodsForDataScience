# Sample dataset: Exam scores
scores <- c(85, 90, 78, 92, 88, 75, 82, 95, 68, 98)

# Create a histogram
hist(scores, breaks = 5, col = "skyblue", xlab = "Scores", ylab = "Frequency",
     main = "Histogram of Exam Scores")

# Create a scatter plot
plot(seq_along(scores), scores, col = "darkorange", pch = 16,
     xlab = "Student", ylab = "Scores", main = "Scatter Plot of Exam Scores")

# Create a box plot
boxplot(scores, col = "lightgreen", xaxt = "n", ylab = "Scores",
        main = "Box Plot of Exam Scores")

# Adding x-axis labels for the box plot
axis(1, at = 1, labels = c("Scores"))
