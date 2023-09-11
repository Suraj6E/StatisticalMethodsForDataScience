# Define two matrices
A <- matrix(c(1, 2, 3, 4), nrow = 2)
B <- matrix(c(5, 6, 7, 8), nrow = 2)
C <- A + B

multiplier <- A %*% B

print("Multiplier: ")
print(multiplier)



# create matrix with 2 row 3 col
m <- matrix(data = 1:6, ncol = 3, nrow = 2)
colnames(m) <- c("A", "B", "C")
print(m)

# create matrix with 3 row 3 col
m <- matrix(data = 1:9, ncol = 3, nrow = 3)
rownames(m) <- c("A", "B", "C")
print(m)


# average
data <- 1:100
average <- mean(data)
print(average)

# average
data <- 1:100
median <- median(data)
print(median)


# matrix determinanet
print("Determinent: ")
m <- matrix(data = 1:16, ncol = 4, nrow = 4)
d <- det(m)

print(d)



# problem solve:

print("Problem")

A <- matrix(data = 1:20, ncol = 4, nrow = 5)
colnames(A) <- c("o", "t", "th", "f")
rownames(A) <- c("o", "t", "th", "fo", "fi")

B <- matrix(data = 1:9, ncol = 3, nrow = 3)
colnames(B) <- c("o", "t", "th")

C <- matrix(data = 1:4, ncol = 2, nrow = 2)

print(A)



## ifelse
a <- 100
if (a > 100) {
    print("A is higher then 100")
} else if (a == 100) {
    print("A is equal to 100")
} else {
    print("A is less then 100")
}
