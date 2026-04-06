
#lpp user inout


library(lpSolve)

# Input number of variables and constraints
n <- as.integer(readline("Enter number of variables: "))
m <- as.integer(readline("Enter number of constraints: "))

# Objective function coefficients
c <- numeric(n)
cat("Enter coefficients of objective function:\n")
for (i in 1:n) {
  c[i] <- as.numeric(readline(paste("X", i, ": ", sep = "")))
}

# Constraint matrix3

A <- matrix(0, m, n)
cat("Enter constraint coefficients:\n")
for (i in 1:m) {
  for (j in 1:n) {
    A[i, j] <- as.numeric(readline(paste("a[", i, ",", j, "]: ", sep = "")))
  }
}

# RHS values
b <- numeric(m)
cat("Enter RHS values:\n")
for (i in 1:m) {
  b[i] <- as.numeric(readline(paste("b", i, ": ", sep = "")))
}

# Solve LP problem
result <- lp("max", c, A, rep("<=", m), b)

# Output
cat("Optimal Value:", result$objval, "\n")
cat("Solution:\n")
print(result$solution)