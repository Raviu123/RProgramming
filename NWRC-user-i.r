#NWCR user input

# Input number of sources and destinations
m <- as.integer(readline("Enter number of sources: "))
n <- as.integer(readline("Enter number of destinations: "))

# Cost matrix
cost <- matrix(0, m, n)
cat("Enter cost matrix:\n")
for (i in 1:m) {
  for (j in 1:n) {
    cost[i, j] <- as.numeric(readline(paste("Cost[", i, ",", j, "]: ", sep = "")))
  }
}

# Supply
supply <- numeric(m)
cat("Enter supply values:\n")
for (i in 1:m) {
  supply[i] <- as.numeric(readline(paste("Supply ", i, ": ", sep = "")))
}

# Demand
demand <- numeric(n)
cat("Enter demand values:\n")
for (j in 1:n) {
  demand[j] <- as.numeric(readline(paste("Demand ", j, ": ", sep = "")))
}

# Allocation matrix
alloc <- matrix(0, m, n)

i <- 1
j <- 1

# North-West Corner Method
while (i <= m && j <= n) {
  x <- min(supply[i], demand[j])
  
  alloc[i, j] <- x
  supply[i] <- supply[i] - x
  demand[j] <- demand[j] - x
  
  if (supply[i] == 0) {
    i <- i + 1
  } else {
    j <- j + 1
  }
}

# Output
cat("Allocation Matrix:\n")
print(alloc)

total <- sum(alloc * cost)
cat("Total cost =", total, "\n")