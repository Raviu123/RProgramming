cost_matrix <- matrix(c(
  6,3,4,
  4,8,4,
  1,7,2
), nrow = 3, byrow = TRUE)

supply <- c(50,40,60)
demand <- c(20,95,35)

alloc <- matrix(0, nrow = 3, ncol = 3)

i <- 1
j <- 1

while (i <= length(supply) && j <= length(demand)) {
  
  min_val <- min(supply[i], demand[j])
  
  alloc[i, j] <- min_val
  
  supply[i] <- supply[i] - min_val
  demand[j] <- demand[j] - min_val
  
  if (supply[i] == 0) {
    i <- i + 1
  } else if (demand[j] == 0) {
    j <- j + 1
  }
}

# Output (outside loop)
cat("Initial allocation using NWCR:\n")
print(alloc)

total_cost <- sum(alloc * costs)
cat("Total transportation cost using NWCR:", total_cost, "\n")