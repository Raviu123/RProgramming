#Matrix minima meton user input

matrix_minima_method <- function(cost_matrix, supply, demand) {
  
  n <- nrow(cost_matrix)
  m <- ncol(cost_matrix)
  
  allocation <- matrix(0, n, m)
  original_cost_matrix <- cost_matrix
  
  supply_left <- supply
  demand_left <- demand
  
  while (any(supply_left > 0) && any(demand_left > 0)) {
    
    # Find minimum cost among valid cells
    valid_rows <- supply_left > 0
    valid_cols <- demand_left > 0
    
    sub_matrix <- cost_matrix[valid_rows, valid_cols, drop = FALSE]
    min_cost <- min(sub_matrix, na.rm = TRUE)
    
    indices <- which(cost_matrix == min_cost, arr.ind = TRUE)
    
    # Filter valid indices
    indices <- indices[
      supply_left[indices[,1]] > 0 &
        demand_left[indices[,2]] > 0,
      , drop = FALSE
    ]
    
    if (nrow(indices) == 0) break
    
    i <- indices[1, 1]
    j <- indices[1, 2]
    
    qty <- min(supply_left[i], demand_left[j])
    
    allocation[i, j] <- qty
    
    supply_left[i] <- supply_left[i] - qty
    demand_left[j] <- demand_left[j] - qty
    
    if (supply_left[i] == 0) {
      cost_matrix[i, ] <- Inf
    }
    
    if (demand_left[j] == 0) {
      cost_matrix[, j] <- Inf
    }
  }
  
  total_cost <- sum(allocation * original_cost_matrix)
  
  return(list(allocation = allocation, total_cost = total_cost))
}

# -------- USER INPUT --------

m <- as.integer(readline("Enter number of sources: "))
n <- as.integer(readline("Enter number of destinations: "))

cost_matrix <- matrix(0, m, n)

cat("Enter cost matrix:\n")
for (i in 1:m) {
  for (j in 1:n) {
    cost_matrix[i, j] <- as.numeric(
      readline(paste("Cost[", i, ",", j, "]: ", sep = ""))
    )
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

# -------- RUN --------

result <- matrix_minima_method(cost_matrix, supply, demand)

cat("Allocation Matrix:\n")
print(result$allocation)

cat("Total transportation cost:", result$total_cost, "\n")