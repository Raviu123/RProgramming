# Vogel Approximation Method (VAM)

vogel_approximation_method <- function(cost_matrix, supply, demand) {
  
  n <- nrow(cost_matrix)
  m <- ncol(cost_matrix)
  
  allocation <- matrix(0, n, m)
  
  supply_left <- supply
  demand_left <- demand
  
  cost_copy <- cost_matrix
  
  while (sum(supply_left) > 0 && sum(demand_left) > 0) {
    
    # Row penalties
    row_penalty <- rep(-1, n)
    for (i in 1:n) {
      if (supply_left[i] > 0) {
        row_costs <- cost_copy[i, demand_left > 0]
        
        if (length(row_costs) >= 2) {
          sorted_costs <- sort(row_costs)
          row_penalty[i] <- sorted_costs[2] - sorted_costs[1]
        } else {
          row_penalty[i] <- row_costs[1]
        }
      }
    }
    
    # Column penalties
    col_penalty <- rep(-1, m)
    for (j in 1:m) {
      if (demand_left[j] > 0) {
        col_costs <- cost_copy[supply_left > 0, j]
        
        if (length(col_costs) >= 2) {
          sorted_costs <- sort(col_costs)
          col_penalty[j] <- sorted_costs[2] - sorted_costs[1]
        } else {
          col_penalty[j] <- col_costs[1]
        }
      }
    }
    
    # Choose max penalty
    if (max(row_penalty) >= max(col_penalty)) {
      i <- which.max(row_penalty)
      valid_j <- which(demand_left > 0)
      j <- valid_j[which.min(cost_copy[i, valid_j])]
    } else {
      j <- which.max(col_penalty)
      valid_i <- which(supply_left > 0)
      i <- valid_i[which.min(cost_copy[valid_i, j])]
    }
    
    # Allocation
    qty <- min(supply_left[i], demand_left[j])
    
    allocation[i, j] <- qty
    
    supply_left[i] <- supply_left[i] - qty
    demand_left[j] <- demand_left[j] - qty
    
    # Block exhausted row/column
    if (supply_left[i] == 0) cost_copy[i, ] <- Inf
    if (demand_left[j] == 0) cost_copy[, j] <- Inf
  }
  
  total_cost <- sum(allocation * cost_matrix)
  
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

result <- vogel_approximation_method(cost_matrix, supply, demand)

cat("Allocation Matrix:\n")
print(result$allocation)

cat("Total transportation cost:", result$total_cost, "\n")