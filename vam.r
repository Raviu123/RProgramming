#Vogel’s Approximation Method (VAM).

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
          sorted <- sort(row_costs)
          row_penalty[i] <- sorted[2] - sorted[1]
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
          sorted <- sort(col_costs)
          col_penalty[j] <- sorted[2] - sorted[1]
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
    
    # Allocate
    qty <- min(supply_left[i], demand_left[j])
    
    allocation[i, j] <- qty
    
    supply_left[i] <- supply_left[i] - qty
    demand_left[j] <- demand_left[j] - qty
    
    # Block exhausted row/column
    if (supply_left[i] == 0) {
      cost_copy[i, ] <- Inf
    }
    
    if (demand_left[j] == 0) {
      cost_copy[, j] <- Inf
    }
  }
  
  total_cost <- sum(allocation * cost_matrix)
  
  return(list(allocation = allocation, total_cost = total_cost))
}

# Example input
cost_matrix <- matrix(c(
  19, 30, 50, 10,
  70, 30, 40, 60,
  40,  8, 70, 20
), nrow = 3, byrow = TRUE)

supply <- c(7, 9, 18)
demand <- c(5, 8, 7, 14)

# Run
result <- vogel_approximation_method(cost_matrix, supply, demand)

cat("Allocation Matrix:\n")
print(result$allocation)

cat("Total Transportation Cost:", result$total_cost, "\n")