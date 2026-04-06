#Matrix Minima Method

matrix_minima_method <- function(cost_matrix, supply, demand) {
  
  n <- nrow(cost_matrix)
  m <- ncol(cost_matrix)
  
  allocation <- matrix(0, nrow = n, ncol = m)
  original_cost_matrix <- cost_matrix
  
  supply_left <- supply
  demand_left <- demand
  
  while (any(supply_left > 0) && any(demand_left > 0)) {
    
    min_cost <- min(cost_matrix[supply_left>0,demand_left>0], na.rm = TRUE)
    
    indices <- which(cost_matrix == min_cost, arr.ind = TRUE)
    
    # Filter valid indices
    indices <- indices[
      supply_left[indices[,1]] > 0 & demand_left[indices[,2]] > 0,
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

# Example input
cost_matrix <- matrix(c(
  6,3,4,
  4,8,4,
  1,7,2
), nrow = 3, byrow = TRUE)

supply <- c(50,40,60)
demand <- c(20,95,35)

# Run
result <- matrix_minima_method(cost_matrix, supply, demand)

cat("Allocation Matrix:\n")
print(result$allocation)

cat("Total Transportation Cost:", result$total_cost, "\n")