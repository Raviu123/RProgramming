library(lpSolve)

n <- as.integer(readline("Enter number of workers/jobs: "))

cat("Enter cost matrix:\n")
cost <- matrix(scan(n = n*n), n, byrow = TRUE)

# Solve assignment problem
res <- lp.assign(cost)

cat("\nAssignment Matrix:\n")
print(res$solution)

cat("\nMinimum Cost:", res$objval, "\n")

cat("\nWorker -> Job:\n")
for (i in 1:n) {
  j <- which(res$solution[i, ] == 1)
  cat("Worker", i, "-> Job", j, "\n")
}