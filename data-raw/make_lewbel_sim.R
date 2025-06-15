# Generate Lewbel simulation dataset
# True parameters: beta_P = -1

set.seed(2025)
n <- 200  # Small for fast CRAN checks, sufficient for IV

# Exogenous variables
X1 <- rnorm(n)
X2 <- rnorm(n)

# Heteroskedastic error in first stage (key for Lewbel identification)
v <- rnorm(n, sd = 0.5 + 0.5 * X2^2)

# Endogenous variable
P <- 0.3 * X1 + 0.7 * X2 + v

# Outcome equation error
eps <- rnorm(n)

# Outcome with true beta_P = -1
y <- 2 + 1.5 * X1 + 3 * X2 - 1.0 * P + eps

# Create data frame with ID column for row-order tests
lewbel_sim <- data.frame(
  id = seq_len(n),
  y = y,
  P = P,
  X1 = X1,
  X2 = X2
)

# Save with maximum compression
save(lewbel_sim, file = "data/lewbel_sim.rda", compress = "xz")

# Optimize file size
tools::resaveRdaFiles("data", compress = "xz")

# Clean up
rm(list = ls())