# Generate Lewbel simulation dataset
# True parameters: beta_P = -1

set.seed(2025)
n <- 200 # Small for fast CRAN checks, sufficient for IV

# Exogenous variables
x1 <- rnorm(n)
x2 <- rnorm(n)

# Heteroskedastic error in first stage (key for Lewbel identification)
v <- rnorm(n, sd = 0.5 + 0.5 * x2^2)

# Endogenous variable
p <- 0.3 * x1 + 0.7 * x2 + v

# Outcome equation error
eps <- rnorm(n)

# Outcome with true beta_P = -1
y <- 2 + 1.5 * x1 + 3 * x2 - 1.0 * p + eps

# Create data frame with ID column for row-order tests
lewbel_sim <- data.frame(
  id = seq_len(n),
  y = y,
  P = p,
  X1 = x1,
  X2 = x2
)

# Save with maximum compression
save(lewbel_sim, file = "data/lewbel_sim.rda", compress = "xz")

# Optimize file size
tools::resaveRdaFiles("data", compress = "xz")

# Clean up
rm(list = ls())
