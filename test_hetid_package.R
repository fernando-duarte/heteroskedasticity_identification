# ============================================
# Complete Testing Guide for hetid Package
# Run this script in RStudio to test everything
# ============================================

# 1. SETUP AND LOAD PACKAGE
# ============================================
library(devtools)

# Load the package (run from package root directory)
load_all()

# Check if package loaded correctly
if ("hetid" %in% loadedNamespaces()) {
  cat("✓ Package loaded successfully!\n")
} else {
  cat("✗ Package failed to load\n")
}

# 2. CHECK PACKAGE STRUCTURE
# ============================================
cat("\n--- Package Check ---\n")
# Run this to check for issues (may take a minute)
# check()  # Uncomment to run full check

# Quick check of exported functions
cat("\nExported functions:\n")
ls("package:hetid")

# 3. TEST DATA GENERATION
# ============================================
cat("\n--- Testing Data Generation ---\n")

# Set up parameters
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)

# Generate data
set.seed(123)
test_data <- generate_lewbel_data(n = 1000, params = params)

# Check data structure
cat("Data dimensions:", dim(test_data), "\n")
cat("Variables:", names(test_data), "\n")
cat("First few rows:\n")
head(test_data)

# 4. TEST ASSUMPTION VERIFICATION
# ============================================
cat("\n--- Testing Lewbel Assumptions ---\n")
verify_lewbel_assumptions(params = params, verbose = TRUE)

# 5. TEST SINGLE SIMULATION
# ============================================
cat("\n--- Testing Single Simulation ---\n")

# Prepare parameters for simulation
sim_params <- c(params, list(
  sample_size = 1000,
  tau_set_id = 0.2,
  bootstrap_reps = 50
))

# Run single simulation
set.seed(456)
single_result <- run_single_lewbel_simulation(
  sim_id = 1,
  params = sim_params
)

# Display results
cat("\nSingle simulation results:\n")
print(single_result)

# 6. TEST BOUNDS CALCULATION
# ============================================
cat("\n--- Testing Bounds Calculation ---\n")

# Calculate bounds with different tau values
bounds_point <- calculate_lewbel_bounds(test_data, tau = 0)
bounds_set <- calculate_lewbel_bounds(test_data, tau = 0.2)

cat(
  "Point identification bounds (tau=0):",
  round(bounds_point$bounds, 4), "\n"
)
cat(
  "Set identification bounds (tau=0.2):",
  round(bounds_set$bounds, 4), "\n"
)

# 7. QUICK MONTE CARLO TEST
# ============================================
cat("\n--- Quick Monte Carlo Test (10 runs) ---\n")

# Run small Monte Carlo
results <- data.frame()
for (i in 1:10) {
  set.seed(2000 + i)
  sim <- run_single_lewbel_simulation(
    sim_id = i,
    params = sim_params
  )
  results <- rbind(results, sim)
}

# Summary statistics
cat(
  "\nOLS estimates - Mean:", round(mean(results$ols_gamma1), 4),
  "SD:", round(sd(results$ols_gamma1), 4), "\n"
)
cat(
  "TSLS estimates - Mean:", round(mean(results$tsls_gamma1), 4),
  "SD:", round(sd(results$tsls_gamma1), 4), "\n"
)
cat("Average F-stat:", round(mean(results$first_stage_F), 2), "\n")

# 8. TEST CONFIGURATION CREATION
# ============================================
cat("\n--- Testing Configuration ---\n")

# Create default config
config <- create_default_config()
cat("Config parameters:", length(config), "\n")
cat("Sample sizes:", config$sample_sizes, "\n")
cat("Number of simulations:", config$num_simulations, "\n")

# 9. TEST VISUALIZATION (if you want plots)
# ============================================
cat("\n--- Testing Visualization ---\n")

# Create a simple plot
if (nrow(results) > 0) {
  # Histogram of estimates
  par(mfrow = c(1, 2))
  hist(results$ols_gamma1,
    main = "OLS Estimates",
    xlab = "Estimate", col = "lightblue"
  )
  abline(v = params$gamma1, col = "red", lwd = 2)

  hist(results$tsls_gamma1,
    main = "TSLS (Lewbel) Estimates",
    xlab = "Estimate", col = "lightgreen"
  )
  abline(v = params$gamma1, col = "red", lwd = 2)
  par(mfrow = c(1, 1))

  cat("✓ Plots created successfully\n")
}

# 10. FULL DEMO (OPTIONAL - TAKES LONGER)
# ============================================
cat("\n--- Full Demo Instructions ---\n")
cat("To run the full demo with all features, uncomment and run:\n")
cat("# demo_results <- run_lewbel_demo(num_simulations = 100)\n")
cat("# print_simulation_summary(demo_results)\n")

# Alternative: Run with custom config
cat("\nOr create a custom configuration:\n")
cat("# custom_config <- create_default_config(\n")
cat("#   num_simulations = 500,\n")
cat("#   gamma1 = -0.5,\n")
cat("#   delta_het = 1.5\n")
cat("# )\n")
cat("# results <- run_lewbel_monte_carlo(custom_config)\n")

# 11. MEMORY CLEANUP
# ============================================
cat("\n--- Cleanup ---\n")
cat("Objects in workspace:", length(ls()), "\n")
cat("To clean up, run: rm(list = ls())\n")

cat("\n============================================\n")
cat("Testing complete! Check for any errors above.\n")
cat("============================================\n")
