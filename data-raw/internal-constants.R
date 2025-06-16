# data-raw/internal-constants.R
# Create locked environment for immutable constants
constants_env <- new.env(parent = emptyenv())

# Statistical thresholds
constants_env$WEAK_INSTRUMENT_F_THRESHOLD <- 10L
constants_env$ALPHA_LEVEL <- 0.05
constants_env$Z_CRITICAL_95 <- 1.96

# Numerical tolerances
constants_env$MIN_EXPONENT <- -10L
constants_env$MAX_EXPONENT <- 10L
constants_env$WEAK_ID_TOLERANCE <- 1e-6
constants_env$INSTRUMENT_SD_THRESHOLD <- 1e-10

# Default simulation parameters
constants_env$DEFAULT_NUM_SIMULATIONS <- 200L
constants_env$DEFAULT_MAIN_SAMPLE_SIZE <- 500L
constants_env$DEFAULT_SAMPLE_SIZES <- c(250L, 500L, 1000L, 2000L)
constants_env$DEFAULT_VERIFICATION_SAMPLE_SIZE <- 10000L
constants_env$DEFAULT_TEST_DATA_SIZE <- 1000L

# Bootstrap parameters
constants_env$DEFAULT_BOOTSTRAP_REPS <- 100L
constants_env$DEFAULT_BOOTSTRAP_SUBSET_SIZE <- 10L
constants_env$DEFAULT_BOOTSTRAP_DEMO_SIZE <- 5L

# Demo parameters
constants_env$DEMO_SAMPLE_SIZES <- c(500L, 1000L)
constants_env$DEMO_BOOTSTRAP_REPS <- 50L
constants_env$DEMO_N_REPS <- 50L
constants_env$DEMO_BOOTSTRAP_SIZE <- 3L

# Parallel processing
constants_env$DEFAULT_PARALLEL_WORKER_OFFSET <- 1L

# Display formatting
constants_env$DISPLAY_DIGITS <- 4L
constants_env$BOOTSTRAP_TABLE_DISPLAY_LIMIT <- 10L

# Plotting constants
constants_env$PLOT_BASE_FONT_SIZE <- 14L
constants_env$PLOT_HISTOGRAM_BINS <- 50L
constants_env$PLOT_LINE_WIDTH_THICK <- 3L
constants_env$PLOT_LINE_WIDTH_NORMAL <- 2L
constants_env$PLOT_LINE_WIDTH_THIN <- 1L
constants_env$PLOT_BOOTSTRAP_DISPLAY_LIMIT <- 20L
constants_env$PLOT_MIN_BOOTSTRAP_THRESHOLD <- 5L

# Mathematical constants
constants_env$DEFAULT_X_MEAN <- 2
constants_env$DEFAULT_X_SD <- 1
constants_env$TWO_TAILED_MULTIPLIER <- 2L
constants_env$POINT_ID_TAU <- 0
constants_env$SUCCESS_EXIT_CODE <- 0L

# Seed management
constants_env$DEFAULT_BASE_SEED <- 123L
constants_env$DEFAULT_TEST_SEED <- 42L
constants_env$SEED_MULTIPLIER_MAIN <- 1000L
constants_env$SEED_MULTIPLIER_BOOTSTRAP <- 3000L
constants_env$SEED_OFFSET_BY_N <- 1L
constants_env$SEED_OFFSET_BY_DELTA <- 2L

# Array indexing (for clarity)
constants_env$FIRST_ELEMENT_IDX <- 1L
constants_env$SECOND_ELEMENT_IDX <- 2L
constants_env$LOWER_BOUND_IDX <- 1L
constants_env$UPPER_BOUND_IDX <- 2L
constants_env$FIRST_COLUMN_IDX <- 1L
constants_env$F_STATISTIC_IDX <- 1L

# Lock all bindings in the environment
for (name in ls(constants_env)) {
  lockBinding(name, constants_env)
}

# Lock environment to prevent modification
lockEnvironment(constants_env, TRUE)

# Save to R/sysdata.rda
save(constants_env, file = "R/sysdata.rda", compress = "xz")
cat("Constants saved to R/sysdata.rda\n")
