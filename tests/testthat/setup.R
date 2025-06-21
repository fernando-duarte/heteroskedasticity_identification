# Common Setup for hetid Tests
# This file is sourced before running any tests

# Note: These common objects are created lazily within tests to avoid
# issues with package loading order

# Define common column names for validation
lewbel_data_cols <- c("Y1", "Y2", "Xk", "Z", "epsilon1", "epsilon2")
rigobon_data_cols <- c("Y1", "Y2", "Xk", "regime", "epsilon1", "epsilon2")
simulation_result_cols <- c("ols_gamma1", "tsls_gamma1", "first_stage_F")
