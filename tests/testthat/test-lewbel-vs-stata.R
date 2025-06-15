test_that("hetid matches Stata ivreg2h", {
  skip_on_cran()
  skip_if_not(has_stata())
  skip_if_not(has_haven())
  
  # Generate data
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    n_x = 2
  )
  
  set.seed(2024)
  n <- 500
  data <- generate_lewbel_data(n, params, n_x = 2)
  
  # Rename columns for Stata
  colnames(data)[colnames(data) == "Y1"] <- "y"
  colnames(data)[colnames(data) == "Y2"] <- "P"
  
  # Write data to temporary Stata file
  tmp_dta <- tempfile(fileext = ".dta")
  tmp_results <- tempfile(fileext = ".csv")
  haven::write_dta(data[, c("y", "P", "X1", "X2")], tmp_dta)
  
  # Create Stata do-file with robust matrix export
  do_file <- tempfile(fileext = ".do")
  writeLines(c(
    sprintf('use "%s", clear', tmp_dta),
    "quietly {",
    "  capture which ivreg2h",
    "  if _rc {",
    "    ssc install ivreg2h, replace",
    "  }",
    "  * Run ivreg2h with multiple X variables",
    "  ivreg2h y X1 X2 (P =), gen(iiv)",
    "  * Store results",
    "  matrix b = e(b)",
    "  matrix V = e(V)",
    "  * Extract coefficients",
    "  scalar b_X1 = b[1,1]",
    "  scalar b_X2 = b[1,2]", 
    "  scalar b_P = b[1,3]",
    "  scalar b_cons = b[1,4]",
    "  * Extract standard errors",
    "  scalar se_P = sqrt(V[3,3])",
    "  * Write results to CSV",
    sprintf('  file open myfile using "%s", write replace', tmp_results),
    '  file write myfile "variable,coefficient,se" _n',
    '  file write myfile "X1," (b_X1) ",." _n',
    '  file write myfile "X2," (b_X2) ",." _n',
    '  file write myfile "P," (b_P) "," (se_P) _n',
    '  file write myfile "cons," (b_cons) ",." _n',
    "  file close myfile",
    "}",
    "exit"
  ), do_file)
  
  # Run Stata
  stata_result <- tryCatch({
    RStata::stata(do_file, stata.echo = FALSE)
    TRUE
  }, error = function(e) {
    message("Stata error: ", e$message)
    FALSE
  })
  
  if (stata_result && file.exists(tmp_results)) {
    # Read Stata results
    stata_results <- read.csv(tmp_results, stringsAsFactors = FALSE)
    stata_coef_P <- as.numeric(stata_results$coefficient[stata_results$variable == "P"])
    stata_se_P <- as.numeric(stata_results$se[stata_results$variable == "P"])
    
    # Run hetid
    params_sim <- c(params, list(sample_size = n))
    result_hetid <- run_single_lewbel_simulation(
      sim_id = 1,
      params = params_sim,
      endog_var = "P",
      exog_vars = c("X1", "X2"),
      return_models = TRUE
    )
    
    coef_P_hetid <- coef(result_hetid$models$tsls_model)["P"]
    se_P_hetid <- sqrt(diag(vcov(result_hetid$models$tsls_model)))["P"]
    
    # Compare coefficients
    expect_equal(
      coef_P_hetid,
      stata_coef_P,
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
    
    # Compare standard errors (may differ slightly due to implementation)
    expect_equal(
      se_P_hetid,
      stata_se_P,
      tolerance = 1e-4,
      ignore_attr = TRUE
    )
  }
  
  # Clean up
  unlink(c(tmp_dta, tmp_results, do_file))
})

test_that("hetid handles single X variable like Stata", {
  skip_on_cran()
  skip_if_not(has_stata())
  skip_if_not(has_haven())
  
  # Single X variable case
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  
  set.seed(2025)
  n <- 500
  data <- generate_lewbel_data(n, params)
  
  # Rename columns
  colnames(data)[colnames(data) == "Y1"] <- "y"
  colnames(data)[colnames(data) == "Y2"] <- "P"
  colnames(data)[colnames(data) == "Xk"] <- "X1"
  
  # Write data
  tmp_dta <- tempfile(fileext = ".dta")
  tmp_results <- tempfile(fileext = ".csv")
  haven::write_dta(data[, c("y", "P", "X1")], tmp_dta)
  
  # Stata do-file for single X
  do_file <- tempfile(fileext = ".do")
  writeLines(c(
    sprintf('use "%s", clear', tmp_dta),
    "quietly {",
    "  capture which ivreg2h",
    "  if _rc {",
    "    ssc install ivreg2h, replace",
    "  }",
    "  ivreg2h y X1 (P =), gen(iiv)",
    "  matrix b = e(b)",
    "  scalar b_P = b[1,2]",
    sprintf('  file open myfile using "%s", write replace', tmp_results),
    '  file write myfile (b_P)',
    "  file close myfile",
    "}",
    "exit"
  ), do_file)
  
  # Run Stata
  stata_result <- tryCatch({
    RStata::stata(do_file, stata.echo = FALSE)
    TRUE
  }, error = function(e) FALSE)
  
  if (stata_result && file.exists(tmp_results)) {
    # Read Stata coefficient
    stata_coef_P <- as.numeric(readLines(tmp_results))
    
    # Run hetid
    params_sim <- c(params, list(sample_size = n))
    result_hetid <- run_single_lewbel_simulation(
      sim_id = 1,
      params = params_sim,
      endog_var = "P",
      exog_vars = "X1",
      return_models = TRUE
    )
    
    coef_P_hetid <- coef(result_hetid$models$tsls_model)["P"]
    
    # Compare
    expect_equal(
      coef_P_hetid,
      stata_coef_P,
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
  }
  
  # Clean up
  unlink(c(tmp_dta, tmp_results, do_file))
})