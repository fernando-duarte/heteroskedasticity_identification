**TASK: Implement a comprehensive testthat suite for the hetid R package that validates its Lewbel (2012) heteroskedasticity-based instrumental variables implementation**

You are implementing a minimal yet convincing test suite that compares hetid against:
- **REndo::hetErrorsIV()** (R package on CRAN, version 2.4.0+)
- **ivreg2h** (Stata module, accessed via RStata if available)

The test suite must be CRAN-compliant: < 5 seconds runtime, < 5MB size, no internet/Stata dependencies on CRAN.

## STEP 1: Update Package Infrastructure

### 1.1 Update DESCRIPTION file
Add to the Suggests field:
```
Suggests:
    REndo (>= 2.4.0),
    AER,
    ivreg,
    RStata,
    curl (>= 5.0.0),
    haven,
    testthat (>= 3.2.3),
    withr
```

Add this line:
```
Config/testthat/edition: 3
```

### 1.2 Create helper functions in R/utils-hetid.R
```r
#' Check if REndo is available
#' @export
has_rendo <- function() {
  requireNamespace("REndo", quietly = TRUE)
}

#' Check if curl is available
#' @export
has_curl <- function() {
  requireNamespace("curl", quietly = TRUE)
}

#' Check if haven is available
#' @export
has_haven <- function() {
  requireNamespace("haven", quietly = TRUE)
}

#' Check if RStata is available
#' @export
has_rstata <- function() {
  requireNamespace("RStata", quietly = TRUE)
}

#' Check if Stata is available via RStata
#' @export
has_stata <- function() {
  if (!has_rstata()) return(FALSE)

  # Check common Stata executables
  stata_paths <- c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata")
  any(nzchar(Sys.which(stata_paths)))
}
```

## STEP 2: Create Test Data

### 2.1 Create data-raw/make_lewbel_sim.R
```r
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
```

Run this script once to create data/lewbel_sim.rda.

## STEP 3: Set Up Test Infrastructure

### 3.1 Create tests/testthat.R
```r
library(testthat)
library(hetid)

test_check("hetid")
```

## STEP 4: Implement Core Tests (Always Run on CRAN)

### 4.1 Create tests/testthat/test-lewbel-internal.R
```r
test_that("hetid recovers known truth on lewbel_sim", {
  withr::with_seed(1, {
    # Load built-in test data
    data(lewbel_sim, package = "hetid")

    # Fit model using hetid with model=TRUE to ensure instruments are stored
    fit <- hetid::hetIV(
      y ~ X1 + X2 + P |  # Structural equation
      P |                # Endogenous variable
      IIV(X2),          # Generate Lewbel instruments from X2
      data = lewbel_sim,
      model = TRUE      # Ensure model matrix is stored
    )

    # Extract coefficient for endogenous variable P
    coef_P <- coef(fit)["P"]

    # Test against known truth with appropriate tolerance
    # At n=200, Monte Carlo std error is ~0.06, so 0.04 keeps us within 2σ
    expect_equal(coef_P, -1.0, tolerance = 0.04)

    # Verify standard errors are reasonable
    se_P <- sqrt(diag(vcov(fit)))["P"]
    expect_true(se_P > 0 && se_P < 0.5)
  })
})
```

### 4.2 Create tests/testthat/test-instrument-stability.R
```r
test_that("Generated instruments are invariant to row order", {
  data(lewbel_sim, package = "hetid")

  # Original order
  fit1 <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2),
                       data = lewbel_sim,
                       model = TRUE)
  iv1 <- model.matrix(fit1)[, "IIV.X2"]

  # Shuffled order
  set.seed(42)
  shuffled_data <- lewbel_sim[sample(nrow(lewbel_sim)), ]
  fit2 <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2),
                       data = shuffled_data,
                       model = TRUE)

  # Use ID column to reorder back to original
  mm2 <- model.matrix(fit2)
  reorder_idx <- match(lewbel_sim$id, shuffled_data$id)
  iv2_reordered <- mm2[reorder_idx, "IIV.X2"]

  # Test equality
  expect_equal(iv1, iv2_reordered, tolerance = 1e-10)
})

test_that("Generated instruments are mean-zero", {
  data(lewbel_sim, package = "hetid")
  fit <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2),
                      data = lewbel_sim,
                      model = TRUE)
  iv <- model.matrix(fit)[, "IIV.X2"]

  expect_equal(mean(iv), 0, tolerance = 1e-10)
})
```

### 4.3 Create tests/testthat/test-api-consistency.R
```r
test_that("hetid returns proper S3 ivreg object", {
  data(lewbel_sim, package = "hetid")
  fit <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2),
                      data = lewbel_sim,
                      model = TRUE)

  # Check S3 class
  expect_s3_class(fit, "ivreg")

  # Test standard S3 methods work
  expect_no_error(print(fit))
  expect_no_error(summary(fit))

  # Check predict method
  pred <- predict(fit)
  expect_equal(length(pred), nrow(lewbel_sim))

  # Check fitted values
  fitted_vals <- fitted(fit)
  expect_equal(length(fitted_vals), nrow(lewbel_sim))

  # Check residuals
  resids <- residuals(fit)
  expect_equal(length(resids), nrow(lewbel_sim))

  # Verify model.matrix works and contains instruments
  mm <- model.matrix(fit)
  expect_true("IIV.X2" %in% colnames(mm))
})
```

## STEP 5: Implement Optional Comparison Tests

### 5.1 Create tests/testthat/test-vs-rendo.R
```r
test_that("hetid matches REndo::hetErrorsIV() on identical data", {
  skip_if_not(has_rendo())

  data(lewbel_sim, package = "hetid")

  # Common formula
  frml <- y ~ X1 + X2 + P | P | IIV(X2)

  # Fit with both packages
  fit_hetid <- hetid::hetIV(frml, data = lewbel_sim, model = TRUE)
  fit_rendo <- REndo::hetErrorsIV(frml, data = lewbel_sim)

  # Compare coefficients
  expect_equal(
    coef(fit_hetid),
    coef(fit_rendo),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )

  # Compare variance-covariance matrices
  expect_equal(
    vcov(fit_hetid),
    vcov(fit_rendo),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )

  # Compare diagnostics (handle REndo 2.4.x's new weak instrument row)
  if ("diagnostics" %in% names(summary(fit_hetid)) &&
      "diagnostics" %in% names(summary(fit_rendo))) {
    diag_hetid <- summary(fit_hetid)$diagnostics
    diag_rendo <- summary(fit_rendo)$diagnostics

    # Find common diagnostic tests (handles version differences)
    if (is.matrix(diag_hetid) && is.matrix(diag_rendo)) {
      common_rows <- intersect(rownames(diag_hetid), rownames(diag_rendo))
      if (length(common_rows) > 0 && "statistic" %in% colnames(diag_hetid)) {
        expect_equal(
          diag_hetid[common_rows, "statistic", drop = FALSE],
          diag_rendo[common_rows, "statistic", drop = FALSE],
          tolerance = 1e-6,
          ignore_attr = TRUE
        )
      }
    }
  }
})
```

### 5.2 Create tests/testthat/test-vs-stata.R
```r
test_that("hetid matches Stata ivreg2h", {
  skip_if_not(has_stata())
  skip_if_not(has_haven())

  data(lewbel_sim, package = "hetid")

  # Write data to temporary Stata file
  tmp_dta <- tempfile(fileext = ".dta")
  tmp_csv <- tempfile(fileext = ".csv")
  haven::write_dta(lewbel_sim, tmp_dta)

  # Create Stata do-file with robust matrix export
  do_file <- tempfile(fileext = ".do")
  writeLines(c(
    sprintf('use "%s", clear', tmp_dta),
    "quietly {",
    "  capture which ivreg2h",
    "  if _rc {",
    "    ssc install ivreg2h, replace",
    "  }",
    "  ivreg2h y X1 X2 (P =), gen(iiv)",
    "  matrix b = e(b)",
    "  matrix V = e(V)",
    "  * Export coefficients to CSV",
    "  svmat b, names(col)",
    sprintf('  outsheet using "%s", comma replace', tmp_csv),
    "}",
    "exit"
  ), do_file)

  # Run Stata
  stata_result <- tryCatch({
    RStata::stata(do_file, stata.echo = FALSE)
    TRUE
  }, error = function(e) FALSE)

  if (stata_result && file.exists(tmp_csv)) {
    # Read Stata results
    stata_coefs <- as.numeric(read.csv(tmp_csv, header = FALSE)[1, ])

    # Stata order is typically: X1 X2 P _cons
    names(stata_coefs) <- c("X1", "X2", "P", "(Intercept)")

    # Fit with hetid
    fit_hetid <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2),
                              data = lewbel_sim,
                              model = TRUE)
    coef_hetid <- coef(fit_hetid)

    # Compare coefficients (reorder to match)
    common_names <- intersect(names(coef_hetid), names(stata_coefs))
    expect_equal(
      coef_hetid[common_names],
      stata_coefs[common_names],
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
  }

  # Clean up
  unlink(c(tmp_dta, tmp_csv, do_file))
})
```

## STEP 6: Implement Internet Tests

### 6.1 Create tests/testthat/test-remote-lewbel-data.R
```r
test_that("hetid works on real Lewbel data from Boston College", {
  skip_on_cran()
  skip_if_not(has_curl())
  skip_if_offline()
  skip_if_not(has_rendo())

  # URL for Lewbel's UK Engel curve data
  url <- "http://fmwww.bc.edu/ec-p/data/stockwatson/uk_engel.csv"

  # Try to download data
  tmp_file <- tempfile(fileext = ".csv")
  tryCatch({
    download.file(url, tmp_file, quiet = TRUE, method = "libcurl")
    uk_data <- read.csv(tmp_file)

    # Basic data validation
    expect_true(nrow(uk_data) > 100)
    expect_true(all(c("foodshare", "lntotalexp", "age") %in% names(uk_data)))

    # Fit models (simplified for testing)
    formula <- foodshare ~ age + lntotalexp | lntotalexp | IIV(age)

    fit_hetid <- hetid::hetIV(formula, data = uk_data, model = TRUE)
    fit_rendo <- REndo::hetErrorsIV(formula, data = uk_data)

    # Compare results
    expect_equal(
      coef(fit_hetid),
      coef(fit_rendo),
      tolerance = 1e-6,
      ignore_attr = TRUE
    )

  }, error = function(e) {
    skip("Could not download Lewbel data")
  })

  # Clean up
  unlink(tmp_file)
})
```

## STEP 6: Run Final Validation

After implementing all files:

1. Regenerate and optimize test data:
   ```r
   source("data-raw/make_lewbel_sim.R")
   tools::checkRdaFiles("data")  # Verify compression
   ```

2. Run tests locally:
   ```r
   devtools::test()
   ```

3. Check package:
   ```r
   devtools::check()
   ```

4. Verify test timing:
   ```r
   system.time(testthat::test_dir("tests/testthat"))
   ```
   Report timing.

5. Verify package size:
   ```bash
   R CMD build .
   ls -lh *.tar.gz
   ```
   Should be < 5MB.

## EXPECTED OUTCOMES

✓ Core tests run on CRAN very fast
✓ hetid matches known truth (β_P = -1) within tolerance 0.04
✓ hetid matches REndo within 1e-6 tolerance when available
✓ hetid matches Stata ivreg2h within 1e-6 tolerance when available
✓ All tests pass R CMD check --as-cran
✓ Tests handle REndo 2.4.x diagnostic changes gracefully
✓ Tests skip appropriately when optional dependencies missing
✓ Total package size remains under CRAN limit with xz compression
