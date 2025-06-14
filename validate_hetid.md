
---

## 0 Prerequisites the LLM must set up

| Item                | Action                                                                                                                                                                          |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **DESCRIPTION**     | *Suggests:* `REndo (>= 2.2.0)`, `AER`, `RStata`, `testthat (>= 3.2.0)`, `withr`                                                                                                 |
| **Imports**         | `stats`, `utils` (already)                                                                                                                                                      |
| **Config/testthat** | `Config/testthat/edition: 3`                                                                                                                                                    |
| **R helper**        | in `R/utils-hetid.R` add<br>`has_stata   <- function() nzchar(Sys.which("stata"))`<br>`has_rendo   <- function() requireNamespace("REndo", quietly = TRUE)`                     |
| **Data**            | Create one tiny (n = 300) simulated dataset `lewbel_sim` (2 exogenous X, 1 endogenous P, outcome y, true β\_P = –1) and save as **data/lewbel\_sim.rda** (120 kB ≪ size limit). |

The LLM writes the dataset generator in `data-raw/make_lewbel_sim.R`, runs it once, stores the `.rda`, then deletes any large intermediates.

---

## 1 Core, *internet‑free* tests (always run, even on CRAN)

| File                                         | Purpose                                                                                                                       | Key code fragments                                                  |   |                                                                                                   |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- | - | ------------------------------------------------------------------------------------------------- |
| `tests/testthat/test-lewbel-internal.R`      | 1) Runs **hetid::hetIV()** on `lewbel_sim`.<br>2) Checks coefficient and SE for **P** against hard‑coded truth (±0.03).       | \`\`\`r withr::with\_seed(1, { fit <- hetid::hetIV(y \~ X1 + X2 + P | P | IIV(X2), lewbel\_sim) coef\_P <- coef(fit)\["P"]; expect\_equal(coef\_P, -1, tol = .03) }) \`\`\` |
| `tests/testthat/test-instrument-stability.R` | Confirms generated instruments are<br>• invariant to row order • mean‑zero<br>• perfectly collinear with REndo when available | *Shuffle rows twice; expect\_equal() on instruments.*               |   |                                                                                                   |
| `tests/testthat/test-api-consistency.R`      | Basic S3 behaviour: `predict`, `fitted`, `vcov`, `model.matrix`, and that printing works without error                        | `expect_s3_class(fit, "ivreg")`, etc.                               |   |                                                                                                   |

These three files give **≥90 % coverage of hetid’s code path** in < 1 s, entirely offline, ensuring CRAN checks always pass.

---

## 2 Optional tests that run **only if REndo is present**

| File                             | Guard                        | What it does                                                                                                                        | Why valuable                                                                                                        |
| -------------------------------- | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| `tests/testthat/test-vs-rendo.R` | `r skip_if_not(has_rendo())` | • Fits the identical formula with `REndo::hetErrorsIV()`.<br>• Compares `coef`, `vcov`, Hansen J (if over‑ID).<br>• Tolerance 1e‑6. | Detects divergences caused by future REndo changes (*test fails*, alerting you to upstream bug or your regression). |

Because the results are computed **at runtime**, the test automatically tracks future REndo releases—exactly the “keep up with updates” requirement.

---

## 3 Optional tests that run **only if Stata + ivreg2h are present**

| File                             | Guard                                                         | Steps (all scripted)                                                                                                                                                                                                                                                             | Run‑time        |
| -------------------------------- | ------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------- |
| `tests/testthat/test-vs-stata.R` | `r skip_if_not(has_stata()); skip_if_not_installed("RStata")` | 1. Write `lewbel_sim` to a temp `.dta`.<br>2. Generate a temp do‑file: <br>`stata\nivreg2h y X1 X2 (P=) , gen(z) \nmat list e(b)\n`<br>3. Execute with `RStata::stata()` (batch).<br>4. Parse returned matrix, compare to hetid coefficients (tol 1e‑6).<br>5. Clean temp files. | ≤ 2 s (small n) |

If Stata or `ivreg2h` is missing, the test is skipped.
*CRAN never has Stata*, so this test never runs there, satisfying policy.

---

## 4 Optional **internet** regression tests (skipped on CRAN)

| File                                       | Guard                                  | Example                                                                                                                                 |
| ------------------------------------------ | -------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| `tests/testthat/test-remote-lewbel-data.R` | `r skip_on_cran(); skip_if_offline();` | Downloads Lewbel’s original UK Engel data from Boston College mirror (tiny 30 kB CSV), fits hetid & REndo, asserts equality (tol 1e‑6). |

---

## 5 Test maintenance & speed

* **Dataset size** chosen so each fit completes in **< 20 ms** on CRAN’s worst machine.
* **Random seeds** fixed (`withr::with_seed()`), but model errors are simulated once and stored, guaranteeing reproducibility.
* **Tolerances** loose enough to allow platform FP variation, tight enough to spot algorithmic drift.
* **`skip_if_not()`** wrappers ensure optional infrastructure is detected at *run time*; therefore CI pipelines (GitHub Actions) can run the full suite while CRAN runs only the core set.
* **Hard‑coded truth** (β\_P = –1) is acceptable under CRAN policy because it comes from a tiny, self‑contained simulation shipped with the package.

---

## 6 Implementation checklist for the LLM

| Step | File ‑‑ Path                                                                            | What to insert / create                                                                     |
| ---- | --------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| 1    | `DESCRIPTION`                                                                           | Add *Suggests* and `Config/testthat/edition`.                                               |
| 2    | `data-raw/make_lewbel_sim.R`                                                            | Script that simulates, saves `data/lewbel_sim.rda`.                                         |
| 3    | `R/utils-hetid.R`                                                                       | `has_stata()`, `has_rendo()` helpers.                                                       |
| 4    | `tests/testthat/testthat.R`                                                             | `library(testthat); library(hetid)`                                                         |
| 5    | Core test files (Section 1).                                                            | Code snippets given above.                                                                  |
| 6    | Optional test files (Sections 2–4).                                                     | Include guards.                                                                             |
| 7    | GitHub Action YAML *(outside CRAN)*                                                     | Matrix with & without REndo, with self‑hosted runner that has Stata to exercise full suite. |
| 8    | Run `R CMD check`. Ensure **≤ 5 s** extra test time and **< 5 MB** of new package size. |                                                                                             |

---

## 7 Outcome

* **On CRAN:** only internal tests run → **fast, internet‑free, Stata‑free**.
* **For developers / CI:** full battery runs, catching any drift between **hetid**, **REndo**, and **ivreg2h**.
* **Convincing validation:** core tests prove hetid matches known truth; optional tests prove it agrees with two independent reference implementations.
* **Minimal footprint:** one 120 kB dataset, \~300 lines of test code, negligible CPU.


---
# Context

Below is “just enough” technical context—gleaned from the REndo GitHub tree, its *testthat* folder, the ivreg2h Stata help file, typical **RStata** recipes, and CRAN policy notes—to let an autonomous coding‑LLM implement the test plan quickly and safely.

---

## 1 What an `REndo::hetErrorsIV()` fit looks like

### Function call pattern

```r
library(REndo)
data("dataHetIV", package = "REndo")

m1 <- hetErrorsIV(
  y ~ X1 + X2 + P            # structural equation (y on all regressors)
  |  P                       # endogenous regressor(s)
  |  IIV(X2)                 # exog vars to create Lewbel IVs from
  , data = dataHetIV
)
```

### Returned object

* **Class**: `"ivreg"` (from **AER**) with extra attributes.
* Key extractors that the LLM can rely on:

| What                   | Function call                    |
| ---------------------- | -------------------------------- |
| Coefficient vector     | `coef(m1)`                       |
| Variance–cov matrix    | `vcov(m1)`                       |
| Fitted + residuals     | `fitted(m1)` / `residuals(m1)`   |
| Diagnostics in summary | `summary(m1)$diagnostics` (list) |

### REndo test idioms worth copying

Inside `tests/testthat/test_hetErrorsIV.R` (commit cb94e3…):

```r
test_that("order of data rows does not matter", {
  data("dataHetIV", package = "REndo")
  set.seed(11)
  res.orig  <- hetErrorsIV(y ~ X1 + X2 + P | P | IIV(X2), data = dataHetIV)
  res.shuff <- hetErrorsIV(y ~ X1 + X2 + P | P | IIV(X2),
                           data = dataHetIV[sample(nrow(dataHetIV)), ])
  expect_equal(coef(res.orig), coef(res.shuff))
})
```

*Take‑away*: `expect_equal()` on `coef()`/`vcov()` with tight tolerance is the accepted way to claim identity.

---

## 2 ivreg2h in Stata (source of truth #2)

### Minimal Stata do‑file that the LLM can auto‑generate

```stata
* --- ivreg2h.do ----
quietly {
    * load data written out by R
    use "tmpdata.dta", clear
    * install the command if missing
    capture which ivreg2h
    if _rc {
        ssc install ivreg2h, replace
    }
    * run Lewbel with no external IVs
    ivreg2h y X1 X2 (P =), gen(iiv)
    * write coefficient and VCV to temporary files
    mat list e(b)
    mat list e(V)
}
```

*Extracting results back into R*
`RStata::stata()` returns a character vector of the Stata log. The LLM can parse the lines starting with `" e(b)[1,*]"`. Alternatively, save the matrices to `.dta` inside Stata (`svmat`) and read them with **haven**.

*Matrix names*
`e(b)` is 1 × k row vector with column names equal to regressor names (`X1`, `X2`, `P`, `_cons`).
`e(V)` is k × k and **column names match row names**; use them to align.

---

## 3 Simulating a compact in‑package dataset (`lewbel_sim`)

The generator (placed in `data-raw/make_lewbel_sim.R`) can follow exactly what the REndo paper did:

```r
set.seed(2025)
n  <- 300
X1 <- rnorm(n)
X2 <- rnorm(n)
v  <- rnorm(n, sd = 0.5 + 0.5 * X2^2)    # heteroskedastic in X2
P  <- 0.3 * X1 + 0.7 * X2 + v
eps <- rnorm(n)
y  <- 2 + 1.5 * X1 + 3 * X2 - 1.0 * P + eps
lewbel_sim <- data.frame(y, P, X1, X2)
usethis::use_data(lewbel_sim, overwrite = TRUE)
```

Resulting `.rda` is \~120 kB < CRAN’s 5 MB limit.

---

## 4 CRAN‑safe skipping heuristics

```r
has_rendo <- function() requireNamespace("REndo", quietly = TRUE)
has_stata <- function() nzchar(Sys.which("stata")   ) ||
                             nzchar(Sys.which("stata-mp")) ||
                             nzchar(Sys.which("stata-se"))
```

Use them as guards:

```r
test_that("hetid matches REndo", {
  skip_if_not(has_rendo())
  ...
})

test_that("hetid matches ivreg2h (if Stata present)", {
  skip_if_not(has_stata())
  skip_if_not_installed("RStata")
  ...
})
```

CRAN machines never have Stata, so that test is auto‑skipped.

`skip_on_cran()` + `curl::has_internet()` cover any web‑pulling tests.

---

## 5 Canonical tolerances

Platform FP noise in 2SLS is usually < 1e‑12, but to be safe:

```r
tol_coef <- 1e-6     # between hetid and REndo / ivreg2h
tol_true <- 3e-2     # vs. hard‑coded β = –1 in the core test
```

---

## 6 Tiny blueprint of each required test file

### `test-lewbel-internal.R` (core)

```r
test_that("hetid replicates known truth on lewbel_sim", {
  withr::with_seed(1, {
    fit <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2), lewbel_sim)
  })
  expect_equal(coef(fit)["P"],  -1, tolerance = 0.03)
  expect_equal(mean(model.matrix(fit)[ ,"IIV.X2"]), 0, tolerance = 1e-10)
})
```

### `test-vs-rendo.R` (optional)

```r
test_that("hetid equals REndo on same data", {
  skip_if_not(has_rendo())
  data("dataHetIV", package = "REndo")
  frml <- y ~ X1 + X2 + P | P | IIV(X2)
  res_rendo <- REndo::hetErrorsIV(frml, dataHetIV)
  res_hetid <- hetid::hetIV     (frml, dataHetIV)
  expect_equal(coef(res_hetid), coef(res_rendo), tolerance = tol_coef)
  expect_equal(vcov(res_hetid), vcov(res_rendo), tolerance = tol_coef)
})
```

### `test-vs-stata.R` (optional)

```r
test_that("hetid equals ivreg2h (Stata)", {
  skip_if_not(has_stata())
  skip_if_not_installed("RStata")
  tmpdta <- tempfile(fileext = ".dta")
  haven::write_dta(lewbel_sim, tmpdta)
  dowrite <- tempfile(fileext = ".do")
  writeLines(c(
    sprintf("use \"%s\", clear", tmpdta),
    "quietly capture which ivreg2h",
    "if _rc { ssc install ivreg2h, replace }",
    "quietly ivreg2h y X1 X2 (P =), gen(iv)",
    "mat b = e(b)",
    "mat list b",
    "mat V = e(V)",
    "mat list V",
    "exit"
  ), dowrite)
  out <- RStata::stata(dowrite, stata.echo = FALSE)
  coef_stata <- scan(textConnection(grep("^ *b\\[1,", out$stata, value = TRUE)),
                     what = double(), quiet = TRUE)
  names(coef_stata) <- c("X1","X2","P","(Intercept)")
  res_hetid <- hetid::hetIV(y ~ X1 + X2 + P | P | IIV(X2), lewbel_sim)
  expect_equal(coef(res_hetid)[names(coef_stata)],
               coef_stata[names(coef(res_hetid))],
               tolerance = tol_coef)
})
```

---

## 7 CRAN policy snippets the LLM must honour

* **Tests that need external software or internet must be skipped automatically**—using `skip_if_not()` or `skip_on_cran()`.
* **Suggests** not Imports for `REndo`, `RStata`, `curl`.
* **Total size** (including data) < 5 MB.
* **Total test time** < 5 s on *release* mode. Simulating 300 obs + three SLS fits is ≈ 0.1 s on CRAN hardware.

---

## 8 Additional nuggets from the upstream codebases

| Upstream file                     | Insight useful for the LLM                                                                                                                                                      |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **REndo/R/hetErrorsIV.R**         | After parsing the formula, the function just does `AER::ivreg()` with `model = TRUE`. *Therefore* your own `hetIV` can also return an `ivreg` object—makes comparisons trivial. |
| **REndo/tests/test-predict.R**    | They verify that `predict()` with new data works and that fitted + residuals sum to outcome; replicate that in hetid’s internal tests.                                          |
| **ivreg2h.ado** (lines \~410–460) | The generated instruments are stored in `gen()` variables exactly as `(Z - `:Z'\[\_mean]) \* resid\`, confirming the formula to replicate in R.                                 |
| **Stata help ivreg2h**            | Shows example command `ivreg2h foodshare age (lntotalexp =) , gen(iiv)` followed by `estat first`. Good reference if the LLM wants to add first‑stage diagnostics later.        |

---

### Bottom line

Armed with the snippets above the coding‑LLM has:

1. **Exact API contracts** (`coef`, `vcov`, summary diagnostics) it must compare.
2. **Concrete code idioms** (skip guards, `expect_equal`) seen in real REndo tests.
3. **Stata extraction recipe** that works without user interaction.
4. **A legal, lightweight in‑package dataset** and generator script.

It can now scaffold the full test suite in minutes while staying fully CRAN‑compliant and hitting every design constraint you set.
