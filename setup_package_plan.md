````markdown
# Integrating `{rworkflows}` + `templateR` into an Existing R Package Repository

Follow these steps to add the **templateR** scaffold and enable the **rworkflows** CI suite in your existing R package. Commit and push after each logical group of changes to keep review easy.

---

## 1. Clone Your Repository and Create a Feature Branch

```bash
git clone git@github.com:YOUR_ORG/YOUR_REPO.git
cd YOUR_REPO
git checkout -b add-rworkflows
````

---

## 2. Install the `rworkflows` Package

Open an R session (e.g., in RStudio or via Rscript) and run:

```r
# Install from CRAN
install.packages("rworkflows")

# (Optional) Install the latest development version:
# remotes::install_github("neurogenomics/rworkflows")
```

---

## 3. Pull in the `templateR` Skeleton

The `templateR` GitHub repo provides a CRAN-compatible scaffold. Merge only the necessary files:

```bash
# 1. Add templateR as a temporary remote
git remote add templateR https://github.com/neurogenomics/templateR.git
git fetch templateR

# 2. Check out scaffold files into your branch
git checkout templateR/main -- \
  DESCRIPTION \
  NAMESPACE \
  inst/ \
  man/ \
  R/000-package.R \
  .github/workflows/templateR.yaml

# 3. Remove the temporary remote
git remote remove templateR
```

---

## 4. Customize Your DESCRIPTION

Open `DESCRIPTION` in your editor and merge or update fields:

```text
Package:        YOURPACKAGENAME
Title:          A Package for X in Econometrics
Version:        0.1.0
Authors@R:      person("First", "Last",
                      email = "you@example.com",
                      role = c("aut", "cre"))
Description:    Brief description of what your package does.
License:        MIT + file LICENSE
Encoding:       UTF-8
LazyData:       true
Roxygen:        list(markdown = TRUE)
```

Also ensure you have a `LICENSE` file (e.g., MIT) and any CRAN-related metadata (e.g., `cran-comments.md` if desired).

---

## 5. Enable the `rworkflows` CI Suite

Back in R, run:

```r
rworkflows::use_workflow(template = "templateR")
```

This creates or updates `.github/workflows/rworkflows.yaml` to:

* Run `R CMD check` across platforms
* Build vignettes and pkgdown site
* (Optionally) Build and push a Docker container
* Perform Bioconductor checks, Python support, etc.

---

## 6. Review & Adjust the CI Workflow

1. Open `.github/workflows/rworkflows.yaml`.
2. Confirm the triggers (`push`, `pull_request`) cover your development branches.
3. If your econometrics code needs system libraries, add them under the `setup` or `jobs:` sections.

---

## 7. Add Package Dev Dependencies

In R, run:

```r
usethis::use_roxygen_md()        # Markdown-friendly Roxygen2
usethis::use_testthat()          # testthat unit testing
usethis::use_vignette("getting-started")  # starter vignette
usethis::use_pkgdown()           # pkgdown website
```

This ensures you have:

* Roxygen2 documentation (in `R/` and auto-generated `man/`)
* `tests/testthat/` with a placeholder test file
* `vignettes/getting-started.Rmd`
* A basic pkgdown config in `_pkgdown.yml`

---

## 8. Commit, Push & Open a Pull Request

```bash
git add .
git commit -m "Add templateR scaffold and rworkflows CI"
git push -u origin add-rworkflows
```

Then open a PR on GitHub for review.

---

## 9. Merge & Verify

1. Once the PR checks pass (build, tests, R CMD check, pkgdown), merge into `main`.
2. On GitHub Actions, confirm you see the **rworkflows** job run successfully and your pkgdown site (if configured) builds/deploys.

---

🎉 Your repository now has:

* A **CRAN-compatible** package scaffold from **templateR**
* A modern **rworkflows** GitHub Action pipeline (checks, docs, Docker, Bioconductor)
* Roxygen2, testthat, vignettes, and pkgdown configured

You’re ready to continue developing your econometrics data-analysis package with automated, reproducible CI/CD in place!
