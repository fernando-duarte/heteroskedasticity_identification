# Check which packages are installed for Prono replication

cat("=== CHECKING REQUIRED PACKAGES ===\n\n")

required_packages <- list(
  rugarch = "GARCH modeling (essential for Prono method)",
  ivreg = "Instrumental variables regression",
  AER = "Alternative IV package",
  gmm = "Generalized Method of Moments",
  tsmarch = "Modern multivariate GARCH (for diagonal GARCH)",
  tsgarch = "Univariate GARCH (companion to tsmarch)"
)

installed <- character()
missing <- character()

for (pkg in names(required_packages)) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    installed <- c(installed, pkg)
    cat("✓", pkg, "is installed -", required_packages[[pkg]], "\n")
  } else {
    missing <- c(missing, pkg)
    cat("✗", pkg, "is NOT installed -", required_packages[[pkg]], "\n")
  }
}

if (length(missing) > 0) {
  cat("\n\nTo install missing packages, run:\n")
  cat("install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))\n")

  # Special note for tsmarch/tsgarch
  if ("tsmarch" %in% missing || "tsgarch" %in% missing) {
    cat("\nNote: tsmarch and tsgarch are the modern replacements for rmgarch.\n")
    cat("They provide better performance and are actively maintained.\n")
  }
} else {
  cat("\n\nAll required packages are installed!\n")
}

# Check if we can at least run basic Prono without all packages
cat("\n\n=== MINIMAL REQUIREMENTS CHECK ===\n")
if ("rugarch" %in% installed) {
  cat("✓ Can run basic Prono method with univariate GARCH\n")
} else {
  cat("✗ Cannot run Prono method without rugarch package\n")
}

if ("gmm" %in% installed) {
  cat("✓ Can run GMM estimation\n")
} else {
  cat("✗ Cannot run GMM estimation without gmm package\n")
}

if (("ivreg" %in% installed) || ("AER" %in% installed)) {
  cat("✓ Can run 2SLS estimation\n")
} else {
  cat("✗ Cannot run 2SLS without ivreg or AER package\n")
}

# Show what we can demonstrate
cat("\n\n=== AVAILABLE DEMONSTRATIONS ===\n")
cat("Based on installed packages, you can run:\n\n")

cat("1. Basic summary statistics (always available)\n")

if ("rugarch" %in% installed && (("ivreg" %in% installed) || ("AER" %in% installed))) {
  cat("2. Prono IV with univariate GARCH ✓\n")
} else {
  cat("2. Prono IV with univariate GARCH ✗\n")
}

if ("rugarch" %in% installed && "gmm" %in% installed) {
  cat("3. Prono GMM estimation ✓\n")
} else {
  cat("3. Prono GMM estimation ✗\n")
}

if (("tsmarch" %in% installed) && ("tsgarch" %in% installed)) {
  cat("4. Diagonal GARCH (Prono's exact specification) ✓\n")
} else {
  cat("4. Diagonal GARCH (Prono's exact specification) ✗\n")
}
