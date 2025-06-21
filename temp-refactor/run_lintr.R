# Apply lintr auto-fixes
library(lintr)

message("Running lintr to check for issues...")

# First, run lintr to see what issues exist
lints <- lint_package(".", cache = FALSE)

message("Found ", length(lints), " lint issues")

# Check if we have the apply_lints function (lintr >= 3.1)
if ("apply_lints" %in% ls("package:lintr")) {
  message("Applying auto-fixes...")

  # Apply fixes non-interactively
  lintr::apply_lints(".", ask = FALSE)

  message("Auto-fixes applied")
} else {
  message("lintr version does not support auto-fixes (need >= 3.1)")
  message("Current version: ", packageVersion("lintr"))
}

# Save lint report
if (length(lints) > 0) {
  lint_df <- as.data.frame(lints)
  write.csv(lint_df, "temp-refactor/lint_report.csv", row.names = FALSE)
  message("Lint report saved to temp-refactor/lint_report.csv")
}
