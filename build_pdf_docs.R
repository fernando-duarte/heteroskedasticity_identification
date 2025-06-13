#!/usr/bin/env Rscript

# Build PDF documentation with multiple LaTeX passes to resolve cross-references

cat("Building PDF documentation with multiple LaTeX passes...\n\n")

# Function to run R CMD Rd2pdf and capture output
build_pdf <- function(pass_num, extra_args = "") {
  cat(sprintf("=== Pass %d ===\n", pass_num))

  cmd <- sprintf(
    "R CMD Rd2pdf . --no-preview --force --output=hetid-manual.pdf %s",
    extra_args
  )

  # Run command and capture output
  output <- system(cmd, intern = TRUE, ignore.stderr = FALSE)

  # Count warnings
  warnings <- grep("warning", output, ignore.case = TRUE, value = TRUE)
  cross_ref_warnings <- grep("has been referenced but does not exist|rerunfilecheck",
    warnings,
    value = TRUE
  )

  cat(sprintf("Total warnings: %d\n", length(warnings)))
  cat(sprintf("Cross-reference warnings: %d\n", length(cross_ref_warnings)))

  # Return number of cross-reference warnings
  length(cross_ref_warnings)
}

# Run up to 3 passes
max_passes <- 3
prev_warnings <- Inf

for (i in 1:max_passes) {
  # Use --no-clean after first pass to preserve auxiliary files
  extra_args <- if (i > 1) "--no-clean" else ""

  current_warnings <- build_pdf(i, extra_args)

  # Check if warnings decreased
  if (current_warnings == 0) {
    cat("\n✅ All cross-reference warnings resolved!\n")
    break
  } else if (current_warnings >= prev_warnings) {
    cat(sprintf("\n⚠️  Warnings did not decrease after pass %d. Stopping.\n", i))
    break
  }

  prev_warnings <- current_warnings

  if (i < max_passes) {
    cat("\nRunning another pass to resolve remaining references...\n\n")
  }
}

# Final summary
if (file.exists("hetid-manual.pdf")) {
  info <- file.info("hetid-manual.pdf")
  cat(sprintf(
    "\n📄 PDF documentation created: hetid-manual.pdf (%.1f KB)\n",
    info$size / 1024
  ))

  # Show remaining warnings if any
  if (is.finite(prev_warnings) && prev_warnings > 0) {
    cat(sprintf(
      "⚠️  %d cross-reference warnings remain (this is normal for R packages)\n",
      as.integer(prev_warnings)
    ))
  }
} else {
  cat("\n❌ PDF documentation was not created\n")
}
