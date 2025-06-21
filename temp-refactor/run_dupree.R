# Run dupree analysis and save report
library(dupree)
library(dplyr)

message("Running dupree analysis...")
dup_results <- dupree_package(".")

# Check the structure of results
message("Structure of dupree results:")
str(dup_results)

# Convert to data frame if needed
if (!is.data.frame(dup_results)) {
  dup_df <- as.data.frame(dup_results)
} else {
  dup_df <- dup_results
}

# Filter for similarity > 0.5 as per plan
if ("score" %in% names(dup_df)) {
  significant_dups <- dup_df[dup_df$score > 0.5, ]

  message("Found ", nrow(dup_df), " total duplications")
  message("Found ", nrow(significant_dups), " duplications with score > 0.5")

  # Save full results
  write.csv(dup_df, file = "temp-refactor/duplication_report_full.csv", row.names = FALSE)

  # Save significant results
  write.csv(significant_dups, file = "temp-refactor/duplication_report.csv", row.names = FALSE)

  # Display summary
  if (nrow(significant_dups) > 0) {
    message("\nTop duplications (score > 0.5):")
    print(head(significant_dups[order(significant_dups$score, decreasing = TRUE), ], 10))
  } else {
    message("No significant duplications found with score > 0.5")
  }
} else {
  message("Unexpected dupree output format")
  # Save whatever we got
  saveRDS(dup_results, "temp-refactor/duplication_report.rds")
}
