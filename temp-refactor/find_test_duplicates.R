# Find duplicate code patterns in test files

library(dupree)

# Find test files
test_files <- list.files(
  "tests/testthat",
  pattern = "^test-.*\\.R$",
  full.names = TRUE
)

# Run dupree on test files
dups <- dupree::dupree(test_files, min_block_size = 10)

# Print duplicates
print(dups)

# Save results
saveRDS(dups, "temp-refactor/test_duplicates.rds")
