# Makefile for heteroskedasticity package
# Provides convenient commands for different test levels

.PHONY: test-cran test-fast test-integration test-comprehensive test-all check-fast check-full clean

# CRAN tests only - very fast unit tests (< 1 minute)
test-cran:
	@echo "Running CRAN tests only..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='cran', NOT_CRAN=''); devtools::test()"

# Fast tests - CRAN + fast tests (for local development)
test-fast:
	@echo "Running CRAN and fast tests..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='fast', NOT_CRAN='true'); devtools::test()"

# Integration tests - CRAN + fast + integration
test-integration:
	@echo "Running CRAN, fast, and integration tests..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='integration', NOT_CRAN='true'); devtools::test()"

# Comprehensive tests - all tests
test-comprehensive:
	@echo "Running all tests (CRAN + fast + integration + comprehensive)..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='comprehensive', NOT_CRAN='true'); devtools::test()"

# Alias for comprehensive
test-all: test-comprehensive

# Quick check for local development (CRAN + fast tests)
check-fast:
	@echo "Running quick package check (CRAN + fast tests)..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='fast', NOT_CRAN='true'); devtools::check()"

# Full check with all tests
check-full:
	@echo "Running full package check (all tests)..."
	@Rscript -e "Sys.setenv(HETID_TEST_LEVEL='comprehensive', NOT_CRAN='true'); devtools::check()"

# Simulate CRAN check environment (proper workflow)
check-cran:
	@echo "Building package..."
	@R CMD build . --no-build-vignettes
	@echo "Running CRAN check on built package..."
	@R CMD check hetid_*.tar.gz --as-cran --no-manual
	@rm -f hetid_*.tar.gz

# Proper R CMD check workflow
check-proper:
	@echo "Building package with vignettes..."
	@R CMD build .
	@echo "Running R CMD check on built package..."
	@R CMD check hetid_*.tar.gz
	@rm -f hetid_*.tar.gz

# Documentation
document:
	@echo "Updating documentation..."
	@Rscript -e "devtools::document()"

# Install package
install:
	@echo "Installing package..."
	@Rscript -e "devtools::install()"

# Build package
build:
	@echo "Building package..."
	@Rscript -e "devtools::build()"

# Clean up
clean:
	@echo "Cleaning up..."
	@rm -rf *.tar.gz *.Rcheck
	@rm -f tests/testthat/*.log
	@rm -f tests/testthat/Rplots.pdf

# Help
help:
	@echo "Available targets:"
	@echo "  test-cran          - Run CRAN tests only (very fast)"
	@echo "  test-fast          - Run CRAN + fast tests (default for development)"
	@echo "  test-integration   - Run CRAN + fast + integration tests"
	@echo "  test-comprehensive - Run all tests"
	@echo "  test-all          - Alias for test-comprehensive"
	@echo "  check-fast        - Quick package check (CRAN + fast tests)"
	@echo "  check-full        - Full package check (all tests)"
	@echo "  check-cran        - Simulate CRAN check environment"
	@echo "  check-proper      - Proper R CMD check with build"
	@echo "  document          - Update documentation"
	@echo "  install           - Install package"
	@echo "  build             - Build package"
	@echo "  clean             - Clean up temporary files"
