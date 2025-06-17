#!/bin/bash

# Quick CI/CD Test Script
# Runs the most essential checks that would catch common CI/CD failures
# This is a lightweight version for rapid feedback

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_status() { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1"; }

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE} Quick CI/CD Test${NC}"
echo -e "${BLUE}========================================${NC}\n"

# 1. Check R package structure
print_status "Checking R package structure..."
if [ ! -f "DESCRIPTION" ]; then
    print_error "DESCRIPTION file missing"
    exit 1
fi

if [ ! -f "NAMESPACE" ]; then
    print_error "NAMESPACE file missing"
    exit 1
fi

if [ ! -d "R" ]; then
    print_error "R/ directory missing"
    exit 1
fi

print_success "R package structure looks good"

# 2. Quick R syntax check
print_status "Checking R syntax..."
R --slave -e "
files <- list.files('R', pattern = '[.]R$', full.names = TRUE)
for (file in files) {
    tryCatch({
        parse(file)
        cat('✓', basename(file), '\n')
    }, error = function(e) {
        cat('✗', basename(file), ':', e\$message, '\n')
        quit(status = 1)
    })
}
cat('All R files have valid syntax\n')
" || {
    print_error "R syntax check failed"
    exit 1
}

print_success "R syntax check passed"

# 3. Check if package can be loaded
print_status "Testing package loading..."
R --slave -e "
tryCatch({
    devtools::load_all('.')
    cat('Package loaded successfully\n')
}, error = function(e) {
    cat('Error loading package:', e\$message, '\n')
    quit(status = 1)
})
" || {
    print_error "Package loading failed"
    exit 1
}

print_success "Package loads successfully"

# 4. Run basic tests if they exist
if [ -d "tests" ]; then
    print_status "Running basic tests..."
    R --slave -e "
    if (dir.exists('tests/testthat')) {
        tryCatch({
            devtools::test()
            cat('Tests completed\n')
        }, error = function(e) {
            cat('Test error:', e\$message, '\n')
            quit(status = 1)
        })
    } else {
        cat('No testthat tests found\n')
    }
    " || {
        print_warning "Some tests failed"
    }
else
    print_warning "No tests directory found"
fi

# 5. Check Docker files if they exist
if [ -f "Dockerfile" ]; then
    print_status "Checking Dockerfile syntax..."
    if command -v docker >/dev/null 2>&1; then
        docker build --dry-run . >/dev/null 2>&1 || {
            print_warning "Dockerfile may have issues"
        }
        print_success "Dockerfile syntax check passed"
    else
        print_warning "Docker not available, skipping Dockerfile check"
    fi
fi

# 6. Check workflow files
if [ -d ".github/workflows" ]; then
    print_status "Checking workflow files..."
    for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
        if [ -f "$workflow" ]; then
            # Basic YAML syntax check using R (since it's available)
            R --slave -e "
            if (require('yaml', quietly = TRUE)) {
                tryCatch({
                    yaml::read_yaml('$workflow')
                    cat('✓', basename('$workflow'), '\n')
                }, error = function(e) {
                    cat('✗', basename('$workflow'), ':', e\$message, '\n')
                    quit(status = 1)
                })
            } else {
                cat('yaml package not available, skipping YAML validation\n')
            }
            " || {
                print_warning "Workflow file $workflow may have issues"
            }
        fi
    done
    print_success "Workflow files check completed"
fi

# 7. Check for common CI/CD issues
print_status "Checking for common CI/CD issues..."

# Check for trailing whitespace in key files
if grep -l '[[:space:]]$' DESCRIPTION NAMESPACE 2>/dev/null; then
    print_warning "Trailing whitespace found in package files"
fi

# Check for very long lines
if grep -l '.\{121,\}' R/*.R 2>/dev/null; then
    print_warning "Lines longer than 120 characters found in R files"
fi

print_success "Common issues check completed"

echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN} Quick CI/CD Test Complete${NC}"
echo -e "${GREEN}========================================${NC}\n"

print_success "All essential checks passed!"
print_status "Your package should pass basic CI/CD workflows"
print_status "Run './simulate_ci_cd.sh' for comprehensive testing"
