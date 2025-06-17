#!/bin/bash

# CI/CD Local Simulation Script
# Simulates the GitHub Actions workflows locally for testing and validation
# Based on workflows: rworkflows.yml, docker.yml, pkgdown.yml

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SKIP_DOCKER=${SKIP_DOCKER:-false}
SKIP_SECURITY=${SKIP_SECURITY:-false}
SKIP_PKGDOWN=${SKIP_PKGDOWN:-false}
VERBOSE=${VERBOSE:-false}

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_section() {
    echo -e "\n${BLUE}========================================${NC}"
    echo -e "${BLUE} $1${NC}"
    echo -e "${BLUE}========================================${NC}\n"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check prerequisites
check_prerequisites() {
    print_section "Checking Prerequisites"

    local missing_deps=()

    # Check R
    if ! command_exists R; then
        missing_deps+=("R")
    else
        print_success "R is installed: $(R --version | head -1)"
    fi

    # Check Docker (if not skipping)
    if [ "$SKIP_DOCKER" != "true" ]; then
        if ! command_exists docker; then
            missing_deps+=("docker")
        else
            print_success "Docker is installed: $(docker --version)"
        fi
    fi

    # Check git
    if ! command_exists git; then
        missing_deps+=("git")
    else
        print_success "Git is installed: $(git --version)"
    fi

    # Check if we're in a git repository
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        print_error "Not in a git repository"
        exit 1
    fi

    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing dependencies: ${missing_deps[*]}"
        print_status "Please install missing dependencies and try again"
        exit 1
    fi

    print_success "All prerequisites satisfied"
}

# Function to simulate R workflows
simulate_r_workflows() {
    print_section "Simulating R Workflows (rworkflows.yml)"

    # Check if DESCRIPTION file exists
    if [ ! -f "DESCRIPTION" ]; then
        print_error "DESCRIPTION file not found. Are you in an R package directory?"
        return 1
    fi

    print_status "Installing R dependencies..."
    R -e "
    if (!require('devtools', quietly = TRUE)) install.packages('devtools')
    if (!require('testthat', quietly = TRUE)) install.packages('testthat')
    if (!require('covr', quietly = TRUE)) install.packages('covr')
    devtools::install_deps(dependencies = TRUE)
    " || {
        print_warning "Some R dependencies may have failed to install"
    }

    print_status "Running R CMD check..."
    R CMD build . || {
        print_error "R CMD build failed"
        return 1
    }

    # Find the built package
    PKG_TAR=$(ls -t *.tar.gz | head -1)
    if [ -n "$PKG_TAR" ]; then
        print_status "Running R CMD check on $PKG_TAR..."
        R CMD check --as-cran "$PKG_TAR" || {
            print_warning "R CMD check completed with warnings/errors"
        }
    fi

    print_status "Running package tests..."
    R -e "devtools::test()" || {
        print_warning "Some tests may have failed"
    }

    print_status "Running test coverage analysis..."
    R -e "
    if (require('covr', quietly = TRUE)) {
        cov <- covr::package_coverage()
        print(cov)
        covr::report(cov, file = 'coverage-report.html')
        cat('Coverage report saved to coverage-report.html\n')
    } else {
        cat('covr package not available, skipping coverage\n')
    }
    " || {
        print_warning "Coverage analysis failed"
    }

    print_success "R workflows simulation completed"
}

# Function to simulate Docker workflows
simulate_docker_workflows() {
    if [ "$SKIP_DOCKER" = "true" ]; then
        print_warning "Skipping Docker workflows (SKIP_DOCKER=true)"
        return 0
    fi

    print_section "Simulating Docker Workflows (docker.yml)"

    # Check if Dockerfiles exist
    if [ ! -f "Dockerfile" ]; then
        print_warning "Dockerfile not found, skipping Docker simulation"
        return 0
    fi

    print_status "Building Docker images..."

    # Build production image
    print_status "Building production image..."
    docker build -t hetid:production --target production . || {
        print_error "Failed to build production image"
        return 1
    }

    # Build builder image
    print_status "Building builder image..."
    docker build -t hetid:builder --target builder . || {
        print_error "Failed to build builder image"
        return 1
    }

    # Build development image if Dockerfile.dev exists
    if [ -f "Dockerfile.dev" ]; then
        print_status "Building development image..."
        docker build -t hetid:development -f Dockerfile.dev . || {
            print_warning "Failed to build development image"
        }
    fi

    print_status "Testing Docker images..."

    # Test production image
    print_status "Testing production image..."
    docker run --rm hetid:production R -e "library(hetid); cat('Production image test passed\n')" || {
        print_warning "Production image test failed"
    }

    # Test builder image with package tests
    print_status "Testing builder image with package tests..."
    docker run --rm -v "$(pwd):/workspace" -w /workspace hetid:builder R -e "devtools::test()" || {
        print_warning "Builder image tests failed"
    }

    print_success "Docker workflows simulation completed"
}

# Function to simulate pkgdown workflows
simulate_pkgdown_workflows() {
    if [ "$SKIP_PKGDOWN" = "true" ]; then
        print_warning "Skipping pkgdown workflows (SKIP_PKGDOWN=true)"
        return 0
    fi

    print_section "Simulating pkgdown Workflows (pkgdown.yml)"

    print_status "Installing pkgdown..."
    R -e "
    if (!require('pkgdown', quietly = TRUE)) install.packages('pkgdown')
    " || {
        print_warning "Failed to install pkgdown"
        return 0
    }

    print_status "Building pkgdown site..."
    R -e "pkgdown::build_site()" || {
        print_warning "pkgdown site build failed"
        return 0
    }

    if [ -d "docs" ]; then
        print_success "pkgdown site built successfully in docs/ directory"
        print_status "You can view the site by opening docs/index.html in a browser"
    fi

    print_success "pkgdown workflows simulation completed"
}

# Function to simulate security scans
simulate_security_scans() {
    if [ "$SKIP_SECURITY" = "true" ]; then
        print_warning "Skipping security scans (SKIP_SECURITY=true)"
        return 0
    fi

    print_section "Simulating Security Scans (Docker Security)"

    # Check if trivy is available
    if command_exists trivy; then
        print_status "Running Trivy security scans..."

        # Scan Dockerfiles
        if [ -f "Dockerfile" ]; then
            print_status "Scanning Dockerfiles..."
            trivy config . --format table || {
                print_warning "Dockerfile security scan completed with issues"
            }
        fi

        # Scan workflows
        if [ -d ".github/workflows" ]; then
            print_status "Scanning GitHub workflows..."
            trivy config .github/workflows --format table || {
                print_warning "Workflow security scan completed with issues"
            }
        fi

        # Scan Docker images if they exist
        if [ "$SKIP_DOCKER" != "true" ] && docker images hetid:production >/dev/null 2>&1; then
            print_status "Scanning Docker images..."
            trivy image hetid:production --format table || {
                print_warning "Docker image security scan completed with issues"
            }
        fi
    else
        print_warning "Trivy not installed, skipping security scans"
        print_status "To install Trivy: https://aquasecurity.github.io/trivy/latest/getting-started/installation/"
    fi

    print_success "Security scans simulation completed"
}

# Function to cleanup
cleanup() {
    print_section "Cleanup"

    # Remove built packages
    if ls *.tar.gz >/dev/null 2>&1; then
        print_status "Removing built packages..."
        rm -f *.tar.gz
    fi

    # Remove check directories
    if ls *.Rcheck >/dev/null 2>&1; then
        print_status "Removing R check directories..."
        rm -rf *.Rcheck
    fi

    print_success "Cleanup completed"
}

# Main execution
main() {
    print_section "CI/CD Local Simulation"
    print_status "Starting local simulation of GitHub Actions workflows"
    print_status "Use environment variables to control execution:"
    print_status "  SKIP_DOCKER=true    - Skip Docker workflows"
    print_status "  SKIP_SECURITY=true  - Skip security scans"
    print_status "  SKIP_PKGDOWN=true   - Skip pkgdown workflows"
    print_status "  VERBOSE=true        - Enable verbose output"

    # Set verbose mode
    if [ "$VERBOSE" = "true" ]; then
        set -x
    fi

    # Check prerequisites
    check_prerequisites

    # Run simulations
    simulate_r_workflows
    simulate_docker_workflows
    simulate_pkgdown_workflows
    simulate_security_scans

    # Cleanup
    cleanup

    print_section "Simulation Complete"
    print_success "All CI/CD workflow simulations completed!"
    print_status "Check the output above for any warnings or errors"

    if [ -f "coverage-report.html" ]; then
        print_status "Coverage report: coverage-report.html"
    fi

    if [ -d "docs" ]; then
        print_status "Documentation site: docs/index.html"
    fi
}

# Handle script arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [options]"
        echo "Options:"
        echo "  --help, -h          Show this help message"
        echo "  --skip-docker       Skip Docker workflows"
        echo "  --skip-security     Skip security scans"
        echo "  --skip-pkgdown      Skip pkgdown workflows"
        echo "  --verbose           Enable verbose output"
        echo ""
        echo "Environment variables:"
        echo "  SKIP_DOCKER=true    Skip Docker workflows"
        echo "  SKIP_SECURITY=true  Skip security scans"
        echo "  SKIP_PKGDOWN=true   Skip pkgdown workflows"
        echo "  VERBOSE=true        Enable verbose output"
        exit 0
        ;;
    --skip-docker)
        export SKIP_DOCKER=true
        ;;
    --skip-security)
        export SKIP_SECURITY=true
        ;;
    --skip-pkgdown)
        export SKIP_PKGDOWN=true
        ;;
    --verbose)
        export VERBOSE=true
        ;;
esac

# Run main function
main "$@"
