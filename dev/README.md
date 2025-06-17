# Development Scripts and Tools

This directory contains development-only scripts and tools that are not part of the R package distribution.

## Directory Structure

```
dev/
├── docker/                    # Docker-related scripts and configurations
│   ├── scripts/              # Docker utility scripts
│   │   ├── benchmark-architectures.sh
│   │   ├── build.sh
│   │   ├── cache-cleanup.sh
│   │   ├── dev.sh
│   │   └── simulation.sh
│   └── PLATFORM_SPECIFIC_OPTIMIZATIONS.md
├── internal-docs/            # Internal documentation not for distribution
├── quick_ci_test.sh         # Quick CI/CD test for common issues
├── simulate_ci_cd.sh        # Simulate GitHub Actions workflows locally
└── test_workflows.sh        # Test workflows with 'act' tool

```

## Script Descriptions

### CI/CD Testing Scripts

- **quick_ci_test.sh**: Lightweight script to check for common CI/CD issues before pushing
- **simulate_ci_cd.sh**: Comprehensive simulation of GitHub Actions workflows locally
- **test_workflows.sh**: Validates workflow syntax and tests with 'act' tool

### Docker Scripts

Located in `docker/scripts/`:
- **build.sh**: Build Docker images for different targets (production, development, builder)
- **dev.sh**: Manage development Docker environment
- **simulation.sh**: Run package simulations in Docker containers
- **benchmark-architectures.sh**: Benchmark performance across architectures
- **cache-cleanup.sh**: Clean up Docker build caches

## Usage

These scripts are for development purposes only and are excluded from the R package build via `.Rbuildignore`.

### Running CI/CD Tests

```bash
# Quick test before pushing
./dev/quick_ci_test.sh

# Full CI/CD simulation
./dev/simulate_ci_cd.sh

# Test workflows with act
./dev/test_workflows.sh
```

### Using Docker Scripts

Most Docker operations are available through the Makefile:

```bash
# Build Docker images
make docker-build

# Start development environment
make docker-dev-start

# Run simulations
make docker-sim-test
```

## Note

All scripts in this directory are excluded from the R package distribution. They are maintained for developer convenience and workflow automation.
