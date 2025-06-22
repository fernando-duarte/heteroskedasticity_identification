# Development Guide

This package uses modern development practices to ensure code quality and consistency.

## Development Environment Setup

Choose your preferred development environment:

### Option 1: Local Docker Development 🐳 (Recommended)

**Consistent environment** across all machines:

1. **Prerequisites**: Docker Engine 28.0+ and docker-compose

2. **Quick Start**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   make dev-start  # Launches RStudio at http://localhost:8787
   ```

**R Versions**:
- Production images: R 4.5.0
- CI/CD testing: Both R 4.5.0 (release) and R 4.4.x (oldrel)

### Option 2: Local R Installation 💻

**Traditional setup** for local development:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   ```

2. **Install development dependencies**:
   ```r
   # Install devtools if not already installed
   install.packages("devtools")

   # Install package dependencies
   devtools::install_deps(dependencies = TRUE)
   ```

3. **Install pre-commit hooks** (requires Python):
   ```bash
   pip install pre-commit
   pre-commit install
   ```

## Code Quality Tools

The project uses several tools to maintain code quality:

- **Pre-commit hooks**: Automatically format code, check for issues, and ensure consistency
- **R CMD check**: Standard R package checks via GitHub Actions
- **Docker security scanning**: Trivy vulnerability assessment with SARIF integration
- **Automated dependency updates**: Dependabot for GitHub Actions and Docker images
- **Code coverage**: Test coverage reporting (to be implemented)
- **pkgdown**: Automatic documentation website generation

## Pre-commit Hooks

The pre-commit hooks will automatically:
- Format R code using the tidyverse style guide (`styler`)
- Check for code quality issues (`lintr`)
- Check spelling
- Ensure R code is parsable
- Remove trailing whitespace and fix file endings
- Prevent committing large files or common R artifacts
- Sort entries in `.Rbuildignore`

To manually run hooks on all files:
```bash
pre-commit run --all-files
```

## Ignore Files

The project includes comprehensive ignore files following 2025 best practices:

- **`.gitignore`**: Excludes common R, OS-specific, and IDE files from version control
- **`.Rbuildignore`**: Excludes development files from the built R package
- **`.dockerignore`**: Optimizes Docker build context

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines on:
- Our development workflow
- Coding standards
- How to submit pull requests
- Setting up your development environment

### Security Setup for Contributors

This repository uses automated security scanning with `oysteR` to check for vulnerabilities in R package dependencies. To avoid rate limiting issues when running security checks:

1. **Register for OSS Index credentials** (free):
   - Go to https://ossindex.sonatype.org/
   - Create an account
   - Get your API token from User Settings

2. **For local development**, add to your `.Renviron`:
   ```
   OSSINDEX_USER=your-email@example.com
   OSSINDEX_TOKEN=your-api-token
   ```

3. **For GitHub Actions** (maintainers only):
   - Add `OSSINDEX_USER` and `OSSINDEX_TOKEN` as repository secrets
   - Go to Settings → Secrets and variables → Actions

## Docker Usage (Recommended for Local Development)

The package includes a complete Docker setup for easy development and deployment.

### Quick Start Guide

**Prerequisites**: Docker Engine 28.0+ and docker-compose

1. **Start Development Environment**:
   ```bash
   make dev-start
   ```
   This launches RStudio Server at http://localhost:8787 with the package pre-loaded.

2. **Run Package Tests**:
   ```bash
   make test
   ```
3. **Use Docker commands:**

```bash
# Development with RStudio
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d hetid-dev

# Run production simulation
docker run --rm hetid:latest R -e "library(hetid); run_lewbel_monte_carlo()"

# Build production image
docker build -t hetid:latest --target production .
```

### Platform-Specific Optimizations

**Apple Silicon (M1/M2/M3) Users**: For 40% faster builds, use x86_64 emulation:

```bash
# Force x86_64 architecture for pak compatibility
docker build --platform linux/amd64 -t hetid:dev-x86 -f Dockerfile.dev .
docker run --platform linux/amd64 -p 8787:8787 hetid:dev-x86
```

This works because pak's parallel installation (even with emulation overhead) is much faster than sequential `install.packages()` on native ARM64.

### Docker Features

- **Multi-stage builds** for optimized production images
- **RStudio Server** for interactive development
- **Parallel processing** optimized for simulations
- **Automated testing** and quality checks
- **Security hardened** with non-root users
- **Multi-platform** support (AMD64/ARM64)
