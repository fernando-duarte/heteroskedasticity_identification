# Development Guide

This package uses modern development practices to ensure code quality and consistency.

## Development Environment Setup

Choose your preferred development environment:

### Option 1: GitHub Codespaces (Recommended for Quick Start) ☁️

**Zero setup required** - fully configured cloud development environment:

1. **Launch Codespace**:
   - Click "Code" → "Codespaces" → "Create codespace on main"
   - Wait 2-3 minutes for automatic setup

2. **Start Developing**:
   - RStudio Server auto-opens on port 8787
   - VS Code with R extensions ready to use
   - Package pre-loaded and ready to use
   - All tools and dependencies included

**Features**: RStudio Server, VS Code, R 4.5.0, all dependencies, Git integration, Docker support

> **New**: Modern devcontainer with R 4.5.0 available! Includes both VS Code and RStudio in one environment. See [.devcontainer/README-modern.md](.devcontainer/README-modern.md) for details.

### Option 2: Local Docker Development 🐳

**Consistent environment** across all machines:

1. **Prerequisites**: Docker Engine 28.0+ and docker-compose

2. **Quick Start**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   make dev-start  # Launches RStudio at http://localhost:8787
   ```

**R Versions**:
- Production images: R 4.5.0 (updated June 2025)
- CI/CD testing: Both R 4.5.0 (release) and R 4.4.x (oldrel)
- Full compatibility maintained

See [dev/internal-docs/DOCKER.md](dev/internal-docs/DOCKER.md) for comprehensive Docker documentation.

### Option 3: Local R Installation 💻

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

### Additional Tools

- **(Optional) For Cursor AI users**: The repository includes a `.cursorignore` file that prevents the AI from accessing sensitive files and improves performance by excluding large generated files.

## Code Quality Tools

The project uses several tools to maintain code quality:

- **Pre-commit hooks**: Automatically format code, check for issues, and ensure consistency
- **R CMD check**: Standard R package checks via GitHub Actions
- **Docker security scanning**: Trivy vulnerability assessment with SARIF integration
- **Automated dependency updates**: Dependabot for GitHub Actions and Docker images
- **Code coverage**: Test coverage reporting (to be implemented)
- **pkgdown**: Automatic documentation website generation

For detailed information about our CI/CD workflows, see [dev/internal-docs/WORKFLOWS.md](dev/internal-docs/WORKFLOWS.md).

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
- **`.cursorignore`**: Controls which files Cursor AI can access (security & performance)
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

See [dev/internal-docs/CI_CD_SUCCESS_SUMMARY.md](dev/internal-docs/CI_CD_SUCCESS_SUMMARY.md) for detailed information about our security practices and CI/CD setup.

## Docker Usage (Recommended for Local Development)

The package includes a complete Docker setup with 2025 best practices for easy development and deployment.

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

3. **Execute Monte Carlo Simulations**:
   ```bash
   # Quick simulation (100 iterations)
   make simulation-quick

   # Full simulation (1000 iterations)
   make simulation

   # Sensitivity analysis
   make simulation-sensitivity
   ```

4. **Interactive Development**:
   ```bash
   # Open R console in container
   make dev-r

   # Open bash shell
   make dev-shell

   # View logs
   make dev-logs
   ```

5. **Stop Environment**:
   ```bash
   make dev-stop
   ```

### Alternative Commands

If you prefer direct Docker commands:

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

See [dev/internal-docs/DOCKER.md](dev/internal-docs/DOCKER.md) for comprehensive documentation, troubleshooting, and advanced usage.
