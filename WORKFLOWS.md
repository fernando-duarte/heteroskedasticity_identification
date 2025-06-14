# CI/CD Workflows Documentation

This document provides comprehensive documentation for all GitHub Actions workflows used in the `hetid` R package.

## Overview

The `hetid` package uses a comprehensive CI/CD pipeline to ensure code quality, security, and reproducibility. Our workflows are designed to:

- ✅ Test the package across multiple operating systems and R versions
- 🐳 Build and test Docker containers
- 📖 Generate and deploy documentation
- 🔒 Perform security analysis
- 🔄 Maintain up-to-date dependencies

## Workflow Files

### 1. R Package Workflow (`rworkflows.yml`)

**Purpose**: Main CI/CD workflow for R package testing, checking, and coverage reporting.

**Triggers**:
- Push to branches: `master`, `main`, `devel`, `RELEASE_**`
- Pull requests to the same branches

**Key Features**:
- **Multi-platform testing**: Runs on Ubuntu 22.04, macOS 14, and Windows 2022
- **R/Bioconductor versions**: Tests against both `devel` and `release` versions
- **Concurrency control**: Prevents duplicate runs with `cancel-in-progress: true`
- **Comprehensive checks**: Runs R CMD check, unit tests, code coverage, and builds documentation

**Matrix Configuration**:
```yaml
- Ubuntu 22.04 with Bioconductor devel
- macOS 14 with Bioconductor release  
- Windows 2022 with Bioconductor release
```

**Dependencies**: Uses `neurogenomics/rworkflows@v1` action for standardized R package workflows.

### 2. Docker Workflow (`docker.yml`)

**Purpose**: Build, test, and deploy Docker containers for the R package.

**Triggers**:
- Push to `main` and `develop` branches
- Pull requests to `main`
- Tags matching `v*`
- Manual workflow dispatch

**Key Features**:
- **Multi-stage builds**: Separate `builder`, `production`, and `development` targets
- **Security scanning**: Trivy vulnerability scanning with SARIF upload
- **Multi-architecture support**: Builds for `linux/amd64` and `linux/arm64`
- **GitHub Container Registry**: Pushes images to `ghcr.io`
- **Comprehensive testing**: Tests package installation and functionality in containers

**Build Matrix**:
- Builder image: Development dependencies
- Production image: Minimal runtime environment
- Development image: Full development environment with RStudio

**Security**: Scans all images and uploads results to GitHub Security tab.

### 3. Documentation Workflow (`pkgdown.yml`)

**Purpose**: Build and deploy package documentation website using pkgdown.

**Triggers**:
- Push to `main` or `master` branches
- Pull requests to the same branches
- Release publications
- Manual workflow dispatch

**Key Features**:
- **GitHub Pages deployment**: Automatic deployment to GitHub Pages
- **Artifact upload**: Stores built documentation as artifacts
- **Conditional deployment**: Only deploys on non-PR events
- **R setup**: Uses public RSPM for package installation

**Process**:
1. Builds documentation with `pkgdown::build_site_github_pages()`
2. Uploads as artifact
3. Deploys to GitHub Pages environment

### 4. Security Analysis Workflow (`codeql.yml`)

**Purpose**: Perform comprehensive security analysis on code and configurations.

**Triggers**:
- Push to `main`, `master`, or `develop` branches
- Pull requests to `main` or `master`
- Weekly schedule (Sundays at 3 AM UTC)
- Manual workflow dispatch

**Key Features**:
- **CodeQL analysis**: Scans JavaScript/TypeScript code
- **Dockerfile scanning**: Security analysis of Docker configurations
- **Workflow scanning**: Checks GitHub Actions workflows for vulnerabilities
- **Extended security queries**: Uses both security and quality query suites
- **SARIF integration**: Results uploaded to GitHub Security tab

**Scan Types**:
1. **Code analysis**: JavaScript/TypeScript in the repository
2. **Dockerfile security**: Vulnerability scanning of all Dockerfiles
3. **Workflow security**: Security analysis of GitHub Actions workflows

## Concurrency Management

All workflows implement concurrency controls to prevent resource waste:

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

This ensures that:
- Only one workflow run per branch/PR at a time
- New pushes cancel in-progress runs
- Resources are used efficiently

## Security Features

### 1. Dependency Scanning
- Dependabot monitors all dependencies
- Weekly updates for GitHub Actions
- Monthly updates for R packages

### 2. Container Security
- Trivy scans all Docker images
- Vulnerabilities uploaded to Security tab
- Scans for CRITICAL, HIGH, and MEDIUM severity issues

### 3. Code Security
- CodeQL analysis for supported languages
- Extended security and quality queries
- Weekly scheduled scans

## Runner Specifications

All workflows use pinned runner versions for reproducibility:

| Workflow | Runner | Version |
|----------|--------|---------|
| rworkflows | Ubuntu | 22.04 |
| rworkflows | macOS | 14 |
| rworkflows | Windows | 2022 |
| docker | Ubuntu | 22.04 |
| pkgdown | Ubuntu | 22.04 |
| codeql | Ubuntu | 22.04 |

## Secrets and Permissions

### Required Secrets
- `GITHUB_TOKEN`: Automatically provided by GitHub
- `DOCKER_TOKEN`: Optional, for DockerHub (not currently used)
- `CODECOV_TOKEN`: Optional, for Codecov uploads

### Permissions
Workflows request only necessary permissions:
- `contents: read` - Read repository content
- `packages: write` - Push Docker images
- `pages: write` - Deploy documentation
- `security-events: write` - Upload security results

## Best Practices

1. **Version Pinning**: All actions use specific versions (e.g., `@v4`, `@v3`)
2. **Matrix Testing**: Test across multiple OS and R versions
3. **Caching**: Use GitHub Actions cache for dependencies
4. **Artifacts**: Store test results and built packages
5. **Security First**: Regular scanning and updates

## Workflow Status Badges

The README displays real-time status badges for all workflows:
- R-CMD-check status
- Docker build status
- CodeQL analysis status
- Documentation deployment status

## Maintenance

### Regular Updates
- Dependabot proposes updates weekly/monthly
- Review and merge security updates promptly
- Test major version updates on feature branches

### Monitoring
- Check workflow run history regularly
- Monitor for deprecation warnings
- Review security alerts

## Troubleshooting

### Common Issues

1. **R Package Installation Failures**
   - Check DESCRIPTION file dependencies
   - Verify system dependencies in workflows

2. **Docker Build Failures**
   - Check Dockerfile syntax
   - Verify base image availability

3. **Documentation Build Failures**
   - Ensure all examples run without errors
   - Check for missing dependencies

### Getting Help
- Check workflow logs in the Actions tab
- Review similar issues in the repository
- Consult the neurogenomics/rworkflows documentation

## Future Improvements

Planned enhancements tracked in `CI_CD_improvement_checklist.md`:
- Performance monitoring metrics
- Workflow failure notifications
- Additional security scanning tools

---

Last updated: December 2024