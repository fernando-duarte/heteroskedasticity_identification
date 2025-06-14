# CI/CD Local Simulation

This directory contains scripts to simulate your GitHub Actions CI/CD workflows locally, allowing you to test changes before pushing to GitHub.

## Quick Start

### 1. Quick Test (Recommended for rapid feedback)
```bash
./quick_ci_test.sh
```

This runs essential checks that catch most common CI/CD failures:
- R package structure validation
- R syntax checking
- Package loading test
- Basic test execution
- Dockerfile syntax check
- Workflow file validation
- Common formatting issues

### 2. Full Simulation (Comprehensive testing)
```bash
./simulate_ci_cd.sh
```

This simulates all GitHub Actions workflows:
- **R workflows** (rworkflows.yml): R CMD check, tests, coverage
- **Docker workflows** (docker.yml): Build and test all Docker images
- **Documentation** (pkgdown.yml): Build package website
- **Security scans** (codeql.yml): Trivy security scanning

## Workflow Coverage

### R Workflows (rworkflows.yml)
- ✅ R CMD build and check
- ✅ Package dependency installation
- ✅ Test suite execution
- ✅ Code coverage analysis
- ✅ Multi-platform simulation (Ubuntu, macOS, Windows logic)

### Docker Workflows (docker.yml)
- ✅ Production image build and test
- ✅ Builder image build and test
- ✅ Development image build (if Dockerfile.dev exists)
- ✅ Package functionality testing in containers
- ✅ Security scanning with Trivy

### Documentation Workflows (pkgdown.yml)
- ✅ pkgdown site generation
- ✅ Documentation deployment simulation

### Security Workflows (codeql.yml)
- ✅ Dockerfile security scanning
- ✅ Workflow security scanning
- ✅ Container image vulnerability scanning

## Prerequisites

### Required
- **R** (with devtools, testthat, covr packages)
- **Git** (must be in a git repository)

### Optional
- **Docker** (for Docker workflow simulation)
- **Trivy** (for security scanning)

### Installing Optional Dependencies

#### Docker
- macOS: `brew install docker` or Docker Desktop
- Ubuntu: `sudo apt-get install docker.io`
- Windows: Docker Desktop

#### Trivy
```bash
# macOS
brew install trivy

# Ubuntu
sudo apt-get install wget apt-transport-https gnupg lsb-release
wget -qO - https://aquasecurity.github.io/trivy-repo/deb/public.key | sudo apt-key add -
echo "deb https://aquasecurity.github.io/trivy-repo/deb $(lsb_release -sc) main" | sudo tee -a /etc/apt/sources.list.d/trivy.list
sudo apt-get update
sudo apt-get install trivy
```

## Configuration Options

### Environment Variables
```bash
# Skip specific workflow types
export SKIP_DOCKER=true      # Skip Docker workflows
export SKIP_SECURITY=true    # Skip security scans
export SKIP_PKGDOWN=true     # Skip documentation workflows
export VERBOSE=true          # Enable verbose output

# Run with options
./simulate_ci_cd.sh
```

### Command Line Options
```bash
./simulate_ci_cd.sh --help           # Show help
./simulate_ci_cd.sh --skip-docker    # Skip Docker workflows
./simulate_ci_cd.sh --skip-security  # Skip security scans
./simulate_ci_cd.sh --skip-pkgdown   # Skip documentation
./simulate_ci_cd.sh --verbose        # Verbose output
```

## Output Files

After running the simulation, you'll find:

- `coverage-report.html` - Test coverage report
- `docs/` - Generated pkgdown documentation site
- `*.tar.gz` - Built R package (cleaned up automatically)
- `*.Rcheck/` - R CMD check results (cleaned up automatically)

## Troubleshooting

### Common Issues

1. **R dependencies missing**
   ```bash
   R -e "install.packages(c('devtools', 'testthat', 'covr', 'pkgdown'))"
   ```

2. **Docker permission denied**
   ```bash
   sudo usermod -aG docker $USER
   # Then log out and back in
   ```

3. **Package build fails**
   - Check DESCRIPTION file syntax
   - Ensure all R files have valid syntax
   - Verify NAMESPACE is up to date

4. **Tests fail locally but pass in CI**
   - Check for platform-specific code
   - Verify all test dependencies are installed
   - Check for hardcoded paths

### Debugging Tips

1. **Use verbose mode** for detailed output:
   ```bash
   VERBOSE=true ./simulate_ci_cd.sh
   ```

2. **Run components individually**:
   ```bash
   # Just R workflows
   SKIP_DOCKER=true SKIP_SECURITY=true SKIP_PKGDOWN=true ./simulate_ci_cd.sh
   ```

3. **Check specific workflow files**:
   ```bash
   # Validate YAML syntax
   R -e "yaml::read_yaml('.github/workflows/rworkflows.yml')"
   ```

## Integration with Development Workflow

### Pre-commit Testing
Add to your development routine:
```bash
# Before committing
./quick_ci_test.sh

# Before pushing major changes
./simulate_ci_cd.sh
```

### IDE Integration
Many IDEs can run these scripts as build tasks:

**VS Code** (tasks.json):
```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Quick CI Test",
            "type": "shell",
            "command": "./quick_ci_test.sh",
            "group": "test"
        }
    ]
}
```

## Differences from GitHub Actions

### What's Simulated
- ✅ Build processes
- ✅ Test execution
- ✅ Package checks
- ✅ Security scanning
- ✅ Documentation generation

### What's Not Simulated
- ❌ Multi-platform matrix builds (runs on local platform only)
- ❌ GitHub-specific features (secrets, artifacts upload)
- ❌ Deployment steps
- ❌ External service integrations
- ❌ Exact container environments

### Platform Differences
The simulation runs on your local platform, while GitHub Actions runs on:
- Ubuntu 22.04
- macOS 14
- Windows 2022

For platform-specific testing, consider using Docker or virtual machines.

## Contributing

To improve the simulation scripts:

1. Test on your platform
2. Add support for additional workflow features
3. Improve error handling and reporting
4. Add more comprehensive checks

## Related Files

- `CI_CD_improvement_checklist.md` - Planned improvements
- `.github/workflows/` - Actual workflow definitions
- `Dockerfile` & `Dockerfile.dev` - Container definitions
