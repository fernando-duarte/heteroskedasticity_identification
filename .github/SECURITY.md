# Security Policy

## Supported Versions

We release patches for security vulnerabilities for the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |
| < 0.1   | :x:                |

## Reporting a Vulnerability

If you discover a security vulnerability in the hetid package, please report it to us privately.

**DO NOT** create a public GitHub issue for security vulnerabilities.

Instead, please email security concerns to: fernando_duarte@brown.edu

Include the following information:
- Type of vulnerability
- Full paths of source file(s) related to the vulnerability
- Location of the affected source code (tag/branch/commit or direct URL)
- Any special configuration required to reproduce the issue
- Step-by-step instructions to reproduce the issue
- Proof-of-concept or exploit code (if possible)
- Impact of the issue

We will acknowledge receipt within 48 hours and provide a detailed response within 5 business days.

## Security Measures

This project implements several security measures:

### 1. Dependency Scanning
- **oysteR**: Scans R package dependencies for known vulnerabilities
- **Weekly automated scans**: GitHub Actions workflow runs security checks every Monday
- **PR checks**: All pull requests are scanned for security issues

### 2. GitHub Actions Security
- **SHA Pinning**: All GitHub Actions use commit SHA pinning to prevent supply chain attacks
- **Minimal Permissions**: Workflows use least-privilege permissions
- **No Credential Persistence**: `persist-credentials: false` on all checkouts

### 3. Docker Security
- **Multi-stage builds**: Minimal production images
- **Non-root user**: Production containers run as non-root user
- **Trivy scanning**: Automated vulnerability scanning for Docker images and configurations

### 4. Code Security
- **Security linting**: Custom linters check for dangerous functions (system(), eval(), etc.)
- **License compatibility**: Automated checks for license conflicts
- **SBOM generation**: Software Bill of Materials for supply chain transparency

## Security Best Practices for Contributors

1. **Never commit secrets**: Use GitHub Secrets for sensitive data
2. **Pin dependencies**: Use exact versions in DESCRIPTION
3. **Review dependencies**: Check new dependencies with oysteR before adding
4. **Use system2()**: Never use system() or shell() for system calls
5. **Validate inputs**: Always validate user inputs, especially in functions that interact with the file system

## Vulnerability Disclosure Timeline

- **Day 0**: Security report received
- **Day 1-2**: Acknowledge receipt
- **Day 3-5**: Initial assessment and response
- **Day 6-30**: Develop and test fix
- **Day 31**: Public disclosure (coordinated with reporter)

## Security Updates

Security updates are released as patch versions (e.g., 0.1.1, 0.1.2) and announced via:
- GitHub Security Advisories
- Package NEWS file
- Repository releases page

## Resources

- [R Package Security Best Practices](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Security-considerations)
- [GitHub Security Features](https://docs.github.com/en/code-security)
- [oysteR Package](https://github.com/sonatype-nexus-community/oysteR)
