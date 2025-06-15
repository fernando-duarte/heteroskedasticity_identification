# GitHub Actions Security Improvements (June 2025)

This document outlines the security improvements made to the R Security workflow based on June 2025 best practices and recent supply chain attack incidents.

## Overview of Changes

Following the March 2025 tj-actions/changed-files supply chain attack that compromised over 23,000 repositories, we've implemented several security hardening measures:

1. **Action Pinning**: All GitHub Actions are pinned to specific commit SHAs
2. **OSS Index Authentication**: Configured authentication for oysteR to avoid rate limits
3. **Minimal Permissions**: Workflows use least privilege principle
4. **Robust Package Installation**: Multi-method fallback system for package installation
5. **Graceful Degradation**: Security scans continue even if specific tools fail

## Key Security Improvements

### 1. Action Pinning (SHA-based)

Instead of using mutable tags like `@v4`, actions are pinned to specific commit SHAs:

```yaml
- uses: actions/checkout@11bd71901bbe5b1630ceea73d27597364c9af683 # v4.2.2
```

**Why**: Prevents supply chain attacks where attackers modify action code retroactively.

### 2. OSS Index Authentication for oysteR

The oysteR package requires authentication to avoid rate limiting from Sonatype's OSS Index.

#### Setting up Authentication:

1. **Register at OSS Index**: https://ossindex.sonatype.org/
2. **Get your API credentials** from the User Settings page
3. **Add GitHub Secrets** to your repository:
   - Go to Settings → Secrets and variables → Actions
   - Add `OSSINDEX_USER` (your email address)
   - Add `OSSINDEX_TOKEN` (your API token)

The workflow automatically configures oysteR to use these credentials.

### 3. Minimal Permissions

Workflows follow the principle of least privilege:

```yaml
permissions:
  contents: read
  actions: read
  security-events: write  # Only for jobs that upload SARIF
```

### 4. Robust Package Installation

The improved oysteR installation uses multiple methods:

1. **pak** (primary) - Better handles GitHub rate limits
2. **CRAN** - If available
3. **remotes** with specific commit
4. **Manual download** - Direct tarball installation

If all methods fail, the workflow continues with a warning rather than failing completely.

### 5. Enhanced Caching

Cache keys include R version and DESCRIPTION hash to ensure compatibility:

```yaml
key: security-r-${{ runner.os }}-${{ steps.r-version-string.outputs.version }}-${{ hashFiles('DESCRIPTION') }}-v3
```

## Best Practices Implemented

### Supply Chain Security
- Pin actions to commit SHAs
- Verify action sources
- Use `persist-credentials: false` in checkout
- Regular security audits

### Secrets Management
- Never hardcode credentials
- Use GitHub Secrets for sensitive data
- Implement OIDC where possible (future improvement)
- Rotate credentials regularly

### Workflow Security
- Minimal permissions by default
- Job-specific permission elevation
- Secure handling of pull requests
- Automated security scanning

## Future Improvements

### 1. OIDC Authentication (Recommended)
Instead of static secrets, implement OpenID Connect for cloud providers:
- AWS: Configure GitHub as OIDC provider
- Azure: Use federated credentials
- GCP: Set up Workload Identity Federation

### 2. Dependency Review
Add GitHub's dependency review action for pull requests:
```yaml
- uses: actions/dependency-review-action@v4
```

### 3. SLSA Provenance
Generate Software Bill of Materials (SBOM) and provenance:
```yaml
- uses: slsa-framework/slsa-github-generator@v1.9.0
```

## Monitoring and Alerts

1. **Enable GitHub Security Alerts** for your repository
2. **Review workflow logs** regularly for security warnings
3. **Monitor Dependabot alerts** for vulnerable dependencies
4. **Set up branch protection** rules

## References

- [GitHub Actions Security Best Practices](https://docs.github.com/en/actions/security-guides/security-hardening-for-github-actions)
- [oysteR Documentation](https://sonatype-nexus-community.github.io/oysteR/)
- [r-lib/actions Repository](https://github.com/r-lib/actions)
- [OWASP CI/CD Security Guidance](https://owasp.org/www-project-devsecops-guideline/)

## Incident Response

If you suspect a security incident:
1. Immediately rotate all secrets
2. Review recent workflow runs
3. Check for unauthorized changes
4. Report to security@github.com if appropriate

---

*Last Updated: June 2025*
*Security Contact: [Insert your security contact]* 