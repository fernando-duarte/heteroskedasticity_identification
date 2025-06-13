#!/bin/bash
# Commit and push Docker implementation

git commit -m "feat: Add comprehensive Docker and GitHub Codespaces setup with 2025 best practices

- Implement multi-stage Docker builds with R 4.5.0
- Add production, development, and testing environments
- Include RStudio Server for interactive development
- Add GitHub Codespaces configuration for zero-setup cloud development
- Create management scripts for Docker operations (build, dev, simulation)
- Add Makefile with 30+ commands for common operations
- Implement CI/CD pipeline with security scanning
- Add comprehensive documentation (DOCKER.md, Codespaces README)
- Optimize for Monte Carlo simulations with parallel processing
- Include security hardening with non-root users
- Support multi-platform builds (AMD64/ARM64)
- Add automated testing and quality assurance workflows

Features:
- One-command setup: make dev-start
- RStudio Server at http://localhost:8787
- Automated dependency installation
- Persistent volumes for development
- Resource allocation controls for simulations
- GitHub integration and authentication
- VS Code extensions for R development
- Helpful aliases and shortcuts

This enables instant development environment setup either locally with Docker
or in the cloud with GitHub Codespaces, following all 2025 containerization
best practices for security, performance, and developer experience."

echo "Commit completed. Pushing to origin..."
git push origin main
echo "Push completed successfully!"
