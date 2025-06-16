# GitHub Codespaces Configuration for hetid Package

This directory contains the configuration for GitHub Codespaces, enabling cloud-based development of the hetid R package with a fully configured environment.

> **📢 New Option Available!** A modern devcontainer configuration with R 4.5.0 is now available, featuring both VS Code and RStudio in a single environment. See [QUICK_START.md](QUICK_START.md) for the 30-second setup or [README-modern.md](README-modern.md) for full details.

## What's Included

### Development Environment
- **RStudio Server** accessible via web browser
- **R 4.5.0** with all package dependencies pre-installed
- **VS Code** with R extensions and development tools
- **Git** configured for seamless GitHub integration
- **Docker** support for container management

### Configuration Options
- **Traditional**: Docker Compose-based setup (current default)
- **Modern**: Single-image devcontainer with enhanced features (NEW!)
- Switch between them using: `./.devcontainer/switch-devcontainer.sh`

### Pre-configured Tools
- R language support with syntax highlighting and debugging
- Package development tools (devtools, testthat, roxygen2)
- Code quality tools (lintr, styler)
- Documentation tools (pkgdown, rmarkdown)
- GitHub integration (CLI, Copilot, Pull Requests)

## Quick Start

### 1. Launch Codespace
Click the "Code" button on GitHub → "Codespaces" → "Create codespace on main"

### 2. Wait for Setup
The environment will automatically:
- Build the Docker container
- Install R dependencies
- Configure RStudio and VS Code
- Set up helpful aliases and functions

### 3. Access RStudio Server
- Go to the "Ports" tab in VS Code
- Click on port 8787 to open RStudio Server
- No login required - start coding immediately!

## File Structure

```
.devcontainer/
├── devcontainer.json              # Main Codespaces configuration
├── docker-compose.codespaces.yml  # Docker Compose override for Codespaces
├── setup.sh                       # Post-creation setup script
└── README.md                       # This file
```

## Configuration Details

### devcontainer.json
- **Base**: Uses existing Docker setup with Codespaces optimizations
- **Extensions**: Pre-installs R, Git, Docker, and development extensions
- **Ports**: Auto-forwards RStudio Server (8787) and Shiny (3838)
- **Settings**: Optimized VS Code and R configurations
- **Resources**: 4 CPUs, 8GB RAM, 32GB storage

### Docker Integration
- Leverages existing `Dockerfile.dev` and `docker-compose.yml`
- Adds Codespaces-specific overrides
- Persistent volumes for R libraries and RStudio settings
- Optimized for cloud development workflow

### Post-Creation Setup
- Configures Git with GitHub credentials
- Installs package dependencies
- Sets up RStudio preferences
- Creates helpful aliases and functions
- Generates welcome documentation

## Development Workflow

### In VS Code
1. Edit R files with full language support
2. Use integrated terminal for R commands
3. Leverage GitHub integration for version control
4. Access Docker tools for container management

### In RStudio Server
1. Click port 8787 in VS Code Ports tab
2. Full RStudio IDE experience in browser
3. Package pre-loaded and ready for development
4. Integrated plotting and data viewing

### Useful Commands
```bash
# R development shortcuts
rdev          # Load package in development mode
rtest         # Run package tests
rdoc          # Generate documentation
rbuild        # Build package

# Package-specific functions
hetid_demo    # Run package demonstration
hetid_test    # Run comprehensive tests
hetid_sim     # Run quick Monte Carlo simulation

# Git shortcuts
gs            # git status
ga .          # git add all files
gc -m "msg"   # git commit with message
gp            # git push to GitHub
```

## Resource Requirements

### Minimum
- **Machine Type**: 2-core, 4GB RAM
- **Storage**: 16GB
- **Network**: Stable internet connection

### Recommended
- **Machine Type**: 4-core, 8GB RAM (configured by default)
- **Storage**: 32GB (configured by default)
- **Network**: High-speed connection for faster setup

## Troubleshooting

### Common Issues

1. **RStudio not accessible**:
   - Check Ports tab in VS Code
   - Ensure port 8787 is forwarded
   - Wait for container to fully start (check setup.sh logs)

2. **R packages not found**:
   - Run `rdev` to reload package
   - Check setup.sh completed successfully
   - Restart Codespace if needed

3. **Git authentication issues**:
   - Codespaces automatically handles GitHub authentication
   - Use `gh auth status` to check authentication
   - Personal access tokens are handled automatically

### Performance Optimization

1. **Faster startup**:
   - Use prebuild configuration (see GitHub docs)
   - Keep Codespace running between sessions
   - Use persistent volumes for R libraries

2. **Better performance**:
   - Close unused browser tabs
   - Use VS Code instead of RStudio for text editing
   - Monitor resource usage in Codespaces dashboard

## Customization

### Adding Extensions
Edit `devcontainer.json` → `customizations.vscode.extensions`

### Changing Resources
Edit `devcontainer.json` → `hostRequirements`

### Additional Setup
Modify `setup.sh` for custom post-creation commands

### Environment Variables
Add to `docker-compose.codespaces.yml` → `environment`

## Security Considerations

- Codespaces runs in isolated containers
- GitHub handles authentication automatically
- Secrets are managed through GitHub Secrets
- No sensitive data stored in container images
- Regular security updates through base images

## Cost Optimization

- **Stop Codespaces** when not in use
- **Use smaller machine types** for light development
- **Enable auto-suspend** for inactive Codespaces
- **Delete unused Codespaces** regularly

## Support

For Codespaces-specific issues:
1. Check GitHub Codespaces documentation
2. Review setup.sh logs in terminal
3. Open issue in repository with "codespaces" label

For package development issues:
- See main [README.md](../README.md)
- Check [CONTRIBUTING.md](../CONTRIBUTING.md)
- Review [DOCKER.md](../DOCKER.md)
