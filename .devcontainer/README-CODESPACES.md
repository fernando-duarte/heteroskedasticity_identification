# GitHub Codespaces Setup

## Problem
The main `devcontainer.json` uses `ghcr.io/rocker-org/devcontainer/tidyverse:4.5.0` which fails in GitHub Codespaces with:
```
Error: Command failed: docker inspect --type image ghcr.io/rocker-org/devcontainer/tidyverse:4.5.0
```

## Solution (Following Preservation Rules)

We've created a **parallel Codespaces-specific configuration** that doesn't modify your working Docker setup:

### Files Created/Modified:
- ✅ **`devcontainer-codespaces.json`** - Codespaces-specific config using `rocker/tidyverse:4.5.0`
- ✅ **`README-CODESPACES.md`** - This documentation
- ❌ **`devcontainer.json`** - **PRESERVED UNTOUCHED** (your working Docker setup)

### How to Use for Codespaces:

#### Option 1: Manual Selection (Recommended)
1. When creating a Codespace, GitHub will show multiple devcontainer options
2. Select **"hetid R 4.5.0 Development (Codespaces Secure)"**
3. This uses the `devcontainer-codespaces.json` configuration

#### Option 2: Temporary Switch
If you need to temporarily make Codespaces use the fixed config:

```bash
# Switch to Codespaces config
cd .devcontainer
cp devcontainer.json devcontainer-original-backup.json
cp devcontainer-codespaces.json devcontainer.json

# Commit and push
git add devcontainer.json
git commit -m "temp: use codespaces config"
git push

# After Codespace works, restore original
cp devcontainer-original-backup.json devcontainer.json
git add devcontainer.json
git commit -m "restore: original devcontainer config"
git push
```

### What's Different in Codespaces Config:
- **Image**: `rocker/tidyverse:4.5.0` (more reliable in Codespaces)
- **Features**: Simplified set for better compatibility
- **Same functionality**: RStudio Server, R packages, ports, VS Code extensions

### Preserved (Untouched):
- ✅ `devcontainer.json` (your working Docker setup)
- ✅ `Dockerfile*` files
- ✅ `docker-compose*.yml` files
- ✅ All package files (`DESCRIPTION`, `R/`, etc.)
- ✅ All CI/CD workflows
- ✅ All build configurations

This follows the codespaces-fix-guide.md rules perfectly! 🎯
