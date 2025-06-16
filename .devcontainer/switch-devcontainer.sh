#!/bin/bash
# Script to switch between traditional and modern devcontainer configurations

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to display usage
usage() {
    echo "Usage: $0 [modern|traditional|status]"
    echo ""
    echo "Commands:"
    echo "  modern       Switch to modern devcontainer (R 4.5.0 with VS Code + RStudio)"
    echo "  traditional  Switch to traditional devcontainer (R 4.5.0 with Docker Compose)"
    echo "  status       Show current configuration"
    echo ""
    echo "Examples:"
    echo "  $0 modern     # Use modern devcontainer"
    echo "  $0 traditional # Use traditional Docker Compose setup"
    echo "  $0 status     # Check which configuration is active"
    exit 1
}

# Function to check current status
check_status() {
    if [ -f ".devcontainer/devcontainer.json" ]; then
        if grep -q "ghcr.io/rocker-org/devcontainer/tidyverse" ".devcontainer/devcontainer.json" 2>/dev/null; then
            echo -e "${GREEN}Current configuration: Modern DevContainer${NC}"
            echo "- Base: ghcr.io/rocker-org/devcontainer/tidyverse:4.5.0"
            echo "- Features: VS Code + RStudio, radian, httpgd, Python"
        elif grep -q "dockerComposeFile" ".devcontainer/devcontainer.json" 2>/dev/null; then
            echo -e "${BLUE}Current configuration: Traditional DevContainer${NC}"
            echo "- Base: Docker Compose with rocker/rstudio:4.5.0"
            echo "- Features: RStudio Server, Docker-in-Docker"
        else
            echo -e "${YELLOW}Current configuration: Unknown${NC}"
            echo "The devcontainer.json file exists but doesn't match known patterns."
        fi
    else
        echo -e "${YELLOW}No devcontainer.json found${NC}"
        echo "Please run this script from the repository root."
    fi
}

# Function to switch to modern
switch_to_modern() {
    echo -e "${GREEN}Switching to modern devcontainer...${NC}"

    # Backup current if it exists and isn't already a backup
    if [ -f ".devcontainer/devcontainer.json" ] && [ ! -f ".devcontainer/devcontainer-traditional.json" ]; then
        echo "Backing up current configuration..."
        cp ".devcontainer/devcontainer.json" ".devcontainer/devcontainer-traditional.json"
    fi

    # Copy modern to active
    if [ -f ".devcontainer/devcontainer-modern.json" ]; then
        cp ".devcontainer/devcontainer-modern.json" ".devcontainer/devcontainer.json"
        echo -e "${GREEN}✓ Switched to modern devcontainer${NC}"
        echo ""
        echo "Next steps:"
        echo "1. In VS Code: Reopen in Container (Cmd/Ctrl+Shift+P → 'Dev Containers: Reopen in Container')"
        echo "2. Wait for container to build (~5-10 minutes first time)"
        echo "3. Access RStudio at http://localhost:8787 or use VS Code's R extension"
    else
        echo -e "${YELLOW}Error: devcontainer-modern.json not found${NC}"
        echo "Please ensure you're in the repository root directory."
        exit 1
    fi
}

# Function to switch to traditional
switch_to_traditional() {
    echo -e "${BLUE}Switching to traditional devcontainer...${NC}"

    # Backup current if it exists and isn't already a backup
    if [ -f ".devcontainer/devcontainer.json" ] && [ ! -f ".devcontainer/devcontainer-modern-backup.json" ]; then
        echo "Backing up current configuration..."
        cp ".devcontainer/devcontainer.json" ".devcontainer/devcontainer-modern-backup.json"
    fi

    # Restore traditional or use existing
    if [ -f ".devcontainer/devcontainer-traditional.json" ]; then
        cp ".devcontainer/devcontainer-traditional.json" ".devcontainer/devcontainer.json"
        echo -e "${BLUE}✓ Switched to traditional devcontainer${NC}"
    elif [ -f ".devcontainer/devcontainer-old.json" ]; then
        cp ".devcontainer/devcontainer-old.json" ".devcontainer/devcontainer.json"
        echo -e "${BLUE}✓ Switched to traditional devcontainer${NC}"
    else
        echo -e "${YELLOW}Warning: No traditional backup found${NC}"
        echo "The current devcontainer.json might already be the traditional version."
    fi

    echo ""
    echo "Next steps:"
    echo "1. In VS Code: Reopen in Container (Cmd/Ctrl+Shift+P → 'Dev Containers: Reopen in Container')"
    echo "2. Or use: make dev-start for local Docker development"
}

# Main script logic
case "${1:-}" in
    modern)
        switch_to_modern
        ;;
    traditional)
        switch_to_traditional
        ;;
    status)
        check_status
        ;;
    *)
        usage
        ;;
esac
