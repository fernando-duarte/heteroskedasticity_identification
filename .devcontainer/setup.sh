#!/bin/bash
# Post-creation setup script for GitHub Codespaces
# Configures the development environment after container creation

set -euo pipefail

echo "🚀 Setting up hetid development environment in Codespaces..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Configure Git if not already configured
configure_git() {
    print_status "Configuring Git..."

    if [ -n "${GITHUB_USER:-}" ]; then
        git config --global user.name "$GITHUB_USER"
        print_status "Set Git user.name to: $GITHUB_USER"
    fi

    if [ -n "${GITHUB_USER:-}" ]; then
        git config --global user.email "$GITHUB_USER@users.noreply.github.com"
        print_status "Set Git user.email to: $GITHUB_USER@users.noreply.github.com"
    fi

    # Configure Git for better Codespaces experience
    git config --global init.defaultBranch main
    git config --global pull.rebase false
    git config --global core.autocrlf input
    git config --global core.editor "code --wait"

    print_success "Git configuration completed"
}

# Install package dependencies
install_dependencies() {
    print_status "Installing R package dependencies..."

    # Install package in development mode
    R --slave -e "
        if (!requireNamespace('devtools', quietly = TRUE)) {
            install.packages('devtools', repos = 'https://cloud.r-project.org/')
        }
        devtools::install_deps(dependencies = TRUE)
        devtools::load_all()
        cat('Package dependencies installed successfully\n')
    "

    print_success "R package dependencies installed"
}

# Set up RStudio preferences for Codespaces
setup_rstudio() {
    print_status "Configuring RStudio for Codespaces..."

    # Create RStudio preferences directory
    mkdir -p /home/rstudio/.rstudio/monitored/user-settings

    # Configure RStudio user preferences
    cat > /home/rstudio/.rstudio/monitored/user-settings/user-settings << 'EOF'
{
    "posix_terminal_shell": "bash",
    "initial_working_directory": "/home/rstudio/hetid",
    "default_project_location": "/home/rstudio/hetid",
    "restore_last_project": true,
    "save_workspace": "never",
    "load_workspace": false,
    "always_save_history": false,
    "remove_history_duplicates": true,
    "show_line_numbers": true,
    "highlight_selected_line": true,
    "highlight_selected_word": true,
    "show_margin": true,
    "margin_column": 80,
    "insert_matching": true,
    "soft_wrap_r_files": false,
    "syntax_color_console": true,
    "highlight_r_function_calls": true,
    "rainbow_parentheses": true,
    "auto_append_newline": true,
    "strip_trailing_whitespace": true,
    "focus_console_after_exec": false,
    "fold_style": "begin_and_end",
    "code_completion": "always",
    "show_help_tooltip_on_idle": true,
    "show_diagnostics_r": true,
    "style_diagnostics": true,
    "check_arguments_to_r_function_calls": true,
    "warn_if_no_such_variable_in_scope": true,
    "warn_if_variable_defined_but_not_used": true,
    "auto_discover_package_dependencies": true,
    "navigate_to_build_error": true,
    "packages_pane_enabled": true,
    "use_rcpp_template": true,
    "restore_source_documents": false,
    "source_with_echo": true,
    "export_viewer_as_static": false,
    "rmd_viewer_type": "pane",
    "shiny_viewer_type": "pane",
    "plumber_viewer_type": "pane"
}
EOF

    # Set proper ownership
    chown -R rstudio:rstudio /home/rstudio/.rstudio

    print_success "RStudio configured for optimal Codespaces experience"
}

# Create helpful aliases and functions
setup_aliases() {
    print_status "Setting up helpful aliases..."

    cat >> /home/rstudio/.bashrc << 'EOF'

# hetid package development aliases
alias ll='ls -la'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'

# R development shortcuts
alias rdev='R --slave -e "devtools::load_all()"'
alias rtest='R --slave -e "devtools::test()"'
alias rcheck='R CMD check --as-cran *.tar.gz'
alias rbuild='R CMD build .'
alias rdoc='R --slave -e "devtools::document()"'

# Docker shortcuts
alias dps='docker ps'
alias dimg='docker images'
alias dlogs='docker logs'

# Git shortcuts
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline'

# hetid specific functions
hetid_demo() {
    echo "Running hetid demo..."
    R --slave -e "library(hetid); run_lewbel_demo()"
}

hetid_test() {
    echo "Running hetid tests..."
    R --slave -e "devtools::test()"
}

hetid_sim() {
    echo "Running quick simulation..."
    R --slave -e "
        library(hetid)
        config <- create_default_config(num_simulations = 100)
        results <- run_lewbel_monte_carlo(config)
        cat('Simulation completed!\n')
    "
}

export -f hetid_demo hetid_test hetid_sim
EOF

    print_success "Aliases and functions configured"
}

# Create welcome message
create_welcome() {
    print_status "Creating welcome message..."

    cat > /home/rstudio/CODESPACES_WELCOME.md << 'EOF'
# Welcome to hetid Development in GitHub Codespaces! 🎉

## Quick Start

### RStudio Server
- **Access**: Click on the "Ports" tab and open port 8787
- **Login**: No authentication required in Codespaces
- **Workspace**: Pre-loaded with the hetid package

### Useful Commands

```bash
# R development
rdev          # Load package in development mode
rtest         # Run package tests
rdoc          # Generate documentation
rbuild        # Build package

# Quick functions
hetid_demo    # Run package demo
hetid_test    # Run tests
hetid_sim     # Run quick simulation

# Git workflow
gs            # git status
ga .          # git add all
gc -m "msg"   # git commit with message
gp            # git push
```

### Package Development Workflow

1. **Edit code** in VS Code or RStudio
2. **Test changes**: `rtest` or `hetid_test`
3. **Run simulations**: `hetid_sim`
4. **Build documentation**: `rdoc`
5. **Commit changes**: `ga . && gc -m "description"`

### Accessing Services

- **RStudio Server**: Port 8787 (auto-forwarded)
- **Shiny Apps**: Port 3838 (if needed)
- **Web Services**: Port 8080 (if needed)

### Resources

- [Package Documentation](./README.md)
- [Docker Guide](./DOCKER.md)
- [Contributing Guidelines](./CONTRIBUTING.md)

Happy coding! 🚀
EOF

    print_success "Welcome message created"
}

# Main setup execution
main() {
    print_status "Starting Codespaces setup for hetid package..."

    # Wait for services to be ready
    print_status "Waiting for services to be ready..."
    sleep 10

    # Run setup steps
    configure_git
    install_dependencies
    setup_rstudio
    setup_aliases
    create_welcome

    # Final message
    echo ""
    print_success "🎉 Codespaces setup completed successfully!"
    echo ""
    print_status "Next steps:"
    echo "  1. Open the Ports tab and click on port 8787 to access RStudio"
    echo "  2. Read CODESPACES_WELCOME.md for quick start guide"
    echo "  3. Run 'hetid_demo' to test the package"
    echo ""
    print_status "Happy coding! 🚀"
}

# Run main function
main "$@"
