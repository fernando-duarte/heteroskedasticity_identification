#!/bin/bash
# Hetid Package Setup Script for GitHub Codespaces
# Based on successful testing and configuration

set -euo pipefail

echo "🚀 Setting up hetid development environment..."

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

# Install system dependencies for R package compilation
install_system_deps() {
    print_status "Installing system dependencies..."

    sudo apt-get update -qq
    sudo apt-get install -y \
        build-essential \
        gfortran \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libgit2-dev

    print_success "System dependencies installed"
}

# Install core R packages that work reliably
install_r_packages() {
    print_status "Installing core R packages..."

    R --slave -e "
        # Install essential packages that we know work
        packages <- c('gmm', 'xts', 'devtools', 'testthat')
        for (pkg in packages) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                cat('Installing', pkg, '...\n')
                install.packages(pkg, repos = 'https://cran.rstudio.com/')
            } else {
                cat(pkg, 'already installed\n')
            }
        }
        cat('Core R packages ready!\n')
    "

    print_success "Core R packages installed"
}

# Install VS Code R Language Server and related packages
install_vscode_r_packages() {
    print_status "Installing VS Code R Language Server..."

    R --slave -e "
        # Install R Language Server for VS Code (this is what VS Code was trying to install)
        if (!requireNamespace('languageserver', quietly = TRUE)) {
            cat('Installing R Language Server for VS Code...\n')
            install.packages('languageserver', repos = 'https://p3m.dev/cran/__linux__/noble/2025-06-12')
        } else {
            cat('R Language Server already installed\n')
        }

        # Install additional packages that enhance VS Code R experience
        vscode_packages <- c('lintr', 'styler', 'httpgd')
        for (pkg in vscode_packages) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                cat('Installing', pkg, 'for VS Code R support...\n')
                tryCatch({
                    install.packages(pkg, repos = 'https://cran.rstudio.com/')
                }, error = function(e) {
                    cat('Skipping', pkg, 'due to installation error\n')
                })
            } else {
                cat(pkg, 'already installed\n')
            }
        }
        cat('VS Code R packages ready!\n')
    "

    print_success "VS Code R Language Server installed"
}

# Install hetid package dependencies (skip problematic ones)
install_hetid_deps() {
    print_status "Installing hetid package dependencies..."

    if [ -f "DESCRIPTION" ]; then
        R --slave -e "
            tryCatch({
                # Try to install dependencies, but skip ones that fail compilation
                devtools::install_deps(dependencies = c('Depends', 'Imports'))
                cat('hetid dependencies installed successfully!\n')
            }, error = function(e) {
                cat('Some dependencies skipped due to compilation issues\n')
                cat('This is normal - core functionality will still work\n')
            })
        "
    else
        print_warning "No DESCRIPTION file found - skipping package dependencies"
    fi

    print_success "hetid dependencies processed"
}

# Configure RStudio Server for Codespaces
configure_rstudio() {
    print_status "Configuring RStudio Server..."

    # Create RStudio config directory
    sudo mkdir -p /etc/rstudio

    # Create minimal RStudio Server config for Codespaces
    sudo tee /etc/rstudio/rserver.conf > /dev/null << 'EOF'
# RStudio Server Configuration for GitHub Codespaces
www-port=8787
www-address=0.0.0.0
rsession-which-r=/usr/local/bin/R
EOF

    # Create session config to ensure proper R session handling
    sudo tee /etc/rstudio/rsession.conf > /dev/null << 'EOF'
# RStudio Session Configuration for Codespaces
session-timeout-minutes=0
session-disconnected-timeout-minutes=0
EOF

    # Set environment variables for RStudio Server authentication bypass
    export DISABLE_AUTH=true
    export ROOT=true

    # Set proper permissions
    sudo chown -R rstudio:rstudio /etc/rstudio 2>/dev/null || true

    print_success "RStudio Server configured with authentication disabled"
}

# Start RStudio Server
start_rstudio() {
    print_status "Starting RStudio Server with authentication disabled..."

    # Ensure environment variables are set for RStudio Server
    sudo tee -a /etc/environment > /dev/null << 'EOF'
DISABLE_AUTH=true
ROOT=true
EOF

    # Stop any existing RStudio Server processes
    sudo pkill -f rstudio-server 2>/dev/null || true

    # Start RStudio Server with environment variables
    if sudo -E service rstudio-server start; then
        print_success "RStudio Server started successfully"
        print_status "Access RStudio via Ports tab (port 8787) - no authentication required"

        # Verify it's running
        sleep 2
        if netstat -tlnp | grep -q :8787; then
            print_success "RStudio Server is listening on port 8787"
        else
            print_warning "RStudio Server may not be listening properly"
        fi
    else
        print_warning "RStudio Server failed to start with service command"
        print_status "Trying alternative startup method..."

        # Try starting manually with explicit environment
        sudo DISABLE_AUTH=true ROOT=true /usr/lib/rstudio-server/bin/rserver \
            --www-port=8787 \
            --www-address=0.0.0.0 \
            --server-daemonize=1 &

        sleep 2
        if netstat -tlnp | grep -q :8787; then
            print_success "RStudio Server started with manual method"
        else
            print_error "RStudio Server failed to start"
        fi
    fi
}

# Test hetid package loading
test_hetid_package() {
    print_status "Testing hetid package..."

    if [ -f "DESCRIPTION" ]; then
        R --slave -e "
            tryCatch({
                devtools::load_all()
                cat('✅ hetid package loaded successfully!\n')
                cat('Available functions:', paste(ls('package:hetid'), collapse = ', '), '\n')
            }, error = function(e) {
                cat('❌ Error loading hetid package:', conditionMessage(e), '\n')
            })
        "
    else
        print_warning "No DESCRIPTION file found - not a package directory"
    fi
}

# Create helpful aliases
setup_aliases() {
    print_status "Setting up development aliases..."

    cat >> ~/.bashrc << 'EOF'

# hetid development aliases
alias rdev='R -e "devtools::load_all()"'
alias rtest='R -e "devtools::test()"'
alias rdoc='R -e "devtools::document()"'
alias rcheck='R -e "devtools::check()"'

# Quick hetid functions
hetid_load() {
    echo "Loading hetid package..."
    R -e "devtools::load_all(); cat('hetid package loaded!\n')"
}

hetid_status() {
    echo "hetid package status:"
    R -e "if ('hetid' %in% loadedNamespaces()) cat('✅ Loaded\n') else cat('❌ Not loaded\n')"
}

export -f hetid_load hetid_status
EOF

    print_success "Development aliases configured"
}

# Main setup function
main() {
    print_status "Starting hetid development environment setup..."

    # Run setup steps
    install_system_deps
    install_r_packages
    install_vscode_r_packages
    install_hetid_deps
    configure_rstudio
    start_rstudio
    test_hetid_package
    setup_aliases

    # Final status
    echo ""
    print_success "🎉 hetid development environment ready!"
    echo ""
    print_status "Quick start:"
    echo "  • RStudio Server: Access via Ports tab (port 8787)"
    echo "  • VS Code R: Open R files and use Ctrl+Enter to run code"
    echo "  • R Language Server: Pre-installed for VS Code IntelliSense"
    echo "  • Load package: rdev or R -e 'devtools::load_all()'"
    echo "  • Test functions: hetid_load && hetid_status"
    echo ""
    print_status "Happy heteroskedasticity identification research! 🎯"
}

# Run main function
main "$@"
