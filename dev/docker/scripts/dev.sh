#!/bin/bash
# Development environment management script for hetid package
# Provides easy commands for development workflow

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
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

# Function to show usage
usage() {
    cat << EOF
Usage: $0 COMMAND [OPTIONS]

Development environment management for hetid package

COMMANDS:
    start           Start development environment with RStudio
    stop            Stop development environment
    restart         Restart development environment
    logs            Show logs from development container
    shell           Open bash shell in development container
    r-console       Open R console in development container
    test            Run package tests
    check           Run R CMD check
    build           Build development image
    clean           Clean up containers and volumes
    status          Show status of development containers

OPTIONS:
    -d, --detach    Run in detached mode (for start command)
    -f, --follow    Follow logs (for logs command)
    -h, --help      Show this help message

EXAMPLES:
    $0 start                    # Start RStudio development environment
    $0 start -d                 # Start in background
    $0 test                     # Run package tests
    $0 shell                    # Open shell in container
    $0 logs -f                  # Follow container logs

EOF
}

# Check if docker-compose is available
check_docker_compose() {
    if ! command -v docker-compose &> /dev/null; then
        print_error "docker-compose is not installed or not in PATH"
        exit 1
    fi
}

# Start development environment
start_dev() {
    local detach=""
    if [[ "${1:-}" == "-d" || "${1:-}" == "--detach" ]]; then
        detach="-d"
    fi

    print_status "Starting hetid development environment..."

    # Build if image doesn't exist
    if ! docker images | grep -q "hetid.*dev"; then
        print_status "Development image not found, building..."
        docker-compose -f docker-compose.yml -f docker-compose.dev.yml build hetid-dev
    fi

    docker-compose -f docker-compose.yml -f docker-compose.dev.yml up $detach hetid-dev

    if [[ -n "$detach" ]]; then
        print_success "Development environment started in background"
        print_status "RStudio Server available at: http://localhost:8787"
        print_status "Use '$0 logs -f' to follow logs"
        print_status "Use '$0 stop' to stop the environment"
    fi
}

# Stop development environment
stop_dev() {
    print_status "Stopping hetid development environment..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml down
    print_success "Development environment stopped"
}

# Restart development environment
restart_dev() {
    print_status "Restarting hetid development environment..."
    stop_dev
    start_dev -d
}

# Show logs
show_logs() {
    local follow=""
    if [[ "${1:-}" == "-f" || "${1:-}" == "--follow" ]]; then
        follow="-f"
    fi

    docker-compose -f docker-compose.yml -f docker-compose.dev.yml logs $follow hetid-dev
}

# Open shell in development container
open_shell() {
    print_status "Opening bash shell in development container..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev bash
}

# Open R console
open_r_console() {
    print_status "Opening R console in development container..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R
}

# Run tests
run_tests() {
    print_status "Running package tests..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "devtools::test()"
}

# Run R CMD check
run_check() {
    print_status "Running R CMD check..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev bash -c "
        R CMD build . &&
        R CMD check --as-cran *.tar.gz
    "
}

# Build development image
build_dev() {
    print_status "Building development image..."
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml build hetid-dev
    print_success "Development image built successfully"
}

# Clean up
clean_dev() {
    print_warning "This will remove all hetid containers and volumes"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        print_status "Cleaning up hetid containers and volumes..."
        docker-compose -f docker-compose.yml -f docker-compose.dev.yml down -v --remove-orphans
        docker system prune -f --filter label=org.opencontainers.image.title="hetid*"
        print_success "Cleanup completed"
    else
        print_status "Cleanup cancelled"
    fi
}

# Show status
show_status() {
    print_status "hetid development environment status:"
    echo
    docker-compose -f docker-compose.yml -f docker-compose.dev.yml ps
    echo
    print_status "hetid Docker images:"
    docker images | grep hetid || echo "No hetid images found"
}

# Main command processing
check_docker_compose

case "${1:-}" in
    start)
        start_dev "${2:-}"
        ;;
    stop)
        stop_dev
        ;;
    restart)
        restart_dev
        ;;
    logs)
        show_logs "${2:-}"
        ;;
    shell)
        open_shell
        ;;
    r-console)
        open_r_console
        ;;
    test)
        run_tests
        ;;
    check)
        run_check
        ;;
    build)
        build_dev
        ;;
    clean)
        clean_dev
        ;;
    status)
        show_status
        ;;
    -h|--help|help)
        usage
        exit 0
        ;;
    "")
        print_error "No command specified"
        usage
        exit 1
        ;;
    *)
        print_error "Unknown command: $1"
        usage
        exit 1
        ;;
esac
