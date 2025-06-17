# Makefile for hetid R package Docker operations
# Implements 2025 best practices for container management

.PHONY: help build build-dev build-all test check dev-start dev-stop dev-restart clean docker-clean simulation

# Default target
help: ## Show this help message
	@echo "hetid R Package Docker Commands"
	@echo "==============================="
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Examples:"
	@echo "  make dev-start    # Start development environment"
	@echo "  make test         # Run package tests"
	@echo "  make simulation   # Run Monte Carlo simulation"
	@echo ""
	@echo "ARM64/Apple Silicon Users:"
	@echo "  make detect-arch  # Check your architecture"
	@echo "  make dev-start-x86 # Use x86 emulation (40% faster)"

# Build targets
build: ## Build production Docker image
	@echo "Building production image..."
	./dev/docker/scripts/build.sh -t production

build-dev: ## Build development Docker image
	@echo "Building development image..."
	./dev/docker/scripts/build.sh -t development

build-all: ## Build all Docker images
	@echo "Building all images..."
	./dev/docker/scripts/build.sh -t builder
	./dev/docker/scripts/build.sh -t production
	./dev/docker/scripts/build.sh -t development

# Development targets
dev-start: ## Start development environment with RStudio
	@echo "Starting development environment..."
	./dev/docker/scripts/dev.sh start -d
	@echo "RStudio Server available at: http://localhost:8787"

dev-stop: ## Stop development environment
	@echo "Stopping development environment..."
	./dev/docker/scripts/dev.sh stop

dev-restart: ## Restart development environment
	@echo "Restarting development environment..."
	./dev/docker/scripts/dev.sh restart

dev-shell: ## Open shell in development container
	./dev/docker/scripts/dev.sh shell

dev-r: ## Open R console in development container
	./dev/docker/scripts/dev.sh r-console

dev-logs: ## Show development container logs
	./dev/docker/scripts/dev.sh logs -f

# Testing targets
test: ## Run package tests in container
	@echo "Running package tests..."
	./dev/docker/scripts/dev.sh test

check: ## Run R CMD check in container
	@echo "Running R CMD check..."
	./dev/docker/scripts/dev.sh check

test-ci: ## Run tests for CI/CD (builds fresh image)
	@echo "Running CI tests..."
	docker-compose run --rm hetid-test

# Simulation targets
simulation: ## Run main Monte Carlo simulation
	@echo "Running main simulation..."
	./dev/docker/scripts/simulation.sh run -t main -n 1000

simulation-quick: ## Run quick simulation (100 iterations)
	@echo "Running quick simulation..."
	./dev/docker/scripts/simulation.sh run -t main -n 100

simulation-bootstrap: ## Run bootstrap demonstration
	@echo "Running bootstrap demonstration..."
	./dev/docker/scripts/simulation.sh run -t bootstrap -n 100

simulation-sensitivity: ## Run sensitivity analysis
	@echo "Running sensitivity analysis..."
	./dev/docker/scripts/simulation.sh run -t sensitivity -n 500

simulation-status: ## Show running simulations
	./dev/docker/scripts/simulation.sh status

simulation-results: ## Show latest simulation results
	./dev/docker/scripts/simulation.sh results

# Utility targets
status: ## Show status of all containers
	@echo "Development environment status:"
	./dev/docker/scripts/dev.sh status
	@echo ""
	@echo "Simulation status:"
	./dev/docker/scripts/simulation.sh status

logs: ## Show logs from all hetid containers
	@echo "Showing logs from hetid containers..."
	docker logs hetid-development 2>/dev/null || echo "Development container not running"
	docker ps --filter "name=hetid-simulation" -q | xargs -I {} docker logs {} 2>/dev/null || echo "No simulation containers running"

# Cleanup targets
clean: ## Clean up development environment
	@echo "Cleaning up development environment..."
	./dev/docker/scripts/dev.sh clean

docker-clean: ## Clean up all Docker resources (containers, images, volumes)
	@echo "WARNING: This will remove all hetid Docker resources"
	@read -p "Are you sure? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@echo "Cleaning up Docker resources..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml down -v --remove-orphans || true
	docker images | grep hetid | awk '{print $$3}' | xargs -r docker rmi -f
	docker volume ls | grep hetid | awk '{print $$2}' | xargs -r docker volume rm
	@echo "Docker cleanup completed"

# Production targets
deploy-build: ## Build production image with latest tag
	@echo "Building production image for deployment..."
	./dev/docker/scripts/build.sh -t production --tag-latest

deploy-test: ## Test production image
	@echo "Testing production image..."
	docker run --rm hetid:latest R -e "library(hetid); cat('Production image test passed\n')"

# Multi-platform builds (for CI/CD)
build-multiplatform: ## Build multi-platform images
	@echo "Building multi-platform images..."
	./dev/docker/scripts/build.sh -t production -p linux/amd64 --push

# Platform-specific targets for ARM64 users (Apple Silicon, etc.)
dev-start-x86: ## Start dev with x86 emulation (40% faster on ARM64)
	@echo "Starting development environment with x86_64 emulation..."
	@echo "This provides faster builds on Apple Silicon by enabling pak + TinyTeX"
	DOCKER_DEFAULT_PLATFORM=linux/amd64 docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d hetid-dev
	@echo "RStudio Server available at: http://localhost:8787"

build-dev-x86: ## Build dev image with x86 emulation (faster on ARM64)
	@echo "Building development image with x86_64 emulation..."
	docker build --platform linux/amd64 -t hetid:dev-x86 -f Dockerfile.dev .

detect-arch: ## Show current architecture settings
	@echo "Host architecture: $$(uname -m)"
	@echo "Docker default: $$(docker run --rm alpine uname -m)"
	@if [ "$$(uname -m)" = "arm64" ] || [ "$$(uname -m)" = "aarch64" ]; then \
		echo ""; \
		echo "💡 TIP: You're on ARM64. For faster builds, use:"; \
		echo "   make dev-start-x86  (40% faster with pak + TinyTeX support)"; \
	fi

# Development workflow shortcuts
dev: dev-start ## Alias for dev-start

stop: dev-stop ## Alias for dev-stop

restart: dev-restart ## Alias for dev-restart

# Quick development cycle
dev-cycle: ## Complete development cycle (build, test, start)
	@echo "Running complete development cycle..."
	make build-dev
	make test
	make dev-start

# Package management
install-deps: ## Install package dependencies in development container
	@echo "Installing package dependencies..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "devtools::install_deps(dependencies = TRUE)"

update-deps: ## Update package dependencies
	@echo "Updating package dependencies..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "devtools::install_deps(dependencies = TRUE, upgrade = 'always')"

# Documentation
docs: ## Generate package documentation
	@echo "Generating package documentation..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "devtools::document()"

docs-site: ## Build pkgdown site
	@echo "Building pkgdown site..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "pkgdown::build_site()"

docs-local: ## Build all documentation locally (roxygen, vignettes, pkgdown)
	@echo "Building complete local documentation..."
	Rscript build_local_documentation.R

docs-local-pdf: ## Build all documentation locally including PDF manual
	@echo "Building complete local documentation with PDF manual..."
	Rscript build_local_documentation.R --pdf

# Linting and formatting
lint: ## Run lintr on package code
	@echo "Running lintr..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "lintr::lint_package()"

format: ## Format R code with styler
	@echo "Formatting R code..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "styler::style_pkg(transformers = styler::tidyverse_style(line_length = 120L))"

# Information targets
info: ## Show Docker and system information
	@echo "Docker Information:"
	@echo "=================="
	@docker version --format "Docker Engine: {{.Server.Version}}"
	@docker-compose version --short
	@echo ""
	@echo "hetid Images:"
	@docker images | grep hetid || echo "No hetid images found"
	@echo ""
	@echo "hetid Containers:"
	@docker ps -a | grep hetid || echo "No hetid containers found"
	@echo ""
	@echo "hetid Volumes:"
	@docker volume ls | grep hetid || echo "No hetid volumes found"

# Lewbel implementation comparison targets
lewbel-comparison-doc: ## Compile Lewbel implementation comparison LaTeX document
	@echo "Compiling Lewbel implementation comparison document..."
	pdflatex lewbel_implementations_comparison.tex
	pdflatex lewbel_implementations_comparison.tex  # Run twice for references

verify-lewbel: ## Build package comparison vignette (verification included)
	@echo "Building package comparison vignette with verification..."
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml exec hetid-dev R -e "devtools::build_vignettes()"

lewbel-analysis: lewbel-comparison-doc verify-lewbel ## Run full Lewbel comparison analysis
	@echo "Lewbel comparison analysis complete!"
	@echo "PDF: lewbel_implementations_comparison.pdf"
	@echo "Verification: See package-comparison vignette"
