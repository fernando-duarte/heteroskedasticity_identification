#!/bin/bash
# Simulation runner script for hetid package
# Manages Monte Carlo simulations in Docker containers

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

# Default values
SIMULATION_TYPE="main"
NUM_SIMULATIONS=1000
CPUS=4
MEMORY="8g"
OUTPUT_DIR="./simulation-results"

# Function to show usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS] COMMAND

Run Monte Carlo simulations for hetid package in Docker

COMMANDS:
    run             Run simulation
    status          Show running simulations
    stop            Stop running simulations
    logs            Show simulation logs
    results         Show latest results
    clean           Clean up simulation containers

OPTIONS:
    -t, --type TYPE         Simulation type (main|bootstrap|sensitivity|sample) [default: main]
    -n, --num NUM          Number of simulations [default: 1000]
    -c, --cpus CPUS        CPU limit [default: 4]
    -m, --memory MEM       Memory limit [default: 8g]
    -o, --output DIR       Output directory [default: ./simulation-results]
    -h, --help             Show this help message

EXAMPLES:
    $0 run                                    # Run main simulation with defaults
    $0 run -t sensitivity -n 500             # Run sensitivity analysis
    $0 run -c 8 -m 16g                       # Run with more resources
    $0 status                                 # Check running simulations
    $0 results                                # Show latest results

EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--type)
            SIMULATION_TYPE="$2"
            shift 2
            ;;
        -n|--num)
            NUM_SIMULATIONS="$2"
            shift 2
            ;;
        -c|--cpus)
            CPUS="$2"
            shift 2
            ;;
        -m|--memory)
            MEMORY="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        run|status|stop|logs|results|clean)
            COMMAND="$1"
            shift
            break
            ;;
        *)
            print_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Check if command was provided
if [[ -z "${COMMAND:-}" ]]; then
    print_error "No command specified"
    usage
    exit 1
fi

# Validate simulation type
case $SIMULATION_TYPE in
    main|bootstrap|sensitivity|sample)
        ;;
    *)
        print_error "Invalid simulation type: $SIMULATION_TYPE"
        print_error "Valid types: main, bootstrap, sensitivity, sample"
        exit 1
        ;;
esac

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Generate R script based on simulation type
generate_r_script() {
    local script_file="$OUTPUT_DIR/simulation_script.R"

    cat > "$script_file" << EOF
# Generated simulation script for hetid package
# Type: $SIMULATION_TYPE
# Simulations: $NUM_SIMULATIONS

library(hetid)

# Set up parallel processing
library(future)
plan(multisession, workers = $CPUS)

# Create configuration
config <- create_default_config(num_simulations = $NUM_SIMULATIONS)

# Generate timestamp for results
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

cat("Starting $SIMULATION_TYPE simulation with $NUM_SIMULATIONS runs...\n")
cat("Timestamp:", timestamp, "\n")

# Run simulation based on type
results <- switch("$SIMULATION_TYPE",
    "main" = {
        cat("Running main Monte Carlo simulation...\n")
        seeds <- generate_all_seeds(config)
        run_main_simulation(config, seeds)
    },
    "bootstrap" = {
        cat("Running bootstrap demonstration...\n")
        run_bootstrap_demonstration(config)
    },
    "sensitivity" = {
        cat("Running sensitivity analysis...\n")
        run_sensitivity_analysis(config)
    },
    "sample" = {
        cat("Running sample size analysis...\n")
        run_sample_size_analysis(config)
    }
)

# Save results
output_file <- paste0("/app/simulation-results/", "$SIMULATION_TYPE", "_", timestamp, ".rds")
saveRDS(results, output_file)

# Save summary
summary_file <- paste0("/app/simulation-results/", "$SIMULATION_TYPE", "_", timestamp, "_summary.txt")
sink(summary_file)
cat("Simulation Summary\n")
cat("==================\n")
cat("Type:", "$SIMULATION_TYPE", "\n")
cat("Simulations:", $NUM_SIMULATIONS, "\n")
cat("Timestamp:", timestamp, "\n")
cat("CPUs:", $CPUS, "\n")
cat("Memory:", "$MEMORY", "\n")
cat("\nResults:\n")
if (is.data.frame(results)) {
    cat("Rows:", nrow(results), "\n")
    cat("Columns:", ncol(results), "\n")
    if ("estimator" %in% names(results)) {
        cat("Estimators:", paste(unique(results\$estimator), collapse = ", "), "\n")
    }
} else if (is.list(results)) {
    cat("List elements:", length(results), "\n")
    cat("Names:", paste(names(results), collapse = ", "), "\n")
}
sink()

cat("Simulation completed successfully!\n")
cat("Results saved to:", output_file, "\n")
cat("Summary saved to:", summary_file, "\n")
EOF

    echo "$script_file"
}

# Run simulation
run_simulation() {
    print_status "Preparing $SIMULATION_TYPE simulation..."

    # Generate R script
    local script_file
    script_file=$(generate_r_script)

    print_status "Generated simulation script: $script_file"
    print_status "Running simulation with $NUM_SIMULATIONS iterations..."
    print_status "Resources: $CPUS CPUs, $MEMORY memory"

    # Run simulation container
    docker run --rm \
        --name "hetid-simulation-$(date +%s)" \
        --cpus="$CPUS" \
        --memory="$MEMORY" \
        -v "$(pwd)/$OUTPUT_DIR:/app/simulation-results" \
        -v "$script_file:/app/simulation_script.R" \
        hetid:latest \
        R --slave -f /app/simulation_script.R

    print_success "Simulation completed!"
    print_status "Results available in: $OUTPUT_DIR"
}

# Show simulation status
show_status() {
    print_status "Running hetid simulations:"
    docker ps --filter "name=hetid-simulation" --format "table {{.Names}}\t{{.Status}}\t{{.CreatedAt}}"
}

# Stop simulations
stop_simulations() {
    print_status "Stopping running simulations..."
    docker ps --filter "name=hetid-simulation" -q | xargs -r docker stop
    print_success "All simulations stopped"
}

# Show logs
show_logs() {
    local container_name
    container_name=$(docker ps --filter "name=hetid-simulation" --format "{{.Names}}" | head -1)

    if [[ -n "$container_name" ]]; then
        print_status "Showing logs for: $container_name"
        docker logs -f "$container_name"
    else
        print_warning "No running simulations found"
    fi
}

# Show results
show_results() {
    print_status "Latest simulation results in $OUTPUT_DIR:"
    if [[ -d "$OUTPUT_DIR" ]]; then
        ls -la "$OUTPUT_DIR" | tail -10

        # Show latest summary if available
        local latest_summary
        latest_summary=$(find "$OUTPUT_DIR" -name "*_summary.txt" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)

        if [[ -n "$latest_summary" ]]; then
            echo
            print_status "Latest summary ($latest_summary):"
            cat "$latest_summary"
        fi
    else
        print_warning "Output directory $OUTPUT_DIR does not exist"
    fi
}

# Clean up
clean_simulations() {
    print_warning "This will remove all simulation containers and results"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        print_status "Cleaning up simulation containers..."
        docker ps -a --filter "name=hetid-simulation" -q | xargs -r docker rm -f
        print_success "Cleanup completed"
    else
        print_status "Cleanup cancelled"
    fi
}

# Execute command
case "$COMMAND" in
    run)
        run_simulation
        ;;
    status)
        show_status
        ;;
    stop)
        stop_simulations
        ;;
    logs)
        show_logs
        ;;
    results)
        show_results
        ;;
    clean)
        clean_simulations
        ;;
    *)
        print_error "Unknown command: $COMMAND"
        usage
        exit 1
        ;;
esac
