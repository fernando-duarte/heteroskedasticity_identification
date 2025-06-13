# Docker Setup for hetid Package

This document provides comprehensive instructions for using Docker with the `hetid` R package, implementing 2025 best practices for containerization.

## Overview

The Docker setup provides multiple environments:
- **Production**: Minimal runtime for package execution
- **Development**: Full RStudio Server environment for interactive development
- **Testing**: Automated testing and CI/CD
- **Simulation**: Optimized for Monte Carlo simulations

## Quick Start

### 1. Development Environment (Recommended for new users)

Start RStudio Server for interactive development:

```bash
# Start development environment
./docker/scripts/dev.sh start

# Access RStudio Server at http://localhost:8787
# Username: rstudio (no password required in dev mode)
```

### 2. Production Usage

Build and run the production image:

```bash
# Build production image
./docker/scripts/build.sh

# Run package
docker run --rm hetid:latest
```

### 3. Run Simulations

Execute Monte Carlo simulations:

```bash
# Run main simulation
./docker/scripts/simulation.sh run

# Run with custom parameters
./docker/scripts/simulation.sh run -n 500 -c 8 -m 16g
```

## Docker Images

### Production Image (`hetid:latest`)
- **Base**: `rocker/r-ver:4.5.0`
- **Size**: ~800MB (optimized)
- **Purpose**: Package execution and simulations
- **Security**: Non-root user, minimal attack surface

### Development Image (`hetid:dev`)
- **Base**: `rocker/rstudio:4.5.0`
- **Size**: ~2GB (includes development tools)
- **Purpose**: Interactive development with RStudio Server
- **Features**: Full development toolchain, debugging tools

## System Requirements

### Minimum Requirements
- Docker Engine 28.0+ (2025 version)
- 4GB RAM
- 2 CPU cores
- 10GB disk space

### Recommended for Simulations
- 8GB+ RAM
- 4+ CPU cores
- 20GB+ disk space
- SSD storage for better I/O performance

## Detailed Usage

### Development Workflow

1. **Start Development Environment**:
   ```bash
   ./docker/scripts/dev.sh start -d  # Start in background
   ```

2. **Access RStudio Server**:
   - Open http://localhost:8787 in your browser
   - Username: `rstudio` (no password in dev mode)

3. **Development Commands**:
   ```bash
   ./docker/scripts/dev.sh test     # Run package tests
   ./docker/scripts/dev.sh check    # Run R CMD check
   ./docker/scripts/dev.sh shell    # Open bash shell
   ./docker/scripts/dev.sh r-console # Open R console
   ```

4. **Stop Environment**:
   ```bash
   ./docker/scripts/dev.sh stop
   ```

### Production Deployment

1. **Build Production Image**:
   ```bash
   ./docker/scripts/build.sh -t production --tag-latest
   ```

2. **Run Package Functions**:
   ```bash
   docker run --rm hetid:latest R -e "library(hetid); run_lewbel_demo()"
   ```

3. **Mount Data Volumes**:
   ```bash
   docker run --rm \
     -v $(pwd)/data:/app/data:ro \
     -v $(pwd)/results:/app/results \
     hetid:latest R -e "library(hetid); # your R code here"
   ```

### Simulation Management

1. **Run Different Simulation Types**:
   ```bash
   # Main simulation
   ./docker/scripts/simulation.sh run -t main -n 1000

   # Bootstrap demonstration
   ./docker/scripts/simulation.sh run -t bootstrap -n 100

   # Sensitivity analysis
   ./docker/scripts/simulation.sh run -t sensitivity -n 500

   # Sample size analysis
   ./docker/scripts/simulation.sh run -t sample -n 200
   ```

2. **Monitor Simulations**:
   ```bash
   ./docker/scripts/simulation.sh status  # Check running simulations
   ./docker/scripts/simulation.sh logs    # Follow logs
   ```

3. **View Results**:
   ```bash
   ./docker/scripts/simulation.sh results
   ```

### Docker Compose Usage

For complex workflows, use Docker Compose:

```bash
# Start all services
docker-compose up -d

# Development with live reload
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up

# Run specific service
docker-compose run hetid-test

# Scale simulation workers
docker-compose up --scale hetid-simulation=3
```

## Configuration

### Environment Variables

- `R_LIBS_USER`: R library path
- `OMP_NUM_THREADS`: OpenMP thread count
- `OPENBLAS_NUM_THREADS`: BLAS thread count
- `DISABLE_AUTH`: Disable RStudio authentication (dev only)

### Volume Mounts

- `/app/data`: Input data (read-only)
- `/app/results`: Output results
- `/app/simulation-results`: Simulation outputs
- `/home/rstudio/hetid`: Source code (development)

### Resource Limits

Configure in `docker-compose.yml`:

```yaml
deploy:
  resources:
    limits:
      cpus: '4.0'
      memory: 8G
    reservations:
      cpus: '2.0'
      memory: 4G
```

## Troubleshooting

### Common Issues

1. **Port 8787 already in use**:
   ```bash
   # Check what's using the port
   lsof -i :8787

   # Use different port
   docker run -p 8788:8787 hetid:dev
   ```

2. **Out of memory during simulations**:
   ```bash
   # Increase memory limit
   ./docker/scripts/simulation.sh run -m 16g

   # Reduce simulation count
   ./docker/scripts/simulation.sh run -n 500
   ```

3. **Permission issues with volumes**:
   ```bash
   # Fix ownership
   sudo chown -R $(id -u):$(id -g) ./results
   ```

### Performance Optimization

1. **Enable BuildKit caching**:
   ```bash
   export DOCKER_BUILDKIT=1
   ```

2. **Use multi-stage builds**:
   ```bash
   ./docker/scripts/build.sh -t production --cache-from hetid:builder
   ```

3. **Optimize for your platform**:
   ```bash
   ./docker/scripts/build.sh -p linux/arm64  # For Apple Silicon
   ```

## Security Considerations

- All images run as non-root users in production
- Development mode disables authentication for convenience
- Secrets should be mounted as read-only volumes
- Use specific version tags, avoid `latest` in production

## Integration with CI/CD

Example GitHub Actions workflow:

```yaml
- name: Build and test Docker image
  run: |
    ./docker/scripts/build.sh -t builder
    docker run --rm hetid:builder R -e "devtools::test()"
```

## Support

For issues related to Docker setup:
1. Check the troubleshooting section above
2. Review container logs: `docker logs <container-name>`
3. Open an issue on the GitHub repository

For R package issues, see the main README.md file.
