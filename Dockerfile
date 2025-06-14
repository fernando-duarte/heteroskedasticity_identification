# syntax=docker/dockerfile:1.7
# Production Dockerfile for hetid R package
# Implements 2025 Docker best practices with multi-stage builds
# Optimized for heteroskedasticity identification simulations

#==============================================================================
# Build Stage - Install dependencies and build package
#==============================================================================
FROM rocker/r-ver:4.5.0 AS builder

# Metadata labels following OCI standards
LABEL org.opencontainers.image.title="hetid R Package Builder"
LABEL org.opencontainers.image.description="Build stage for heteroskedasticity identification package"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.source="https://github.com/fernando-duarte/heteroskedasticity_identification"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.authors="Fernando Duarte <duarte@alum.mit.edu>"
LABEL org.opencontainers.image.created="2025-06-01T00:00:00Z"

# Set working directory
WORKDIR /build

# Install system dependencies with cache mounts for faster builds
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    apt-get update && apt-get install -y \
    # Core development tools
    build-essential \
    git \
    gfortran \
    # R package compilation dependencies
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Statistical computing dependencies
    libgsl-dev \
    liblapack-dev \
    libblas-dev \
    # Additional dependencies for nloptr and lme4
    libnlopt-dev \
    libgmp-dev \
    libmpfr-dev \
    # Parallel processing support
    libopenmpi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install remotes, devtools, and knitr for package management
RUN --mount=type=cache,target=/usr/local/lib/R/site-library \
    R -e "install.packages(c('remotes', 'devtools', 'knitr', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# Copy package metadata files first for better layer caching
COPY DESCRIPTION NAMESPACE ./

# Install core dependencies first (in order to handle dependency chains)
RUN R -e "install.packages(c('nloptr', 'minqa', 'RcppEigen'), repos='https://cloud.r-project.org/', type='source')" && \
    R -e "install.packages(c('lme4'), repos='https://cloud.r-project.org/')" && \
    R -e "install.packages(c('pbkrtest'), repos='https://cloud.r-project.org/')" && \
    R -e "install.packages(c('car'), repos='https://cloud.r-project.org/')" && \
    R -e "install.packages(c('AER'), repos='https://cloud.r-project.org/')"

# Install remaining package dependencies
RUN R -e "install.packages(c('boot', 'dplyr', 'furrr', 'future', 'ggplot2', 'purrr', 'rlang', 'tidyr', 'testthat'), repos='https://cloud.r-project.org/')"

# Install package dependencies using remotes (ensure remotes is available)
RUN R -e "if (!require('remotes', quietly = TRUE)) install.packages('remotes', repos='https://cloud.r-project.org/'); remotes::install_deps('.', dependencies = TRUE, repos='https://cloud.r-project.org/')"

# Copy source code and build package
COPY . .

# Build and install the package (ensure knitr is available)
RUN R -e "if (!require('knitr', quietly = TRUE)) install.packages('knitr', repos='https://cloud.r-project.org/')" && \
    R CMD build . && \
    R CMD INSTALL *.tar.gz && \
    # Verify installation
    R -e "library(hetid); packageVersion('hetid')"

#==============================================================================
# Production Stage - Minimal runtime image
#==============================================================================
FROM rocker/r-ver:4.5.0 AS production

# Production metadata
LABEL org.opencontainers.image.title="hetid R Package"
LABEL org.opencontainers.image.description="Heteroskedasticity identification package implementing Lewbel (2012)"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.source="https://github.com/fernando-duarte/heteroskedasticity_identification"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.authors="Fernando Duarte <duarte@alum.mit.edu>"
LABEL org.opencontainers.image.created="2025-06-01T00:00:00Z"
LABEL org.opencontainers.image.documentation="https://fernando-duarte.github.io/heteroskedasticity_identification/"

# Install minimal runtime dependencies only
RUN apt-get update && apt-get install -y \
    # Essential runtime libraries
    libcurl4 \
    libssl3 \
    libxml2 \
    # Graphics libraries for ggplot2
    libfontconfig1 \
    libfreetype6 \
    libpng16-16 \
    # Mathematical libraries
    libgsl27 \
    liblapack3 \
    libblas3 \
    # Runtime libraries for nloptr and lme4
    libnlopt0 \
    libgmp10 \
    libmpfr6 \
    # Clean up
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security (2025 best practice)
RUN groupadd -r hetid && useradd -r -g hetid -m -s /bin/bash hetid

# Copy installed R packages from builder
COPY --from=builder /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/

# Set working directory and ownership
WORKDIR /app
RUN chown -R hetid:hetid /app

# Switch to non-root user
USER hetid

# Health check to verify R and package availability
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD R --slave -e "library(hetid); cat('OK')" || exit 1

# Set environment variables for optimal performance
ENV R_LIBS_USER=/usr/local/lib/R/site-library
ENV OMP_NUM_THREADS=1
ENV OPENBLAS_NUM_THREADS=1

# Default command
CMD ["R", "--slave", "-e", "library(hetid); cat('hetid package loaded successfully\\n')"]

#==============================================================================
# Development Stage - Full development environment
#==============================================================================
FROM rocker/rstudio:4.5.0 AS development

# Development metadata
LABEL org.opencontainers.image.title="hetid R Package Development"
LABEL org.opencontainers.image.description="Development environment for heteroskedasticity identification package"
LABEL org.opencontainers.image.version="0.1.0-dev"

# Install additional development tools and system dependencies
RUN apt-get update && apt-get install -y \
    # Development tools
    vim \
    nano \
    htop \
    curl \
    wget \
    # Git for version control
    git \
    # Additional utilities
    less \
    tree \
    # System libraries needed for R packages
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install development R packages
RUN install2.r --error --skipinstalled \
    devtools \
    testthat \
    roxygen2 \
    pkgdown \
    covr \
    lintr \
    styler \
    rmarkdown \
    knitr

# Copy package source for development
WORKDIR /home/rstudio/hetid
COPY --chown=rstudio:rstudio . .

# Install package in development mode
RUN R -e "devtools::install(dependencies = TRUE)"

# Expose RStudio port
EXPOSE 8787

# Set development environment variables
ENV DISABLE_AUTH=true
ENV ROOT=true

# Default command for development
CMD ["/init"]
