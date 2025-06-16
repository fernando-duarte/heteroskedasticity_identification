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
    # qpdf for R CMD check
    qpdf \
    # wget for downloading TinyTeX
    wget \
    && rm -rf /var/lib/apt/lists/*

# Copy pandoc from minimal image instead of installing via apt
# This saves ~200MB compared to apt-get install pandoc
COPY --from=pandoc/minimal:latest /usr/local/bin/pandoc /usr/local/bin/pandoc

# Install pak for fast parallel package installation (skip on ARM64 due to compatibility issues)
# Use BuildKit cache mount for R package downloads
RUN --mount=type=cache,target=/root/.cache/R,sharing=locked \
    R -e "options(repos = 'https://cloud.r-project.org/'); \
          .libPaths(c('/usr/local/lib/R/site-library', .libPaths())); \
          arch <- Sys.info()[['machine']]; \
          if (arch != 'aarch64' && arch != 'arm64') { \
            tryCatch({ \
              install.packages('pak', repos = 'https://r-lib.github.io/p/pak/stable/', \
                             lib = '/usr/local/lib/R/site-library'); \
              library(pak); \
              cat('pak installed successfully\n'); \
            }, error = function(e) { \
              cat('pak installation failed, will use traditional install.packages\n'); \
            }); \
          } else { \
            cat('Skipping pak on ARM64 architecture, will use traditional install.packages\n'); \
          }"

# Install package management tools
RUN --mount=type=cache,target=/root/.cache/R,sharing=locked \
    R -e ".libPaths(c('/usr/local/lib/R/site-library', .libPaths())); \
          arch <- Sys.info()[['machine']]; \
          if (arch != 'aarch64' && arch != 'arm64' && requireNamespace('pak', quietly = TRUE)) { \
            tryCatch({ \
              pak::pkg_install(c('remotes', 'devtools', 'knitr', 'rmarkdown', 'testthat', 'tinytex'), \
                             lib = '/usr/local/lib/R/site-library'); \
            }, error = function(e) { \
              install.packages(c('remotes', 'devtools', 'knitr', 'rmarkdown', 'testthat', 'tinytex'), \
                             lib = '/usr/local/lib/R/site-library'); \
            }); \
          } else { \
            install.packages(c('remotes', 'devtools', 'knitr', 'rmarkdown', 'testthat', 'tinytex'), \
                           lib = '/usr/local/lib/R/site-library'); \
          }"

# Install TinyTeX for LaTeX support (much smaller than texlive)
# This replaces ~3GB of texlive packages with ~150MB TinyTeX
# CACHING BEHAVIOR:
# - TinyTeX installation is cached as a Docker layer (one-time cost: ~20-30 min on ARM64, ~10 min on x86)
# - Subsequent builds with no changes: 0 seconds (uses cached layer)
# - GitHub Actions cache via 'cache-from: type=gha' ensures persistence across workflow runs
# - PDF generation during R CMD check: ~30-60 seconds (not cached, runs each time)
RUN R -e "tinytex::install_tinytex(force = TRUE, dir = '/opt/TinyTeX', extra_packages = c('inconsolata', 'times', 'tex-gyre', 'fancyhdr', 'natbib', 'caption'))" && \
    /opt/TinyTeX/bin/*/tlmgr path add

# Ensure TinyTeX is in PATH for all subsequent operations
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"

# Copy package metadata files first for better layer caching
COPY DESCRIPTION NAMESPACE ./

# Install all package dependencies
# Note: nloptr requires libnlopt-dev which was already installed in system dependencies
RUN --mount=type=cache,target=/root/.cache/R,sharing=locked \
    R -e ".libPaths(c('/usr/local/lib/R/site-library', .libPaths())); \
          arch <- Sys.info()[['machine']]; \
          if (arch != 'aarch64' && arch != 'arm64' && requireNamespace('pak', quietly = TRUE)) { \
            tryCatch({ \
              pak::pkg_install(c('nloptr', 'minqa', 'RcppEigen', 'lme4', 'pbkrtest', 'car', 'AER', \
                               'boot', 'dplyr', 'furrr', 'future', 'ggplot2', 'purrr', 'rlang', 'tidyr'), \
                             lib = '/usr/local/lib/R/site-library'); \
            }, error = function(e) { \
              install.packages(c('nloptr', 'minqa', 'RcppEigen', 'lme4', 'pbkrtest', 'car', 'AER', \
                               'boot', 'dplyr', 'furrr', 'future', 'ggplot2', 'purrr', 'rlang', 'tidyr'), \
                             lib = '/usr/local/lib/R/site-library'); \
            }); \
          } else { \
            install.packages(c('nloptr', 'minqa', 'RcppEigen', 'lme4', 'pbkrtest', 'car', 'AER', \
                             'boot', 'dplyr', 'furrr', 'future', 'ggplot2', 'purrr', 'rlang', 'tidyr'), \
                           lib = '/usr/local/lib/R/site-library'); \
          }; \
          if (!requireNamespace('testthat', quietly = TRUE)) stop('testthat installation failed')"

# Install package dependencies
# Install both runtime and test dependencies (including Suggests)
RUN --mount=type=cache,target=/root/.cache/R,sharing=locked \
    R -e ".libPaths(c('/usr/local/lib/R/site-library', .libPaths())); \
          arch <- Sys.info()[['machine']]; \
          if (arch != 'aarch64' && arch != 'arm64' && requireNamespace('pak', quietly = TRUE)) { \
            tryCatch({ \
              pak::local_install_deps(dependencies = TRUE); \
            }, error = function(e) { \
              remotes::install_deps(dependencies = TRUE); \
            }); \
          } else { \
            remotes::install_deps(dependencies = TRUE); \
          }"

# Install security scanning tools (oysteR) for vulnerability checks
RUN R -e ".libPaths(c('/usr/local/lib/R/site-library', .libPaths())); \
          if (!requireNamespace('oysteR', quietly = TRUE)) { \
            arch <- Sys.info()[['machine']]; \
            if (arch != 'aarch64' && arch != 'arm64' && requireNamespace('pak', quietly = TRUE)) { \
              tryCatch({ \
                pak::pkg_install('oysteR', lib = '/usr/local/lib/R/site-library'); \
              }, error = function(e) { \
                install.packages('oysteR', lib = '/usr/local/lib/R/site-library'); \
              }); \
            } else { \
              install.packages('oysteR', lib = '/usr/local/lib/R/site-library'); \
            } \
          }; \
          library(oysteR); \
          audit_results <- audit_installed_r_pkgs(verbose = FALSE); \
          if (nrow(get_vulnerabilities(audit_results)) > 0) { \
            print(get_vulnerabilities(audit_results)); \
            warning('Security vulnerabilities found in dependencies!'); \
          }"

# Copy source code and build package
COPY . .

# Build and install the package
# Build with --no-build-vignettes to avoid vignette building issues in Docker
RUN R CMD build . --no-build-vignettes && \
    R CMD INSTALL *.tar.gz --with-keep.source && \
    # Verify installation
    R -e "library(hetid); packageVersion('hetid')" && \
    # Strip build tools to reduce image size (saves ~250MB)
    # Keep essential dev libraries that R packages might need at runtime
    apt-get remove -y build-essential gfortran && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

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

# Copy TinyTeX installation from builder
COPY --from=builder /opt/TinyTeX /opt/TinyTeX

# Copy pandoc from builder (was copied from minimal container)
COPY --from=builder /usr/local/bin/pandoc /usr/local/bin/pandoc

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
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"

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
