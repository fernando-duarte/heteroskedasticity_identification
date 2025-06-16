# Building hetid Package Documentation Locally

This guide explains how to build all documentation for the hetid R package on your local machine.

## Quick Start

To build all documentation at once:

```bash
# Using the Makefile
make docs-local

# Or directly with R
Rscript build_local_documentation.R
```

This will:
1. Update roxygen2 documentation (man/ files)
2. Build package vignettes
3. Generate the pkgdown website
4. Create a documentation index page

## Documentation Components

### 1. Function Documentation (man/)
Generated from roxygen2 comments in R source files:
```r
devtools::document()
```

### 2. Vignettes
The package includes three vignettes:
- **Getting Started**: Introduction to the hetid package
- **Package Comparison**: Comparing Lewbel (2012) implementations
- **Degrees of Freedom**: Understanding degrees of freedom adjustments

Build vignettes with:
```r
devtools::build_vignettes()
```

### 3. pkgdown Website
A complete website with all documentation:
```r
pkgdown::build_site()
```

### 4. PDF Manual (Optional)
Generate a PDF reference manual:
```bash
make docs-local-pdf
# Or
Rscript build_local_documentation.R --pdf
```

Note: PDF generation requires LaTeX. Install with:
```r
tinytex::install_tinytex()
```

## Viewing Documentation

After building, documentation is available at:

- **Quick Access Page**: `documentation_index.html`
- **Full Website**: `docs/index.html`
- **Built Vignettes**: `doc/*.html`
- **PDF Manual**: `hetid_*.pdf` (if built)

Open the quick access page:
```bash
# On macOS
open documentation_index.html

# On Linux
xdg-open documentation_index.html

# On Windows
start documentation_index.html
```

## Directory Structure

```
hetid/
├── man/                  # Function documentation (roxygen2)
├── vignettes/           # Vignette source files (.Rmd)
├── doc/                 # Built vignettes (.html)
├── docs/                # pkgdown website
│   ├── reference/       # Function reference
│   ├── articles/        # Vignettes as articles
│   └── index.html       # Main page
├── inst/doc/            # Installed vignettes
└── documentation_index.html  # Quick access page
```

## Prerequisites

Required R packages:
- devtools
- roxygen2
- pkgdown
- knitr
- rmarkdown
- tinytex (for PDF manual)

Install with:
```r
install.packages(c("devtools", "roxygen2", "pkgdown", "knitr", "rmarkdown"))
```

## Troubleshooting

### Missing packages
The build script will automatically install missing packages.

### Vignette build errors
Ensure all package dependencies are installed:
```r
devtools::install_deps(dependencies = TRUE)
```

### PDF manual errors
Install LaTeX:
```r
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}
```

### Permission errors
On Unix systems, make the script executable:
```bash
chmod +x build_local_documentation.R
```

## Development Workflow

1. **Edit R files**: Add/modify roxygen2 comments
2. **Run documentation build**: `make docs-local`
3. **Check results**: Open `documentation_index.html`
4. **Iterate**: Make changes and rebuild as needed

## Continuous Documentation

For automatic documentation updates during development:
```r
# In R console
devtools::load_all()  # Load package
devtools::document()  # Update docs after changes
```

## Build Logs

Each documentation build creates a timestamped log file:
- `documentation_build_YYYYMMDD_HHMMSS.log`

Check logs for any warnings or errors during the build process.
