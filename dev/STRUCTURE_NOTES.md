# Development Directory Structure Notes

## Why `dev/` Directory?

Following R package best practices (June 2025), development-only scripts and tools are organized in the `dev/` directory:

1. **Clear Separation**: Distinguishes development tools from package code
2. **Single Exclusion**: Only need `^dev$` in `.Rbuildignore` instead of multiple entries
3. **Modern Convention**: Follows current R community practices
4. **Future-proof**: Avoids conflicts with R's official `tools/` directory purpose

## Directory Organization

```
dev/
├── README.md              # Overview of development tools
├── STRUCTURE_NOTES.md     # This file - explains the organization
├── docker/                # All Docker-related development tools
│   ├── scripts/          # Docker utility scripts
│   └── *.md             # Docker documentation
├── internal-docs/        # Internal documentation not for distribution
├── quick_ci_test.sh     # Quick CI/CD testing
├── simulate_ci_cd.sh    # Full CI/CD simulation
└── test_workflows.sh    # Workflow testing with 'act'
```

## Key Points

1. **Everything in `dev/` is excluded from the R package** via `.Rbuildignore`
2. **User-facing scripts go in `inst/scripts/`** and are installed with the package
3. **Makefile references updated** to use `dev/docker/scripts/` paths
4. **No impact on package functionality** - purely organizational

## Migration from Root Directory

On 2025-06-17, moved the following from root to `dev/`:
- `quick_ci_test.sh`
- `simulate_ci_cd.sh`
- `test_workflows.sh`
- `docker/` (entire directory)

This cleanup improves repository organization while maintaining all functionality through updated paths in the Makefile.
