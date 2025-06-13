# Local R Hooks Solution for macOS

## Summary

This solution bypasses the renv isolation issues that cause R package compilation failures on macOS by using local hooks that run with your system R installation.

## How It Works

1. **No renv isolation**: Uses `language: system` instead of the R environment
2. **Your R packages**: Uses the digest 0.6.37+ and other packages from your system
3. **Your compiler settings**: Respects your `~/.R/Makevars` configuration
4. **Same functionality**: Implements all the same R-specific hooks

## Setup

1. Ensure required R packages are installed:
   ```r
   install.packages(c("styler", "lintr", "roxygen2", "desc", "spelling"))
   ```

2. Switch to the local R hooks configuration:
   ```bash
   bash inst/scripts/use-local-r-hooks.sh
   ```

3. Run the hooks:
   ```bash
   pre-commit run --all-files
   ```

## Configuration Files

- `.pre-commit-config-local-r.yaml` - The local hooks configuration
- `inst/hooks/` - Individual R scripts for each hook
- `.lintr` - Linting configuration
- `inst/WORDLIST` - Custom dictionary for spell checking

## Advantages

✅ No compilation errors  
✅ Uses your working R setup  
✅ Faster (no package installation)  
✅ Easier to debug  

## Disadvantages

❌ Less reproducible across team members  
❌ Requires local R package installation  
❌ Not as isolated from your environment  

## Hook Scripts

Each hook is a simple R script in `inst/hooks/`:
- `style-files.R` - Format code with styler
- `lintr.R` - Lint code quality issues
- `parsable-R.R` - Check R syntax
- `roxygenize.R` - Update documentation
- `use-tidy-description.R` - Format DESCRIPTION
- `spell-check.R` - Check spelling

## Switching Back

To revert to the standard configuration:
```bash
cp .pre-commit-config.backup.yaml .pre-commit-config.yaml
```

Or to use the simplified configuration without R hooks:
```bash
cp .pre-commit-config.yaml.bak .pre-commit-config.yaml
```

## Future Improvements

When the upstream lorenzwalthert/precommit repository updates to digest 0.6.37+, you can switch back to the standard configuration for better reproducibility across team members. 