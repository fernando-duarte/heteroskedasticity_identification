# Testing the Modern DevContainer

Follow these steps to test the new modern devcontainer configuration.

## 1. Switch to Modern Configuration

```bash
# From repository root
./.devcontainer/switch-devcontainer.sh modern
```

Expected output:
- "Switching to modern devcontainer..."
- "✓ Switched to modern devcontainer"

## 2. Open in VS Code

1. Open VS Code in the repository directory
2. You should see a notification: "Folder contains a Dev Container configuration"
3. Click "Reopen in Container" (or press Cmd/Ctrl+Shift+P → "Dev Containers: Reopen in Container")
4. First build will take 5-10 minutes, subsequent builds ~30 seconds

## 3. Verify Container Started

Once the container is running, check:

### In VS Code Terminal:
```bash
# Check R version
R --version
# Should show: R version 4.5.0

# Check if radian is available
which radian
# Should show: /usr/local/bin/radian

# Check Python (for pre-commit)
python --version
# Should show: Python 3.11.x

# Check if pre-commit is installed
pre-commit --version
```

### In VS Code:
- The status bar should show "Dev Container: hetid R 4.5.0 Development"
- The Ports panel should show:
  - 8787 (RStudio Server)
  - 8080 (httpgd)

## 4. Test RStudio Server

1. Go to the Ports panel in VS Code
2. Click the globe icon next to port 8787
3. RStudio should open in your browser
4. No login required (auth is disabled)

In RStudio, verify:
```r
# Check R version
R.version.string
# [1] "R version 4.5.0 (2025-04-11)"

# Check if hetid dependencies are available
library(AER)
library(boot)
library(furrr)
library(ggplot2)

# All should load without errors
```

## 5. Test VS Code R Extension

In VS Code:

1. Open an R file (e.g., `R/data-generation.R`)
2. You should see:
   - Syntax highlighting
   - Code completion (IntelliSense)
   - Function signatures on hover

3. Test running code:
   - Select a line of code
   - Press Shift+Enter
   - Code should execute in R terminal

## 6. Test Radian Console

In VS Code terminal:
```bash
# Start radian
radian

# You should see enhanced R console with:
# - Syntax highlighting
# - Multi-line editing
# - Better autocomplete
```

Test some R code:
```r
# Create a simple plot
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Should open in httpgd browser window
```

## 7. Test httpgd Graphics

In R (either console):
```r
# Check if httpgd is available
httpgd::hgd()

# Create a plot
plot(1:10)

# Should see message about httpgd server
# Check port 8080 in Ports panel
```

## 8. Test Package Development

```bash
# In terminal
cd /home/rstudio/workspace

# Run package tests
make test

# Run R CMD check
make check

# Generate documentation
make docs
```

## 9. Test Pre-commit Hooks

```bash
# Check if pre-commit is installed
pre-commit run --all-files

# Should run all hooks and pass
```

## 10. Test Persistence

1. Create a test file:
```bash
echo "test" > ~/test-persistence.txt
```

2. In VS Code: Cmd/Ctrl+Shift+P → "Dev Containers: Rebuild Container"
3. After rebuild, check if file persists:
```bash
cat ~/test-persistence.txt
# Should still exist due to volume mounts
```

## 11. Performance Checks

```r
# Test package installation speed
system.time({
  install.packages("jsonlite")
})
# Should be fast due to caching

# Test parallel processing
library(furrr)
plan(multisession, workers = 4)
system.time({
  future_map(1:8, ~ Sys.sleep(1))
})
# Should take ~2 seconds (parallel)
```

## 12. Check All Features

✅ VS Code with R extension working
✅ RStudio Server accessible
✅ Radian console available
✅ httpgd graphics working
✅ Python/pre-commit installed
✅ All hetid dependencies available
✅ Package tests pass
✅ Git integration working
✅ Persistence across rebuilds

## Troubleshooting

### Container fails to start
- Check Docker is running: `docker --version`
- Check for port conflicts: `lsof -ti:8787`
- View logs: View → Output → Dev Containers

### RStudio not loading
- Wait 30 seconds after container starts
- Try: `sudo rstudio-server restart` in terminal
- Check: `sudo rstudio-server status`

### Graphics not showing
- Ensure port 8080 is forwarded
- Try: `httpgd::hgd_browse()` in R

### Extensions not working
- Reload window: Cmd/Ctrl+Shift+P → "Developer: Reload Window"
- Check extensions are installed in container

## Report Results

After testing, note any issues:
- [ ] Container build time: _____ minutes
- [ ] All features working as expected
- [ ] Any error messages: _____
- [ ] Performance observations: _____

## Next Steps

If everything works:
1. Try your normal development workflow
2. Run some hetid simulations
3. Test on different projects

To switch back to traditional:
```bash
./.devcontainer/switch-devcontainer.sh traditional
```
