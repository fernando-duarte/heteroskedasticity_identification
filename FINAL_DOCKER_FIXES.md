# Final Docker and R Security Workflow Fixes

## Issues Addressed

### 1. Docker Tag Extraction Bug ✅
**Problem**: The workflow was trying to read tag files that might not be created or downloaded properly.

**Solution**:
- Added fallback mechanism: if tag file doesn't exist, extract tag from `docker images` output
- Added verification steps after artifact downloads to debug issues
- Made all tag reading operations check for file existence first
- Added diagnostic output to help troubleshoot artifact issues

**Code changes**:
```bash
# Before
IMAGE_TAG=$(cat /tmp/builder-tag.txt)

# After
if [ -f /tmp/builder-tag.txt ]; then
  IMAGE_TAG=$(cat /tmp/builder-tag.txt)
  echo "Using image tag from artifact: ${IMAGE_TAG}"
else
  # Fallback: get the tag from loaded images
  IMAGE_TAG=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep -v "<none>" | head -n 1)
  echo "Using image tag from docker images: ${IMAGE_TAG}"
fi
```

### 2. Missing Package Verification ✅
**Problem**: The r-security workflow used `pak::` and `remotes::` without properly loading the packages.

**Solution**:
- Added `library()` calls after `requireNamespace()` checks
- Added fallback to install `remotes` if neither `pak` nor `remotes` are available
- Improved error handling with nested try-catch blocks

**Code changes**:
```r
# Before
if (requireNamespace('pak', quietly = TRUE)) {
  pak::pkg_install('sonatype-nexus-community/oysteR', upgrade = FALSE)
}

# After
if (requireNamespace('pak', quietly = TRUE)) {
  library(pak)
  pak::pkg_install('sonatype-nexus-community/oysteR', upgrade = FALSE)
} else if (requireNamespace('remotes', quietly = TRUE)) {
  library(remotes)
  remotes::install_github('sonatype-nexus-community/oysteR', upgrade = 'never')
} else {
  # Try to install remotes first, then use it
  tryCatch({
    install.packages('remotes')
    library(remotes)
    remotes::install_github('sonatype-nexus-community/oysteR', upgrade = 'never')
  }, error = function(e) {
    # Last resort: try CRAN
    install.packages('oysteR')
  })
}
```

## Additional Improvements

1. **Better Debugging**: Added verification steps that show:
   - Contents of /tmp directory after artifact downloads
   - Whether tag files exist and their contents
   - Which method was used to determine the Docker image tag

2. **Robustness**: All Docker image operations now have fallbacks
   - Primary: Use tag from artifact file
   - Fallback: Extract tag from loaded Docker images

3. **Error Prevention**: File existence checks prevent "file not found" errors

## Expected Outcome

These fixes should resolve the remaining Docker build and test issues by:
- Ensuring Docker workflows can complete even if artifacts aren't perfectly transferred
- Properly loading R packages before using them in the security workflow
- Providing clear diagnostic output to debug any remaining issues

The workflows are now more robust and should handle edge cases gracefully.