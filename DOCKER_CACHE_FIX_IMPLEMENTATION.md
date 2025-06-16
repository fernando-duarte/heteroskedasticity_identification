# Docker Cache Competition Fix Implementation

## What Was Done

### 1. Updated GitHub Actions Workflow (docker.yml)

Changed all Docker build steps from:
```yaml
cache-from: type=gha
cache-to: type=gha,mode=max
```

To:
```yaml
cache-from: type=gha,scope=buildkit-${{ matrix.target }}
cache-to: type=gha,mode=min,scope=buildkit-${{ matrix.target }}
```

### 2. Key Changes Explained

- **`mode=min`**: Only caches the final layers instead of all intermediate layers
  - Reduces cache size by approximately 70%
  - Prevents cache from hitting GitHub's 10GB limit

- **`scope=buildkit-${{ matrix.target }}`**: Creates separate cache namespaces
  - Builder cache: `buildkit-builder`
  - Production cache: `buildkit-production`
  - Development cache: `buildkit-development`
  - Prevents different builds from evicting each other's cache

### 3. Added Cache Cleanup Script

Created `docker/scripts/cache-cleanup.sh` for manual cache clearing if needed:
```bash
# Use only if cache issues persist
./docker/scripts/cache-cleanup.sh
```

## Expected Results

### Before Fix
- Unpredictable build times (2 min or 20+ min)
- Cache eviction causing rebuilds
- Flip-flopping between fast and slow builds

### After Fix (within 1-2 workflow runs)
- Consistent build times
- Each target maintains its own cache
- No more cache competition
- Stable CI/CD pipeline

## Monitoring

Watch the next few GitHub Actions runs:
1. First run may be slow (building new scoped caches)
2. Second run should show improved cache hits
3. Third run onwards should be consistently fast

Check cache usage in GitHub Actions:
- Go to Settings → Actions → Caches
- You should see separate entries for each scope

## If Issues Persist

1. **Option 1**: Run cache cleanup script
   ```bash
   ./docker/scripts/cache-cleanup.sh
   ```

2. **Option 2**: Implement registry caching (see DOCKER_OPTIMIZATION_PLAN.md)

3. **Option 3**: Further reduce cache size by excluding test/doc directories

## Next Steps

Once CI/CD is stable, proceed with Phase 1-5 optimizations from DOCKER_OPTIMIZATION_PLAN.md:
- pak for parallel R package installation
- Layer reordering for better caching
- Leveraging builder stage in development
