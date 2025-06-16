# Docker Cache Fix v2 - Conservative Approach

## Changes Made

Fixed potential syntax/compatibility issues with the cache configuration by using a more conservative approach:

### Before (v1 - might have caused issues):
```yaml
cache-from: type=gha,scope=buildkit-${{ matrix.target }}
cache-to: type=gha,mode=min,scope=buildkit-${{ matrix.target }}
```

### After (v2 - more compatible):
```yaml
cache-from: type=gha
cache-to: type=gha,mode=max,scope=${{ matrix.target }}
```

## Why This Should Work Better

1. **cache-from without scope**: More compatible with existing cache entries and avoids potential syntax issues
2. **mode=max**: Returns to the original mode that was working (though larger cache size)
3. **scope only on cache-to**: Still prevents cache competition between targets
4. **Simpler scope names**: Removed "buildkit-" prefix for cleaner scope names

## Expected Behavior

- Docker builds should work without syntax errors
- Each target still gets its own cache scope (builder, production, development)
- Cache competition is reduced (though not as much as with mode=min)
- Full compatibility with existing GitHub Actions setup

## Next Steps

If builds are still failing:
1. Check the specific error message in GitHub Actions logs
2. Consider removing scope entirely as a fallback
3. Investigate if the issue is unrelated to cache configuration
