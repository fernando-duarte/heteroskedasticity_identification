# Understanding Pre-commit's R Environment Isolation

## The Multi-Layer Problem

### Layer 1: Package Bug (FIXED ✓)
The digest package had a bug in version 0.6.36:
```c
// Incorrect syntax in raes.c
ctx = (aes_context*)Calloc(sizeof(*ctx), char);
                                         ^^^^ wrong: literal 'char'
```
This was fixed in version 0.6.37 (August 2024).

### Layer 2: System Configuration (FIXED ✓)
Your macOS system now has:
- digest 0.6.37 installed globally
- `~/.R/Makevars` with compiler flags to suppress warnings
- Everything works when you run R normally

### Layer 3: Pre-commit Isolation (STILL BROKEN ✗)
Pre-commit creates its own environment that:
- Downloads its own copy of packages
- Uses its own `renv.lock` file
- Ignores your global R packages
- Ignores your `~/.R/Makevars`

## Why Isolation Exists

Pre-commit uses isolation to ensure:

1. **Reproducibility**: Same hooks work for all team members
2. **Version Control**: Specific versions locked for consistency
3. **No Conflicts**: Won't break your other R projects
4. **Self-Contained**: Works without admin/root access

## The Timeline Problem

```
2023: lorenzwalthert/precommit creates hooks
      └── renv.lock specifies digest 0.6.36

2024: digest 0.6.37 fixes the bug
      └── Your system gets the fix

2025: You try to use pre-commit
      └── Still downloads old digest 0.6.36 from lock file
```

## What Happens When You Run Pre-commit

```bash
pre-commit run style-files --all-files
```

1. Pre-commit reads `.pre-commit-config.yaml`
2. Finds repo: `lorenzwalthert/precommit`
3. Clones that repository
4. Finds `renv.lock` in the cloned repo
5. Creates isolated R environment
6. Installs packages from `renv.lock`:
   ```json
   "digest": {
     "Package": "digest",
     "Version": "0.6.36",  // ← Old buggy version!
     "Source": "Repository"
   }
   ```
7. Compilation fails on macOS

## Why Your Fixes Don't Work

### Fix 1: Installing digest 0.6.37 globally
- ✓ Works for your regular R sessions
- ✗ Pre-commit doesn't use global packages

### Fix 2: Setting `~/.R/Makevars`
- ✓ Works for your regular R compilations
- ✗ Pre-commit's renv might not respect this

### Fix 3: Using Homebrew GCC
- ✓ Could work if pre-commit used it
- ✗ Pre-commit uses system default compiler

## Potential Solutions (Not Yet Implemented)

### 1. Update lorenzwalthert/precommit
They need to update their `renv.lock` to use digest 0.6.37+

### 2. Override renv behavior
Force renv to use newer package versions (complex)

### 3. Fork and Fix
Create your own fork with updated dependencies

### 4. Wait for Ecosystem Update
- Pre-commit hooks update their dependencies
- renv improves macOS handling
- R packages become more compiler-agnostic

## Current Best Practice

Use the simplified configuration without R-specific hooks until:
1. The pre-commit R ecosystem updates
2. Someone creates macOS-specific R hooks
3. R/renv handles compiler differences better

## Technical Details

### What renv.lock Controls

```json
{
  "R": {
    "Version": "4.3.0",
    "Repositories": [{
      "Name": "CRAN",
      "URL": "https://cloud.r-project.org"
    }]
  },
  "Packages": {
    "digest": {
      "Package": "digest",
      "Version": "0.6.36",  // Frozen at old version
      "Source": "Repository",
      "Repository": "CRAN",
      "Hash": "..."
    },
    // ... other packages
  }
}
```

### The Compilation Environment

```
Pre-commit Environment:
├── Uses: /usr/bin/clang (Xcode)
├── Ignores: ~/.R/Makevars
├── Downloads: Source packages from CRAN
└── Compiles: In isolated directory
    └── No access to:
        - Your fixes
        - Global packages
        - Custom compilers
```

## Summary

The digest package IS fixed, but pre-commit creates a time capsule environment that recreates the old problem. It's like having a modern car but being forced to use 1950s gasoline - the car is fine, but the fuel makes it break down.
