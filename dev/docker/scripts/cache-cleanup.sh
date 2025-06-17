#!/bin/bash
# Script to clean up GitHub Actions cache for hetid Docker builds
# Use this if cache issues persist after implementing the optimization

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
    print_error "GitHub CLI (gh) is not installed"
    print_status "Install it from: https://cli.github.com/"
    exit 1
fi

# Check if authenticated
if ! gh auth status &> /dev/null; then
    print_error "Not authenticated with GitHub"
    print_status "Run: gh auth login"
    exit 1
fi

print_warning "This will delete ALL GitHub Actions caches for the hetid repository"
print_warning "This action cannot be undone!"
read -p "Are you sure you want to continue? (y/N): " -n 1 -r
echo

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    print_status "Cache cleanup cancelled"
    exit 0
fi

print_status "Fetching cache list..."

# Get all caches
CACHES=$(gh api \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    /repos/fernando-duarte/heteroskedasticity_identification/actions/caches \
    --jq '.actions_caches[].id')

if [ -z "$CACHES" ]; then
    print_status "No caches found"
    exit 0
fi

COUNT=$(echo "$CACHES" | wc -l | tr -d ' ')
print_status "Found $COUNT cache(s)"

# Delete each cache
for CACHE_ID in $CACHES; do
    print_status "Deleting cache $CACHE_ID..."
    gh api \
        --method DELETE \
        -H "Accept: application/vnd.github+json" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        /repos/fernando-duarte/heteroskedasticity_identification/actions/caches/$CACHE_ID
done

print_success "All caches deleted successfully"
print_status "Next builds will start with fresh cache"
