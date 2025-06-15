#!/bin/bash
# Test GitHub Actions workflows locally

echo "GitHub Actions Workflow Test Script"
echo "==================================="

# Check if act is installed
if ! command -v act &> /dev/null; then
    echo "❌ 'act' is not installed. Install it with: brew install act"
    echo "   See: https://github.com/nektos/act"
else
    echo "✅ 'act' is installed"
fi

# Validate workflow syntax
echo -e "\n📋 Validating workflow syntax..."

for workflow in .github/workflows/*.yml; do
    echo -n "Checking $(basename "$workflow")... "
    if yamllint -d relaxed "$workflow" 2>/dev/null; then
        echo "✅"
    elif python3 -c "import yaml; yaml.safe_load(open('$workflow'))" 2>/dev/null; then
        echo "✅"
    else
        echo "❌ (syntax error)"
    fi
done

# Check for required files
echo -e "\n📁 Checking required files..."
files=(
    "DESCRIPTION"
    "Dockerfile" 
    "Dockerfile.dev"
    "_pkgdown.yml"
)

for file in "${files[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file exists"
    else
        echo "❌ $file missing"
    fi
done

# List workflows
echo -e "\n🔧 Available workflows:"
for workflow in .github/workflows/*.yml; do
    name=$(basename "$workflow" .yml)
    echo "  - $name"
done

echo -e "\n💡 To test a workflow locally with act:"
echo "   act -W .github/workflows/R-CMD-check.yml"
echo "   act -W .github/workflows/pkgdown.yml"
echo "   act -W .github/workflows/docker.yml -P ubuntu-22.04=catthehacker/ubuntu:act-22.04"

echo -e "\n📝 Summary of fixes applied:"
echo "1. ✅ R-CMD-check.yml: Updated to use r-lib/actions/setup-r-dependencies"
echo "2. ✅ Docker.yml: Simplified image tagging and artifact handling"
echo "3. ✅ pkgdown.yml: Updated to use r-lib/actions/setup-r-dependencies"
echo "4. ✅ r-security.yml: Complete rewrite for simplicity and reliability"
echo "5. ✅ WORKFLOWS.md: Updated documentation to match actual workflows"

echo -e "\n🚀 Next steps:"
echo "1. Commit these changes to a feature branch"
echo "2. Create a pull request to test the workflows"
echo "3. Monitor the GitHub Actions tab for results"
echo "4. Set up OSSINDEX_USER and OSSINDEX_TOKEN secrets for better security scanning"