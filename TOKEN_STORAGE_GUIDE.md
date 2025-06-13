# Best Practices for Storing GitHub Tokens

## 🔐 Security First

**NEVER commit tokens to Git!** This guide shows secure ways to store and use your GitHub token.

## Option 1: .env File (Recommended for Development)

### Setup
1. Create a `.env` file in your project root:
```bash
touch .env
echo "GITHUB_TOKEN=your_token_here" >> .env
```

2. The `.env` file is already in `.gitignore` (I added it for you)

3. Update the script to load from .env:
```bash
# The enable_github_pages.sh script already checks for GITHUB_TOKEN
# Just source the .env file before running:
source .env && ./enable_github_pages.sh
```

### For R Projects
Create a `.Renviron` file for R-specific usage:
```bash
echo "GITHUB_TOKEN=your_token_here" >> .Renviron
```

Then in R:
```r
# Automatically loaded when R starts
Sys.getenv("GITHUB_TOKEN")
```

## Option 2: Shell Configuration (Recommended for Personal Machine)

Add to your shell config file (`~/.zshrc` or `~/.bash_profile`):
```bash
echo 'export GITHUB_TOKEN="your_token_here"' >> ~/.zshrc
source ~/.zshrc
```

## Option 3: macOS Keychain (Most Secure for Mac)

Store in keychain:
```bash
security add-generic-password -a "$USER" -s "GITHUB_TOKEN" -w "your_token_here"
```

Retrieve from keychain:
```bash
export GITHUB_TOKEN=$(security find-generic-password -a "$USER" -s "GITHUB_TOKEN" -w)
```

## Option 4: 1Password CLI (If You Use 1Password)

```bash
# Store in 1Password, then:
export GITHUB_TOKEN=$(op item get "GitHub Token" --fields token)
```

## Best Practices Summary

### ✅ DO:
- Use `.env` files for project-specific tokens
- Add `.env` to `.gitignore` immediately
- Use different tokens for different projects
- Set token expiration dates
- Use minimal required scopes (just `repo` for GitHub Pages)
- Rotate tokens regularly

### ❌ DON'T:
- Commit tokens to Git (even in private repos)
- Share tokens with others
- Use the same token everywhere
- Store tokens in plain text files without `.gitignore`
- Echo tokens in terminal (they get saved to history)

## Quick Setup for This Project

```bash
# 1. Create .env file
cat > .env << EOF
GITHUB_TOKEN=your_actual_token_here
EOF

# 2. Use it
source .env && ./enable_github_pages.sh

# 3. For R sessions, create .Renviron
cp .env .Renviron
```

## Checking Your Git History

If you accidentally committed a token:
```bash
# Check if any secrets are in git history
git log -p | grep -i "token\|secret\|key\|password"

# If found, you need to:
# 1. Revoke the token immediately on GitHub
# 2. Use BFG Repo-Cleaner or git filter-branch to remove from history
# 3. Force push (coordination needed with other contributors)
```

## Token Permissions

For this project, you only need:
- [x] `repo` - Full control of private repositories

Don't add unnecessary permissions like:
- [ ] `admin:org`
- [ ] `delete_repo`
- [ ] `admin:public_key`

## Automation Tip

Create a helper script `setup_env.sh`:
```bash
#!/bin/bash
if [ ! -f .env ]; then
    echo "Creating .env file..."
    echo "Please enter your GitHub token:"
    read -s token
    echo "GITHUB_TOKEN=$token" > .env
    echo "✅ .env file created"
else
    echo "✅ .env file already exists"
fi
```

Remember: Security is not convenient, but it's necessary! 🔒 