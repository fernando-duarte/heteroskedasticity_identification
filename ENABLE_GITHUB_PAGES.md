# Enable GitHub Pages for Documentation

You can enable GitHub Pages either via the API or manually through the web interface.

## Option 1: Using the Script (Easiest)

I've created a script to enable GitHub Pages for you:

```bash
# First, create a personal access token:
# 1. Go to https://github.com/settings/tokens
# 2. Click "Generate new token (classic)"
# 3. Give it a name and select the `repo` scope
# 4. Click "Generate token" and copy it

# Then run:
export GITHUB_TOKEN='your_token_here'
./enable_github_pages.sh
```

## Option 2: Using GitHub API Directly

If you prefer to run the API command directly:

```bash
curl -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer YOUR_GITHUB_TOKEN" \
  "https://api.github.com/repos/fernando-duarte/heteroskedasticity_identification/pages" \
  -d '{"source":{"branch":"gh-pages","path":"/"}}'
```

## Option 3: Manual Setup

1. Go to your repository: https://github.com/fernando-duarte/heteroskedasticity_identification

2. Click on **Settings** (in the repository navigation)

3. Scroll down to **Pages** section (in the left sidebar under "Code and automation")

4. Under **Source**, select:
   - **Deploy from a branch**
   
5. Under **Branch**, select:
   - **gh-pages**
   - **/ (root)**
   
6. Click **Save**

## Access Your Documentation

After enabling (either method), wait a few minutes for the site to build.

Your documentation will be available at:
https://fernando-duarte.github.io/heteroskedasticity_identification/

## Automatic Updates

The documentation in the `gh-pages` branch was built from your `main` branch. To update it in the future:

```r
# In R, from your package directory
pkgdown::build_site()
```

Then push the updated `docs/` folder to your `main` branch. You can manually update the `gh-pages` branch when you want to publish new documentation. 