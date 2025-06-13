# Enable GitHub Pages for Documentation

To enable GitHub Pages and make your documentation publicly accessible:

1. Go to your repository: https://github.com/fernando-duarte/heteroskedasticity_identification

2. Click on **Settings** (in the repository navigation)

3. Scroll down to **Pages** section (in the left sidebar under "Code and automation")

4. Under **Source**, select:
   - **Deploy from a branch**
   
5. Under **Branch**, select:
   - **gh-pages**
   - **/ (root)**
   
6. Click **Save**

7. Wait a few minutes for the site to build

8. Your documentation will be available at:
   https://fernando-duarte.github.io/heteroskedasticity_identification/

## Automatic Updates

The documentation in the `gh-pages` branch was built from your `main` branch. To update it in the future:

```r
# In R, from your package directory
pkgdown::build_site()
```

Then push the updated `docs/` folder to your `main` branch. You can manually update the `gh-pages` branch when you want to publish new documentation. 