# hetid Package Hex Sticker

This directory contains the hex sticker design for the `hetid` package.

## Design (`hex.png`)

### Theme: Heteroskedasticity Visualization with Bias Correction

The hex sticker visually demonstrates the core problem that the `hetid` package solves: endogeneity bias in the presence of heteroskedasticity.

### Features:
- **Blue confidence bands**: Show increasing variance from left to right (heteroskedasticity)
- **Pink scatter points**: Represent actual data with heteroskedastic errors
- **Orange solid line**: True relationship (γ = -0.8) correctly estimated by Lewbel/2SLS method
- **Gray dashed line**: Severely biased OLS estimate (γ ≈ -0.2) due to endogeneity
- **Clean design**: No URL for professional use in papers and presentations

### Visual Story:
The dramatic difference between the two regression lines immediately communicates why heteroskedasticity-based identification matters. The OLS line (gray dashed) shows a 75% bias, while the Lewbel method (orange solid) recovers the true parameter.

## Color Palette
- **Primary Blue** (#2E86AB): Package name, borders, confidence bands
- **Pink** (#A23B72): Data points
- **Orange** (#F18F01): True/Lewbel regression line
- **Gray** (#6C757D): Biased OLS line
- **Light Gray** (#F5F5F5): Background

## Usage

Add the hex sticker to your README:
```markdown
<img src="inst/hex/hex.png" align="right" height="139" />
```

Or in package documentation:
```r
#' @section Logo:
#' \if{html}{\figure{hex.png}{options: width=120 alt="hetid hex sticker"}}
```

## Regenerating the Sticker

To regenerate or modify the sticker, run:
```r
source("inst/hex/hexSticker.R")
```
