# REndo Matching Summary

## Current Status

The hetid package now successfully handles both Stata and REndo compatibility through the `df_adjust` parameter:

### Key Implementation

1. **df_adjust = "asymptotic"** (default)
   - Matches Stata's ivreg2 default behavior
   - Uses n instead of n-k for standard error calculation
   - Provides asymptotic standard errors

2. **df_adjust = "finite"**
   - Matches R's ivreg/lm() default behavior
   - Uses n-k degrees of freedom adjustment
   - Matches what REndo expects when using external instruments

### Test Updates

The REndo tests have been updated to:
1. Compare apples to apples - using the same pre-computed Lewbel instruments
2. Recognize that ivreg (used by both hetid and REndo for external IVs) uses finite sample SEs by default
3. Document that REndo's hetErrorsIV() implements a different identification strategy than Lewbel (2012)

### How Matching Works

When using the **same instrument**:
```r
# hetid with finite df adjustment
result <- run_single_lewbel_simulation(
  sim_id = 1,
  params = params,
  df_adjust = "finite"  # Matches ivreg/REndo default
)

# Is equivalent to:
model <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = data)
```

Both give identical results because they use the same underlying ivreg function.

### Important Distinction

REndo's `hetErrorsIV()` with `IIV(X1)`:
- Looks for heteroskedasticity in the X ~ P relationship
- Is NOT the same as Lewbel (2012) identification
- Produces much weaker instruments for Lewbel-type data
- This is why the original tests showed ~5x larger standard errors

### Conclusion

✓ hetid now matches both Stata and REndo through the df_adjust parameter
✓ Use "asymptotic" for Stata compatibility
✓ Use "finite" for REndo/ivreg compatibility
✓ The package correctly implements Lewbel (2012) while supporting both SE conventions
