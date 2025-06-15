# REndo Comparison Report

## Summary

The hetid package and REndo's `hetErrorsIV()` implement **different identification strategies** and are not directly comparable.

## Key Differences

### 1. Identification Strategy

**hetid (Lewbel 2012)**:
- Identifies through heteroskedasticity in the structural equation errors
- Constructs instrument: `(Z - mean(Z)) * e2` where `e2 = residuals(Y2 ~ X)`
- Z is specifically generated to satisfy Lewbel's moment conditions

**REndo's hetErrorsIV**:
- Identifies through heteroskedasticity in the reduced form `X ~ P` relationship
- Tests whether `Var(X|P)` is non-constant
- Uses squared residuals from `X ~ P` regression as instruments

### 2. Performance on Lewbel-type Data

When using data generated according to Lewbel (2012):

| Method | Coefficient | Std. Error | First-stage F |
|--------|------------|------------|---------------|
| hetid (finite) | -0.800 | 0.00098 | 5,816 |
| REndo | -0.800 | 0.00527 | 0.27 |

**Key observations**:
- Coefficients match closely (0.012% difference) - both are consistent
- Standard errors differ by ~5x due to instrument strength
- REndo's F-statistic < 1 indicates very weak instruments
- REndo warns: "assumption of heteroscedasticity not satisfied"

### 3. Why the Difference?

Lewbel (2012) data generating processes create:
- Heteroskedasticity in the relationship between Z and ε₂²
- NOT necessarily heteroskedasticity in X ~ P

REndo is looking for the wrong type of heteroskedasticity for this DGP.

## Implications

### When to use each method:

**Use hetid when**:
- You have Lewbel (2012) type heteroskedasticity
- Structural errors exhibit heteroskedasticity related to instruments
- You need the specific Lewbel identification approach

**Use REndo's hetErrorsIV when**:
- You have heteroskedasticity in the X ~ P relationship
- Your exogenous variables' variance depends on endogenous variables
- This specific pattern of heteroskedasticity exists in your data

## Conclusion

The hetid package correctly implements Lewbel (2012) and should not be expected to match REndo's `hetErrorsIV()` results. The ~5x difference in standard errors is not due to degrees of freedom adjustments but rather due to fundamentally different identification strategies and instrument strength.

**Bottom line**: hetid successfully replicates Lewbel (2012), while REndo implements a different heteroskedasticity-based identification approach.