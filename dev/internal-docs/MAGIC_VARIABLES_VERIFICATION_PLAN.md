# Magic Variables Documentation Correction Plan

## Overview
This document provides a systematic plan for verifying and correcting the magic variables documentation to ensure 100% accuracy and completeness.

## Task Objective
Perform a comprehensive audit of magic numbers and strings in R package code and directly update the MAGIC_VARIABLES_DOCUMENTATION.md file to correct any errors, add missing items, and remove false positives.

## Files to Examine
The following R files must be systematically analyzed:

1. `R/analysis.R`
2. `R/data-generation.R`
3. `R/data.R`
4. `R/estimation.R`
5. `R/globals.R`
6. `R/lewbel-monte-carlo.R`
7. `R/messager.R`
8. `R/simulation-helpers.R`
9. `R/simulation.R`
10. `R/stopper.R`
11. `R/utils-df.R`
12. `R/utils-hetid.R`
13. `R/utils.R`
14. `R/visualization.R`

## Magic Number/String Classification

### INCLUDE as Magic Values:
- **Numeric literals**: `10`, `0.05`, `1e-6`, `100`, `1.96`
- **String literals as identifiers**: `"Y1"`, `"Y2"`, `"Xk"`, `".do"`, `".log"`
- **Array indices as constants**: `[1]`, `[2]`
- **Mathematical constants**: `mean = 2`, `sd = 1`
- **Configuration defaults**: sample sizes, replications, thresholds
- **Display parameters**: digits, bins, limits, font sizes
- **File extensions and system paths**
- **Environment variable names**
- **Default parameter values in function signatures**

### EXCLUDE from Magic Values:
- **Documentation strings and comments**: anything after `#` or in `#'` blocks
- **Variable names and function names**
- **Obvious mathematical operations**: like `x + y`, `n - k`
- **R language keywords and built-in constants**
- **Package namespace references**: like `stats::`, `ggplot2::`

## Systematic Verification Process

### Phase 1: File-by-File Analysis

For each R file, perform the following steps:

1. **Numeric Pattern Scan**
   - Use regex pattern: `[0-9]+\.?[0-9]*`
   - Identify all numeric literals in executable code
   - Exclude numbers in comments and documentation

2. **String Literal Identification**
   - Look for quoted strings that serve as constants
   - Focus on column names, file extensions, identifiers
   - Exclude descriptive text and error messages

3. **Function Default Examination**
   - Check all function signatures for default parameter values
   - Note any numeric or string defaults

4. **Array Indexing Analysis**
   - Identify constant array/list access patterns
   - Note matrix column/row access with fixed indices

5. **Location Recording**
   - Record exact file name and line number
   - Note the code context for each magic value

### Phase 2: Documentation Cross-Reference

For each identified magic value:

1. **Check Documentation Status**
   - Is it listed in MAGIC_VARIABLES_DOCUMENTATION.md?
   - Are the file location and line number correct?

2. **Validate Usage Description**
   - Are downstream usage patterns accurately described?
   - Are cross-file references complete?

3. **Verify Context Explanation**
   - Is the purpose and context correctly explained?
   - Are recommendations appropriate?

### Phase 3: Documentation Updates

Use the `str-replace-editor` tool to make necessary corrections:

#### Adding Missing Magic Values
```markdown
### New Magic Value: `value`

**Definition Location:**
- `R/filename.R:line` - `code context`

**Usage Downstream:**
- Description of how it's used
- Any cross-file references

**Context:** Explanation of purpose

**Recommendation:** Suggested constant name or refactoring approach
```

#### Correcting Existing Entries
- Update incorrect line numbers
- Add missing usage locations
- Improve context descriptions
- Enhance recommendations

#### Removing False Positives
- Delete entries that are not actually magic numbers
- Update frequency counts in summary sections
- Remove from appendix inventory

## Special Attention Areas

### High-Priority Patterns to Verify:

1. **Function Parameter Defaults**
   ```r
   function(param = 100, alpha = 0.05)
   ```

2. **Array/Matrix Indexing**
   ```r
   matrix[, 1]
   list[[2]]
   seeds$main[1]
   ```

3. **Statistical Thresholds**
   ```r
   if (p_value < 0.05)
   F_stat > 10
   z_critical = 1.96
   ```

4. **Loop Bounds and Ranges**
   ```r
   for(i in 1:n)
   1:config$num_simulations
   ```

5. **Conditional Checks**
   ```r
   if (abs(value) < 1e-6)
   nrow(data) > 5
   ```

6. **String Constants**
   ```r
   names(data) == "Y1"
   tempfile(fileext = ".do")
   ```

7. **Display Formatting**
   ```r
   digits = 4
   bins = 50
   base_size = 14
   ```

## Quality Assurance Checklist

After completing all updates:

- [ ] All line numbers are accurate and current
- [ ] No duplicate entries exist
- [ ] Usage frequency counts are correct
- [ ] Cross-references between sections are consistent
- [ ] Appendix matches main content
- [ ] All magic values have appropriate recommendations
- [ ] Documentation structure is maintained
- [ ] Formatting is consistent throughout

## Expected Outcomes

The corrected MAGIC_VARIABLES_DOCUMENTATION.md should:

1. **Contain every magic number/string** in the R codebase
2. **Have accurate file locations** and line numbers
3. **Provide complete usage descriptions** with downstream references
4. **Maintain consistent formatting** and structure
5. **Serve as a reliable reference** for systematic refactoring

## Success Criteria

The verification is complete when:
- 100% of magic values are documented
- All line numbers are verified accurate
- Usage patterns are comprehensively described
- Cross-file dependencies are mapped
- Refactoring recommendations are provided for all entries
- Documentation is ready for immediate use in code refactoring

## Implementation Notes

- **Only edit** the MAGIC_VARIABLES_DOCUMENTATION.md file
- **Do not modify** any R source files
- **Do not create** additional documentation files
- **Use systematic approach** to ensure nothing is missed
- **Maintain existing structure** while improving accuracy
