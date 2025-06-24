# Refactor Plan: Achieving DRY & SSOT in the Econometrics R Package

## Introduction and Goals

This plan outlines a comprehensive step‑by‑step refactoring of the econometrics R package to fully embrace **DRY (Don't Repeat Yourself)** and **SSOT (Single Source of Truth)** principles. In essence, every piece of knowledge or logic in the codebase should be defined *once* and used everywhere it's needed. By eliminating duplication and centralizing definitions, we will make the package easier to maintain and less error‑prone. Importantly, **this is a pure refactor** – we will **not change any functionality or external behavior** of the package. As Martin Fowler defines, *refactoring is restructuring code without changing its observable behavior*, performed via many small, behavior‑preserving steps. After each tiny change, all tests and checks must pass before proceeding. We will adhere to that discipline rigorously.

The package is already in a healthy state (lint‑free code, nearly 100 % tests coverage, CI/CD, Docker, etc.), which gives us a solid safety net. Our strategy will leverage that safety net at every step:

* **Continuous Verification:** After *every* refactoring step, we run the full test suite, linters, and `R CMD check` to ensure nothing broke. We **never bypass or skip** these checks – no quick hacks or disabling tests allowed. If something fails, we fix the issue **before moving on**. This guarantees that functionality, inputs/outputs, and performance remain unchanged throughout. Review the DESCRIPTION file diff to ensure any new package dependencies are correctly declared in Imports.

* **Strict Git Workflow:** We will perform work on temporary **non‑`main` branches**. Each logically distinct refactoring (or group of closely related changes) will happen in its own branch. We commit after each successful refactoring step (when tests and checks are green), with clear messages. We merge back into `main` only when that branch’s changes are thoroughly verified. Importantly, **we never delete these refactor branches** – keeping them allows traceability and potential rollback if ever needed (though we aim not to need it!).

  *If a regression is discovered after a merge to main, the merge can be safely undone without losing history by creating a new commit that reverts the changes. The command for this is `git revert -m 1 <merge-commit-hash>`. When multiple merges have occurred, or the regression’s origin is unclear, use `git bisect` to pinpoint the first bad commit and then apply an appropriate revert.*

  All merges should be done via Pull Requests (PRs) to ensure CI runs and to provide a space for documentation and review. We recommend using a squash-and-merge strategy. This condenses all the small, iterative commits on the feature branch into a single, clean, and atomic commit on the main branch. This keeps the main history readable while the full detailed history remains available on the feature branch.

  All merges should be done via PRs or equivalent to ensure CI runs and for documentation, but without requiring any special approval from project owners unless absolutely necessary.

  *CI optimisation tip:* heavy static‑analysis steps (e.g., `lintr`, `dupree`) can be cached to speed up repeat runs. If using GitHub Actions, add something like:

  ```yaml
  - uses: actions/cache@v4
    with:
      path: ~/.cache/R/lintr
      key: ${{ runner.os }}-lintr-${{ hashFiles('**/*.R') }}
  ```

* **Ultrathink Before Major Steps:** Before diving into any major portion of the refactor, we will pause for an “**Ultrathink**” session – to plan the approach in detail, do brief research (as of June 2025) on best practices or similar refactor experiences, and adjust our plan accordingly. This prevents rushing into a complex change blindly. Essentially, *measure twice, cut once*.

* **Temporary Refactor Workspace:** We will use a `temp-refactor/` folder at the project root to store any intermediate results, analysis scripts, or documentation during this process. For example, reports of duplicated code, one‑off scripts to aid refactoring, or notes can reside there. We’ll add this folder to `.Rbuildignore` so that it doesn’t affect package build or checks. In the end, the entire `temp-refactor/` directory can be deleted with no impact on the package (it’s purely for our use during refactoring).

* **Junior‑Developer Friendly Tips:** Throughout the plan, we include implementation tips and explanations to help a less experienced developer follow along independently. We’ll explain not just the “what” but also the “why” behind each step, provide guidance on tools and commands to use, **and link to helpful chapters in *R Packages (2e)* or RStudio cheatsheets where appropriate for deeper learning**.

* **Encouragement and Milestones:** Refactoring can be challenging, so we’ll celebrate major milestones! 🎉 As you complete each phase of this plan, take a moment to appreciate the progress. This will keep motivation high and recognize the incremental achievements.

* **Namespace / Import Awareness (NEW):** Because refactoring introduces new internal helpers and may move code across files, we will explicitly monitor **`NAMESPACE` changes** generated by roxygen2. After each refactor step that adds or removes functions, run `devtools::document()` and review the diff to confirm that only *internal* helpers remain un‑exported, and any new imports (`@importFrom`) are correctly declared. For S3/S4 methods, ensure that new generics or methods are registered (`@export` for generics, `@method` tags, or `setMethod`) and that dispatch continues to work.

* **pkgdown Site Checkpoints (NEW):** At the end of Phases 1, 3, and 4 we will build the site with `pkgdown::build_site()` to confirm that documentation renders without errors or warnings. Host‑specific links help catch broken Rd inheritance or missing topics early.

Now, let’s get started. **Phase 1** will tackle redundancy detection and quick wins in an entirely automated way, before we move on to carefully planned manual refactoring steps.

## Phase 1: Fully Automated Redundancy Detection & Reduction 🚀

In this first phase, we use automated tools to identify and eliminate low‑hanging fruit in terms of code repetition and inefficiency, **without any manual code edits**. The goal is to let the tools do as much as possible, while we oversee and verify. We will be using two excellent R packages for this:

* **`dupree`** – to detect duplicated code blocks across the project.
* **`styler`** and **`lintr`** – to automatically format code to tidyverse style and to highlight (and in some cases auto‑fix) common lint issues and sub‑optimal idioms.
  *`NOTE — tool correction:* the originally‑mentioned package **“flir”** appears to be a misunderstanding or a renamed internal prototype; it does not exist on CRAN or GitHub. `styler` (for code re‑formatting) together with `lintr` (for lint detection) is the modern, actively‑maintained replacement stack that achieves the same purpose of automated, behavior‑preserving clean‑ups.*

These tools will help standardize the code and possibly remove some duplication (e.g. by highlighting occurrences of sub‑optimal patterns that we will later DRY out). All of this will be done programmatically. **We will not manually modify the code in this phase** – our interventions will only be to run the tools and possibly revert any of their changes that turn out to be problematic. Think of this phase as the “automated cleanup” phase.

### 1.1 Set Up an Automation Branch and Environment

1.  **Create a new branch for Phase 1:** In Git, create and switch to a branch named something like `refactor-auto-dup-lint`. For example:
    ```bash
    git checkout -b refactor-auto-dup-lint
    ```
    This isolates our automated changes from the main branch. All Phase 1 commits will go here.

2.  **Ensure baseline is green:** Run the package’s tests and checks on this branch *before* making any changes, to confirm we’re starting from a good state. For instance, in R:
    ```r
    devtools::check()  # or devtools::test() and lintr::lint_package() as appropriate
    ```
    You should see all tests passing, no linter issues, and `R CMD check` with 0 errors/warnings (since the package is known to be clean). This baseline gives confidence that any failure we see after this phase is due to our changes.

3.  **Identify Snapshot Tests (Crucial Safety Check):** Check if the package uses snapshot tests (`testthat::expect_snapshot()` or visual snapshots with **vdiffr**). Automated formatters like **styler** will almost certainly alter snapshot files by changing whitespace or code formatting, causing these tests to fail.
    If snapshot tests are present, be prepared to review and update them after running **styler**. You will use `testthat::snapshot_review()` to interactively accept the (expected) formatting changes. **Remember to add and commit the updated `_snaps/` directory before pushing**, otherwise CI will still fail. This is not a bug; it's a planned maintenance step. Acknowledging this now prevents surprises on CI.

4.  **Set up `temp-refactor/` folder:** Create a directory `temp-refactor/` in the project root. Inside it, we will store outputs like the duplication report. Add this directory to `.Rbuildignore` so that R CMD check ignores it. You can do this with:
    ```r
    usethis::use_build_ignore("temp-refactor")
    ```
    (This adds a line to `.Rbuildignore` automatically.) Commit this minor change (it's a meta change, not affecting code logic).

5.  **Install/Update automation tools:** Ensure you have the latest versions of **dupree**, **styler**, and **lintr** installed (they are actively maintained as of 2025). For example:
    ```r
    install.packages(c("dupree", "styler", "lintr"))
    ```
    Having the latest version means we get the most robust detection and fixes.

### 1.2 Detect Duplicated Code with `dupree`

6.  **Run `dupree` to find duplicate code blocks:** Using dupree, we will scan the entire package for similar code segments. Dupree can analyze R scripts and tests to find chunks of code that are very alike. We will use it in “package” mode to cover the `R/` directory (and possibly tests). In R, run:
    ```r
    library(dupree)
    dup_results <- dupree::dupree_package(".", strip_roxygen = TRUE)
    ```
    Setting `strip_roxygen = TRUE` avoids treating identical roxygen comment blocks as code, reducing false positives.

    This will return a tibble of pairs of code blocks with a similarity score. Higher scores (closer to 1.0) mean more similar code. Dupree uses a token‑based longest common subsequence method, ignoring trivial differences. Any code pairs with a score **> 0.5** are strong candidates for true duplication worth refactoring.

    **Save the duplication report** for later analysis:
    ```r
    readr::write_csv(dup_results, file = "temp-refactor/duplication_report.csv")
    ```
    Also, glance through `dup_results` in R to see what it found. (No manual code changes yet, just observing output is fine!) This gives you an idea of which files/functions have overlapping code.

    *Tip:* Dupree might flag code in tests as duplicated with code in R (for example, if a test reimplements a formula to compare with the function’s output). Such duplication can be intentional (to validate correctness) and not something we will refactor out. **Don’t worry** – we will use human judgment later to decide which duplications are real targets. Remember, not every repeated snippet is bad or needs fixing; we will filter out “coincidental” duplicates later (Ultrathink step).

### 1.3 Automated Formatting and Lint Fixes with `styler` & `lintr`

7.  **Run `styler` to automatically re‑format the package:** `styler` enforces a consistent tidyverse style and, in combination with `lintr`, catches certain anti‑patterns (e.g. `T`/`F` vs `TRUE`/`FALSE`). Although primarily a formatter, `styler` will sometimes also simplify obvious syntax (like redundant braces). In R:
    ```r
    library(styler)
    styler::style_pkg()
    ```
    Use the project’s configured style guide (if you have a `styler` config file) or the default tidyverse style. This step is purely cosmetic but crucial for diff readability in subsequent refactor commits.

8.  **Run `lintr` in “auto‑fix” mode for safe replacements:** Beginning with **lintr 3.1**, the helper to apply autofixes is called `apply_lints()`. Older versions used experimental names. We therefore guard the call with a version check:
    ```r
    library(lintr)

    if ("apply_lints" %in% ls("package:lintr")) {
      lintr::lint_package(".", cache = FALSE)
      lintr::apply_lints(".", ask = FALSE)
    } else {
      warning("lintr is < 3.1; consider upgrading to enable automatic fixes.")
      lintr::lint_package(".", cache = FALSE)
    }
    ```
    *If you stay on an older lintr, you can still copy the code frame of each lint and correct it manually or script your own replacements following the vignette.*

9.  **Run automated style formatters again:** Running `styler::style_pkg()` a second time after autofixes ensures everything is still in canonical style.

### 1.4 Verify Tests and Checks After Automation

10. **Run the full test suite and checks:** Now, **crunch time** – run:
    ```r
    devtools::check()
    ```
    This calls `R CMD check` under the hood, rebuilding vignettes, ensuring that the package installs from a *source tarball*, and catching issues that `load_all()` alone cannot. If all tests pass, fantastic! 🎉

    *   If **any test fails** or you see an error/warning in check:
        *   Identify the failing test and error message. Determine which change from flir (or any automated step) might have caused it. Because we didn’t do any manual changes, a failing test indicates that one of the automated replacements wasn’t equivalent in this context.
        *   For example, suppose a test expected a specific warning message substring, and flir changed the code to use a different function that alters the wording slightly. Or flir replaced a function call in a way that changes the class of the result (unlikely, but just hypothetically).
        *   **Do not ignore or bypass the failing test.** Instead, address it:
            *   If the change is clearly a mistake (flir’s rule misapplied), the simplest course is to **manually revert that specific change** to the original code. You can open the affected file, restore the original logic for that snippet, and add a comment `# TODO: flir suggested change reverted due to test failure; will review later`.
            *   If the change is good but the test is too strict (e.g., the test was checking for an exact error message that changed punctuation), you can update the test expectation to match the new output. However, be cautious: this means the behavior/output did change in a tiny way. Only do this if the change is truly superficial and acceptable (like a better wording of a message). In general, prefer reverting the code to keep behavior 100% identical.
        *   Once adjustments are made, run tests again and ensure they pass. Essentially, **we allow ourselves to undo any automated change that isn’t behavior-preserving**, since our prime directive is no behavior change.
    *   If **linters or R CMD check** show new issues (e.g., maybe flir introduced a line that is longer than your 80 char limit, or a new `library()` call or something odd):
        *   Fix those similarly (e.g., run `lintr::lint_package()` and address any newly flagged lint).
        *   For R CMD check notes, one possibility is if flir added a usage of a function that requires an import you don’t have. Unlikely, but if it did (perhaps using `anyNA` which is base so okay, but just be mindful), you might need to adjust NAMESPACE or DESCRIPTION Imports. No new package should be needed though; flir sticks to base alternatives or safe patterns.
        *   Ensure `R CMD check` ends clean.

11. **Commit the automated changes:** Once everything is passing, make a git commit on the `refactor-auto-dup-lint` branch. Use a message like “Automated refactor: apply flir fixes and generate duplication report (no behavior change)”. This commit marks the completion of Phase 1.

    Double-check that you include **all** changed files (R scripts and possibly some test files if flir touched them). Also include `temp-refactor/duplication_report.csv` in the commit (so that the data is saved in version control for reference, though we won’t ship this file with the package).

12. **Merge Phase 1 branch into main:** Now that the automated fixes are validated, merge this branch back into the `main` branch (or whatever is the primary branch). This could be via a PR that you self-review or directly via command:
    ```bash
    git checkout main
    git merge refactor-auto-dup-lint
    ```
    (Since tests are green, this should be straightforward.) **Do not delete** the `refactor-auto-dup-lint` branch after merging. Keep it in case someone later wants to see what was changed by automation.

> 🎉 **Milestone achieved:** The codebase has been cleaned up with automated tools, and all tests still pass! At this point, the package might already have fewer redundancies (thanks to standardized code replacements). Great job kicking things off on a high note! Take a moment to appreciate that mundane, repetitive patterns have been zapped by automation. Now it’s time to plan our more surgical manual refactoring steps.

## **Ultrathink:** Analyzing Duplication and Planning Next Steps

Before jumping into manual refactoring, let’s do an **Ultrathink** analysis of the duplication report and outline our plan of attack for phases 2 onward.

Open the `temp-refactor/duplication_report.csv` (or examine the `dup_results` object in R). Look for the following:

* **True Duplications vs Coincidental Similarities:** Identify which code block pairs (or groups) represent the *same underlying logic*. Focus on those. If two code blocks look very similar and have a high similarity score (e.g., 0.6 or above) and you know they perform the same task, mark them as candidates to refactor into one source. If some code is similar but actually does different things (e.g., two loops that both iterate but over different structures with different purposes), that might be coincidental duplication. According to the SPOT/DRY principle, we **do not** abstract code that is only accidentally similar. We only DRY out code that represents the same “fact” or operation in the program. Use your understanding of the package’s domain to make this call. For each duplication candidate, ask: “If I change how this works in one place, should the others change too?” If yes, it’s the same fact and should have a single source of truth. If no, leave it separate.

* **Prioritize by Impact:** Among the true duplications, prioritize them by **benefit/cost**:

  * **High Benefit, Low Cost:** These are the “easy wins” to do first. e.g. A 10-line identical (or almost identical) code snippet copied in 3 functions – definitely worth extracting to one function. Or a constant value used in multiple places – trivial to unify (we’ll do constants in Phase 2). These refactors reduce a lot of maintenance burden and are straightforward to implement.
  * **High Benefit, Higher Cost:** e.g. A larger block of logic duplicated in 2-3 places, but with slight variations, meaning we have to carefully parameterize or handle edge cases to unify them. These are worth doing (they eliminate substantial duplication), but we should plan them carefully to ensure we don’t introduce bugs. We will tackle these after the easy wins.
  * **Lower Benefit or Uncertain:** e.g. Very short duplications (2-3 lines) that are harmless or could even be coincidental. If abstraction would make the code *less clear* or require an awkward function, we might decide not to refactor these. Remember, **avoid creating an abstraction that doesn’t truly fit** – the wrong abstraction can be worse than duplication. (This is the “rule of three” philosophy: it's not always beneficial to DRY out code that is duplicated only twice unless you're sure it's the same thing.)

**Warning: The Abstraction Trap**

As you analyze duplication, you will find code that is 95% similar but has small, crucial differences. The temptation is to create a single, "unified" helper function with multiple boolean flags or complex `if/else` logic to handle all variations. **Resist this temptation.** A bad abstraction—a convoluted function with a confusing signature—is often harder to understand and maintain than the original duplication.

* **Guideline:** If a unified helper function requires more than one or two simple parameters to control its behavior, or if the internal logic becomes tangled, the abstraction is likely a poor fit.
* **Action:** In such cases, consider either:
    1.  Factoring out only the **truly identical** sub-part of the code into a smaller, simpler helper.
    2.  Accepting the duplication as the clearer option. Clarity trumps dogmatic adherence to DRY.

* **Plan by Category:** It can help to categorize the types of duplication:

  * *Repeated calculations or formulae:* e.g., maybe the package computes a certain statistical measure in multiple places with the same formula. That should be one internal function.
  * *Repeated argument handling or input checking:* e.g., several functions might check inputs in the same way. We can centralize that.
  * *Repeated constant values or thresholds:* (We know we have to handle magic numbers – that’s Phase 2 coming up next!)
  * *Documentation repetition:* (We’ll handle in a later phase).
  * *Test code repetition:* (Also later phase).

* **Research if needed:** If any duplication seems tricky to refactor (e.g., involves S3/S4 methods or compiled code), pause to see if there are known patterns. For example, if multiple C++ functions duplicate code, perhaps the fix is to refactor into a static inline function. If multiple R functions for different classes have similar logic, maybe introduce a new generic or utilize existing generic mechanisms. A quick web search or checking how similar CRAN packages structure such code in 2025 could give insight. (E.g., search for “R refactor duplicate code <topic>” if needed.) If the package is using S3, sometimes the solution is a hidden internal function called by multiple methods. If using S4, perhaps a slot in a class can unify something. Plan the approach that fits the situation.

*   **Identify Non-Standard Evaluation (NSE) Hotspots (CRITICAL):** Scan the duplicated code blocks for any use of NSE. This is common in R code that interacts with data frames, such as `subset()`, `with()`, `transform()`, or any `dplyr` verbs that use bare column names (e.g., `filter(cyl == 4)`).

    **This is a major refactoring risk.** Simply copying NSE-based code into a standard helper function **will fail** because the function won't know where to find the column names (e.g., `cyl`). Refactoring this code correctly requires using the **`rlang` tidyevaluation framework** (e.g., embracing the `{{...}}` operator). Mark any such duplication as high-cost and requiring special expertise.

* **Set the sequence:** We will proceed in the following general order for manual refactoring:

  1. **Small, high-value refactors first:** e.g., unify constants, trivial duplicate snippets. (These are quick and de-risked by tests.)
  2. **Medium refactors next:** factor out medium-sized duplicate blocks into new functions.
  3. **Largest or most complex refactors last:** e.g., dealing with any duplication that spans multiple files or requires structural changes (hopefully none too drastic in a small package). This also includes any duplicates in compiled code or performance-critical code – we leave those for last so we have maximum confidence and all tests at the ready.
  4. **Non-code duplication:** documentation and tests, being last since they don't affect user-facing behavior (though we will keep documentation up to date as we go, of course).

Now that we have a plan and priority list, let’s proceed to Phase 2, tackling one of the easiest yet high-impact areas: eliminating duplicate constant values and ensuring each constant has a single source of truth.

## Phase 2: Single Source of Truth for Constants and Magic Numbers

One of the simplest forms of duplication is using the same literal value in multiple places. This could be a number like `0.05` used as a default significance level in several functions, or a string like `"method = \"OLS\""` repeated in messages. Such values should be defined once and referenced everywhere. This way, if the value ever needs to change, you do it in one place – **Single Source of Truth (SSOT)** for that value. It’s a quick win that also documents intent (a named constant is clearer than a “magic” number).

**Actions in this phase will:**

* Introduce named constants for repeated values.
* Not change any behavior (we’re literally using the same values, just via a variable).
* Make the code more self-documenting and easier to tweak in the future.

All changes here are very low risk, but we still do them carefully with tests after each replacement.

### 2.1 Identify Repeated Constants

1.  **Search for candidate literals:** Scan through the R code (and tests) for any *literal values* or *strings* that appear multiple times. Common examples:
    *   Numerical constants like `0`, `1`, `0.05`, `1e-6`, etc., that have a specific meaning in context (e.g. tolerance, alpha level). Use your editor’s "Find in Files" for such values. If a number appears in many places but each context is unrelated (like `4` in “4 cores” vs `4` in something else), that’s coincidental, not a single concept. We only unify if it’s the same concept.
    *   Character strings that are duplicated. Perhaps the package issues warnings or errors with identical phrasing in multiple functions – those could be pulled into a single constant string (or a function) to avoid divergence.
    *   Any lengthy vector or data structure copied in two places (less likely in a small package, but if so, that’s a candidate to refactor into a single object).

2.  **Review function defaults:** Check function definitions to see if the same default argument value is used in multiple functions. For example, if two different functions have an argument `conf_level = 0.95`, that `0.95` could be a constant. Consistent defaults across functions are good; ensuring they reference a single constant guarantees they stay consistent.

Write down the list of duplicates found. For each, decide on a **name** for the constant that clearly conveys its meaning (and perhaps its units if applicable). Since these will be internal, we can prefix with a dot (e.g., `.ALPHA_DEFAULT`) or use lowercase but all-caps for clarity that it’s a constant. There’s no strict rule in R, but a common convention is to use all-caps for constants. We’ll also not export these constants.

### 2.2 Introduce Constants and Replace Literals

3.  **Create a constants script:** Use
    ```r
    usethis::use_r("00-constants", open = FALSE)
    ```
    This creates `R/00-constants.R`, which alphabetically collates *first*, guaranteeing the constants are sourced before any function that uses them. If the package already has a non‑standard **Collate:** field in DESCRIPTION, add `00-constants.R` at the top of that list. This prevents intermittent “object '.ALPHA\_DEFAULT' not found” errors when the package is installed from a source tarball (where alphabetical loading is strictly observed). Inside this file, define your constants—for example:
    ```r
    #' @keywords internal
    # Default significance level for tests:
    .ALPHA_DEFAULT <- 0.05
    .MAX_ITER      <- 1000
    .ERROR_MSG_TEMPLATE <- "Convergence not achieved in %d iterations"
    ```

4.  **Replace occurrences in code:** Go through the code and replace every literal occurrence with the constant:
    *   E.g., every time `0.05` was used to mean a significance level, replace it with `.ALPHA_DEFAULT`. This includes in function bodies, and possibly in tests expectations if relevant.
    *   When replacing, be careful to replace whole values, not just substrings. For instance, searching for “0.05” and doing a blind replace might also catch “10.05” if it existed. So confirm each replace context.
    *   Use your IDE or a controlled find-replace to do this systematically, one file at a time, so you can verify you’re not altering something unintended.
    *   If the value appears in **documentation examples or vignettes**, you might also update it there to keep consistency (only if it doesn’t confuse the example – often examples show literal values for clarity).

5.  **Replace occurrences in tests:** If tests have hard-coded these values, they will still be valid because the value hasn’t changed. However, to follow SSOT, consider using the constant in tests too, if appropriate:
    *   For example, if a test does `expect_equal(object$alpha, 0.05)`, you could change it to `expect_equal(object$alpha, .ALPHA_DEFAULT)`. This way, tests also rely on the single source. But be mindful: if the test is specifically ensuring that the default is 0.05, using `.ALPHA_DEFAULT` in the test just mirrors the code. It might be better for the test to still check the actual numeric value to catch if someone unintentionally changes the constant. There’s a trade-off: tests ideally should know the expected outcome independent of code.
    *   Perhaps the best approach: leave tests as-is for checking actual values (ensuring behavior), but if the same expected value is used across many tests, you can define a testing constant for it. Given near-100% coverage, if 0.05 is expected in many places, define something like `alpha <- 0.05` at the top of the test file or in a helper. However, that is a minor detail. For now, focus on code duplication rather than test expectations duplication; we will revisit test refactor in Phase 5.
    *   Conclusion: It’s fine to keep tests comparing to literal `0.05` – they’ll still pass. Or define a single `TEST_ALPHA <- 0.05` in a test helper file and use that. Use your judgment; the priority is not to break tests. **Do not** modify what the test is fundamentally asserting (the number itself) unless necessary.

6.  **Multiple constants of same value:** If the same number appears for different reasons (e.g., `4` could appear as “4 predictors” somewhere and “4 default clusters” somewhere else), do **not** use one constant for both unless they truly represent the same concept. It’s okay to have two constants with the same value if they mean different things; that’s not a violation of DRY because the *knowledge represented* is different. (This ties to the earlier caution about coincidental duplication.) So, you might end up with `.LEG_COUNT <- 4` for dogs and `.DEFAULT_CLUSTERS <- 4` for clusters, and that’s fine – they’re separate truths.

### 2.3 Verify and Commit Constants Refactor

8.  **Run tests:** After replacing all targeted literals with constants, run the full test suite. Everything should pass exactly as before (since we haven’t changed any values).
    *   If a test fails now, it likely means we made a mistake in replacement or in scoping. Perhaps a constant wasn’t in scope where used. Remember that R will look for `.ALPHA_DEFAULT` in the package namespace. As long as it’s defined in an R file (and that file is sourced by `load_all()` or during package load), it should be found by functions. If you see an error like “object ‘.ALPHA\_DEFAULT’ not found,” it might mean:
        *   The constant file hasn’t been loaded before the function used it. This could happen if, for example, the constant is defined in a file that comes after the function file alphabetically and you didn’t rebuild. **Solution:** re-run `devtools::load_all()` or restart R to simulate package load order, and ensure no ordering issue. Usually, R sources files in alphabetical order (unless specified in Collate in DESCRIPTION). If needed, you can add that file to Collate or ensure its name alphabetically comes earlier (e.g., name it `zzz.R` or `00_constants.R` to load first). This way any function can use the constant. Check `.Rbuildignore` to ensure we didn't accidentally ignore that file – we shouldn’t have.
        *   Or we accidentally deleted one too many characters in a replacement.
    *   If tests pass, we’re good. If not, fix the issue (likely adding the constant definition earlier or adjusting scope) and test again.

9.  **Run R CMD check:** It’s wise to run `devtools::check()` at this point. In addition, **always run `R CMD build .` before `check`** on the produced tarball to ensure that build‑time collation, vignette compilation, and `.Rbuildignore` entries are correct:
    ```bash
    R CMD build .
    R CMD check econometrics_*.tar.gz --as-cran
    ```
    Potential things to watch:
    *   **Global‑variable NOTE fix:** If you call these constants inside data‑masking verbs (`dplyr::mutate()`, `data.table`, etc.) you may see a NOTE in `R CMD check` such as *“no visible binding for global variable '.ALPHA\_DEFAULT'”*. Add this once in `R/zzz.R`:
        ```r
        if (getRversion() >= "3.2.0") {
          utils::globalVariables(c(".ALPHA_DEFAULT", ".MAX_ITER"))
        }
        ```
        This suppresses false positives while keeping the constants internal.
    *   **Documentation**: Since we added a new R file, check might want a docs entry. But with `@keywords internal`, it should be fine. No user-facing documentation needed.
    *   Ensure no new WARNINGs or NOTEs beyond maybe the above. Ideally, zero.

10. **Commit the constant refactor:** On a new branch (let’s call it `refactor-constants-ssot`), commit the changes:
    ```bash
    git checkout -b refactor-constants-ssot
    git add R/constants.R R/* (and any modified files)
    git commit -m "Refactor: use single-source constants for duplicated values"
    ```
    The commit should include the new constants file and all replacements. Now merge this into `main` (after tests pass on CI if you push it):
    ```bash
    git checkout main
    git merge refactor-constants-ssot
    ```
    Keep the branch (`refactor-constants-ssot`) intact after merging.

    ✅ *Implementation tip:* After merging, consider running the test suite one more time on `main` to double-confirm nothing weird happened in merge (shouldn’t, since it was fast-forward likely). Also, look at the diff in GitHub or your Git GUI to ensure all intended changes went in.

11. **Celebrate this SSOT improvement:** Now all those magic numbers and repeated strings have one home! This is a small refactor with big benefits – if the significance level needs to change or a message text needs tweaking, you'll do it once instead of in many places. The code reads more clearly (“`.ALPHA_DEFAULT`” is more expressive than `0.05` out of context). Great work making the code more maintainable 👍.

*(You might not feel a dramatic change, but future maintainers will thank you when they don’t miss one instance of a value that needed updating.)*

With constants handled, we turn to the core of the refactoring: removing actual **code duplication** in functions – the heart of DRY coding.

## Phase 3: DRY Refactoring – Eliminating Duplicate Code Blocks

This phase addresses the duplicated logic identified by `dupree` (and any other spots we know of) by **extracting common code into single definitions**. The goal is that no substantial sequence of operations is copy-pasted in multiple places; instead, it's implemented once (in one function or helper) and reused. This is typically the most impactful part of the refactor.

We will proceed systematically, usually tackling one duplication at a time. Each duplication we fix will be verified by tests. By doing them one by one, we isolate any issues and keep changes small.

**Git branch strategy:** It’s recommended to isolate unrelated code duplications in separate branches for clarity. For example, if there are two entirely separate areas of duplication (say one in data loading functions, another in summary stats functions), you can do each on its own branch. That way, if one set is trickier or needs to be dropped, it won’t tangle with the other. On the other hand, if duplications are closely related or in the same set of files, you can do them in one branch together. Use your judgment. The rule of thumb: **one branch per logical refactoring group**. We’ll illustrate with one group at a time; rinse and repeat for others.

### 3.1 Extract and Refactor One Duplication at a Time

**General approach** for each duplication group (one or more code blocks that do the same thing):

1.  **Create a new branch for this refactor group:** e.g., `refactor-<feature>-dup` (replace `<feature>` with something descriptive like `model-fitting-dup` or `print-method-dup`). Switch to it.

2.  **Choose the abstraction type:** Usually, the best way to DRY out repeated R code is to create a **new function** (could be exported if it’s generally useful, but in our case likely an internal helper) that encapsulates the duplicated logic. In some cases, if the duplication is very small, you might decide not to abstract it (e.g., two functions both do `cat("Done\n")` at end — trivial duplication, might leave it or maybe a `message_done()` helper if you want). But for any substantial duplication, a function is the way to go. Occasionally, other abstractions like macros (for C code) or using an existing common function from base R or a package can help. But writing a new R function is simplest.

3.  **Draft the new function:** Open a new R script (or find a suitable existing file like `R/utils.R` or `R/internal_helpers.R`). Write a function that performs the duplicated task. For instance, if you found that multiple functions have a block that filters a data frame and computes a certain statistic, you might create:
    ```r
    .compute_stat <- function(data, var, threshold) {
        # Performs the filtering and statistic calculation that was duplicated
        filtered <- data[data[[var]] > threshold, ]
        result <- mean(filtered[[var]])
        return(result)
    }
    ```
    This is just an illustrative example. In writing the function, consider:

    *   **Parameters:** What inputs does the code block need? Likely they were local variables in each original place. Those become parameters.
    *   **Return value:** What output does that block produce or what effect (if any)? If it was producing a value or modifying something, the function should return the value (and we’ll use the return in the caller). If it was, say, a chunk that prints a message, maybe the new function just handles the printing.
    *   **Name:** Give it a meaningful name, prefixed with `.` if internal. Since we are not adding new user-facing features, prefer to keep it internal (`@noRd` in docs or no export).
    *   **No side-effects (if possible):** Ideally, the function should not rely on or modify global state. Use arguments and returns to make it self-contained. This mirrors what the duplicated code did, but it's now explicit.
    *   **Documentation/Comment:** Add a brief comment or roxygen block (marked internal) describing what it does, so future devs know why it exists. E.g., `#' @description Internal helper that [does X]. Used by functions A, B, C to avoid code duplication.` (you can do it with `@keywords internal`.)
    *   **Handling Non-Standard Evaluation (NSE):** If the duplicated code relies on NSE (e.g., `dplyr` verbs with bare column names), it **cannot** be extracted into a standard function without special care. You must use the `rlang` tidyevaluation framework to pass column names and expressions correctly. For example, a function parameter that accepts a column name from the user should be embraced with the `{{...}}` (curly-curly) operator inside the function body. This is a non-trivial change; consult the *Tidy-evaluation* chapter in *Advanced R* or seek help if unsure.

     *Example of a correct tidyeval-aware helper:*
      ```r
         .filter_by_var <- function(data, var_name, value) {
            # This function correctly accepts a bare column name for `var_name`
            dplyr::filter(data, {{ var_name }} == .env$value)
      }
      ```

4.  **Replace code in original spots:** Now go to each location where the code was duplicated:

    *   Remove or comment out the old code block, and in its place call your new function with the appropriate arguments/assignments.
    *   Be careful to pass the correct variables. For example, if the original code was using a variable `df` and `col` from that function’s environment, you call `.compute_stat(df, col, threshold=5)` or whatever is needed.
    *   If the original code assigned to some variable, make sure to assign the function’s result to that same variable. E.g., original had `stat <- mean(filtered[[var]])`; now you do `stat <- .compute_stat(data, var, threshold)`.
    *   If the original code had slight variations, you might need to accommodate that:
        *   Sometimes two code blocks are mostly the same but one has an extra step or a different constant. You can handle this by adding a parameter to the new function to control that behavior, or by writing the function to handle both cases (maybe an `if` inside based on a parameter). For example, if one function’s version always drops NA and another doesn’t, you could give `.compute_stat` a boolean parameter `na.rm` and set it accordingly for each call.
        *   If differences are too significant, consider whether refactoring them together is wise. You might decide to only refactor the common part and leave the rest separate.
    *   Remove any code that is now handled inside the new function to avoid duplication. The diff should show a net reduction in lines across the codebase, typically.

*   **Be Wary of Over-Complication:** As warned in the Ultrathink phase, if handling the variations between code blocks requires adding multiple new parameters (especially boolean flags like `do_this_special_thing = TRUE`) to your new helper function, **stop and reconsider**. A complex helper function can be worse than the duplication it replaces. It may be better to abstract only the 100% common logic or to leave the code blocks separate if the differences are too significant.

5.  **Run tests for this change:** After refactoring one group of duplication, run the tests (`devtools::test()` for speed, or `check()` if you prefer). Possible outcomes:

    *   **All tests still pass:** Great! This means your new function behaves exactly like the old inline code did in all cases. You successfully preserved behavior. You can commit this change.
    *   **Some tests fail:** Investigate quickly:
        *   If the failing tests are related to the functions you changed, likely the new abstraction introduced a subtle change. For example, maybe an environment issue – perhaps the new function doesn’t carry over something like the parent environment needed for evaluation (common if you moved code that uses `eval()` or non-standard evaluation). Or you forgot to return a value, etc.
        *   Use the test error messages to pinpoint where the difference is. Then inspect your new function vs old code to identify the discrepancy.
        *   **Debugging tip:** You can use `devtools::load_all()` to load your modified package and then interactively call the functions that failed to see what they return now vs what they returned before (you can temporarily check out the main branch to run the original code for comparison, or consult expected values from tests). Adding some `message()` in the new function to ensure it was called, etc., can help too.
        *   Once you see the issue, fix the new function or the calls. For example, maybe you realized one of the original functions used a global variable inside the block that you didn’t pass in. The test fails because that variable isn’t found in the new function (since it’s out of scope). Solution: pass it as a parameter to the new function.
        *   Another example: maybe performance differences cause a timing test to fail (less likely, but if the test had a time assumption, though unusual in unit tests).
        *   After fixing, run tests again. Repeat until green.
    *   **No test for that code path:** If by chance the duplicated code wasn’t fully covered by tests (though coverage is \~100%, assume it is covered), you might not immediately know if you broke something. Given high coverage, we assume tests will catch issues. But if you suspect coverage gap, you could manually test the affected functions with some typical inputs to reassure yourself the outputs match pre-refactor outputs (perhaps using saved results or known correct values).

6.  **Commit the refactoring of that group:** Once tests pass, commit the changes in this branch. E.g., “Refactor: extract common code for XYZ into .compute\_stat()”. Keep commits focused; if you do multiple groups in one branch, you might still want separate commits per group for clarity.

7.  **Repeat for other duplication groups:** If there are multiple unrelated duplication sections to handle, you have a choice:

    *   **Option A:** Continue on the same branch sequentially for convenience (especially if they're small and you're confident). Just make sure to commit between them.
    *   **Option B:** Checkout main again, make a new branch for the next group, and do them independently. This can make code review easier and avoids intermixing changes. The downside is if they touch nearby code, you might get merge conflicts later, but those can be resolved.

    Using separate branches is cleaner as per our workflow rules, so prefer that unless the duplications are in the exact same function/file (in which case doing them together might be simpler).

9.  **Maintain the checks rigor:** After each major extraction, run all tests. After finishing all targeted duplications, run a full `devtools::check()` again. Linting might complain if, say, the new function isn’t used (shouldn’t happen, we use it) or if there’s now an unused argument somewhere (maybe in the original function if some argument became unnecessary after refactor). Remove or adjust as needed (but careful: removing a function argument changes its interface – avoid doing that unless it was truly internal and unexported; if exported, don’t remove it even if now unused, as that changes API. Instead, perhaps leave it and mark it in docs that it's unused, or better, see if you can utilize it in the refactored call).

10. **Merge each refactor branch into main:** Once a branch’s changes are complete and all checks pass, merge it to `main` (don’t forget to **not delete the branch**). If you did them sequentially, you might merge after each, or do a final merge if you stacked them. Sequential merging is easier (less conflicts).

    If merging multiple branches that touched nearby code, you might have to resolve conflicts. Typical conflicts might occur if two refactors edited the same function. Resolve by keeping both sets of changes (since they should be orthogonal improvements). Run tests again after merging to be sure.

11. **Major milestone reached!** At this point, all the significant duplicate code segments should be refactored. The package’s core code is now **DRY**: each piece of logic is implemented in one place. This is a huge improvement for maintainability. 🎉

    Take a moment to celebrate: you likely reduced the code size and definitely reduced the cognitive load for future readers. If a bug is found in that logic, it can be fixed in one spot now, and all uses will get the fix. Excellent work!

    *Encouragement:* This was the most critical phase, and you aced it. It might have been challenging to ensure nothing changed behavior, but thanks to the tests, you had guidance. Give yourself a pat on the back for significantly improving the code quality without any user noticing a thing (which is exactly how a good refactor should be 😄).

### 3.2 Post-Refactoring Checks and Polishing

12. **Rerun duplication analysis:** Now is a good time to run `dupree::dupree_package(".")` again (and perhaps compare with the earlier `duplication_report.csv`). Ideally, you should see that those high-similarity code blocks are gone or much reduced in similarity score. If some remain and you consciously left them (because they were coincidental or too risky to refactor), that’s okay. But confirm that nothing obvious was missed. If you spot another easy one, you can still quickly address it in a new mini-branch.

13. **Check coverage:** Run `covr::package_coverage()` to ensure test coverage remains high. If the new internal functions you wrote are not being exercised by tests, consider adding some tests for them (or relying on indirect tests via the original functions). Ideally, because you replaced code with the function, those lines are still being run when the original functionality is tested. The overall coverage percentage might remain similar or even improve (since duplicate code removal reduces total lines). If coverage dropped slightly because maybe some branch in the new function isn’t hit, you could add a targeted test if warranted. (But if it's an internal nuance not easily testable from outside, it might be okay given everything else is tested.)

14. **Ensure no functionality change:** This is mostly done via tests, but it can be nice to do a quick manual run of examples:

    *   Try a few key exported functions of the package manually (especially those that we refactored internally) with typical inputs and see if outputs match what they were before. Since we trust tests, this isn’t strictly needed, but it might give peace of mind.
    *   Check that any vignettes or examples still work (they should if tests pass, but if vignettes had something timing-related or any subtle usage, rebuild them via `devtools::build_vignettes()` to be sure no errors).

At this point, the code logic is DRY and SSOT for core computations. We will now address duplication in documentation (so that we maintain SSOT in our communications about the code) and then in tests.

## Phase 4: DRY in Documentation – Single Source of Truth for Docs

Even documentation can suffer from copy-paste. Since our code and behavior didn’t change, the user-facing docs (function help files, vignettes) should remain correct – we are not rewriting documentation entirely, just refactoring how it’s maintained.

Goals in this phase:

*   Avoid having the same descriptive text in multiple places. For example, if multiple functions have the same parameter (say `data` or `formula`) with the same meaning, we should document that parameter once and reuse that documentation in each function.
*   Ensure any important details (like formulas or references) appear in a single place. If they need to be in multiple help files, use inclusion or referencing rather than duplicating text.
*   Keep documentation accurate and up-to-date with minimal effort in the future (SSOT principle: one place to change if needed).

Roxygen2 provides directives to help with this:

*   `@inheritParams` to copy parameter documentation from another function.
*   `@inherit` (with specifics like `@inherit SectionName`) to copy whole sections from another topic.
*   Roxygen **templates** (`@template` and `@templateVar`) for reusing blocks of text across docs.

We will use these tools to eliminate duplicated documentation.

### 4.1 Identify Documentation Duplication

1.  **Scan function documentation:** Open the `.Rd` files in `man/` or the roxygen comments in `R/` files. Look for instances of repeated text:
    *   Common phrases in multiple docs. E.g., “This function computes the OLS regression...” might appear in two places if you have similar functions.
    *   Parameter descriptions: The classic case – many functions have a `data` argument described as “A data frame containing the variables.” You shouldn’t have to write that 10 times; instead, inherit it.
    *   Return value sections that are identical for similar functions (e.g., if several functions return a lm object or a tibble, and you describe it the same way each time).
    *   Examples or references repeated (rare, but maybe two functions have the same example usage block? If so, maybe they should be consolidated or one documented via the other).
    *   **Vignette vs manual overlap:** Sometimes vignettes repeat some content from function docs (like an explanation of a concept). This is harder to de-duplicate automatically because vignettes are free-form. But just note if anything stands out.

2.  **Plan doc refactors by type:**
    *   For parameters that recur: we'll use `@inheritParams`.
    *   For entire sections or details: possibly use `@inherit`.
    *   For repeated snippets that don't fit well with inherit (maybe two completely separate topics share some text), consider a roxygen template. For instance, if multiple functions have an **identical Note** or **identical See Also**, you can create a file `man-roxygen/mytemplate.R` with the content and include it via `@template mytemplate` in each roxygen block.
    *   Check the NAMESPACE: no changes needed for docs specifically, but if we want to use `@inherit` from an internal function, note that roxygen can only inherit from documented topics. If needed, you might temporarily document one function fully and have others inherit from it.

3.  **Ensure accuracy:** As you refactor docs, double-check that each parameter or detail you inherit is truly applicable to the other function. Sometimes a parameter with the same name might have a slightly different meaning in another function – don’t blindly inherit in that case. Only do it when it’s the same concept.

### 4.2 Apply Documentation Refactoring

For each documentation duplication identified:

4.  **Choose a source for `@inheritParams`:** If multiple functions share a param `foo` with identical meaning, pick one function to be the “source” of documentation for `foo`. Often this could be the first function you documented or the most detailed one. Make sure that function’s roxygen has a full description for `foo`.
    *   In the other functions, remove the repetitive description of `foo` and replace it with `@inheritParams function_name` (where `function_name` is the source function). This will pull in the param docs for all parameters that have the same names. If you want to be precise, you can do `@inheritParams function_name ref=foo` to only inherit that one parameter’s doc (depending on roxygen version).
    *   Example: Suppose functions `regress()` and `plot_regression()` both have param `data`. You can document `data` in `regress()` as “Data frame containing the variables…”. In `plot_regression()` roxygen, just put `@inheritParams regress`. Now `plot_regression()`’s docs will include whatever `regress` says about `data` (and any other common params).
    *   After doing this, run `devtools::document()` to regenerate Rd files, and inspect the output `.Rd` to ensure it looks correct (the inherited text should appear).
    *   Repeat for other params: e.g., formula, subset, etc., any common ones.

5.  **Deduplicate return sections or details:** If you find entire sections duplicated:
    *   One way is `@inherit function_name` which by default inherits title, description, and some other pieces. But you often don't want to inherit everything (each function needs its own title/description). Instead, roxygen allows `@inheritSection function_name SectionName` if you label sections.
    *   If, for example, multiple functions have an **Identical** Details section explaining a statistical concept, you could:
        *   In one function’s roxygen, wrap that part in a section:
            ```r
            #' @section OLS details:
            #' (Then your explanation of OLS here)
            ```
            This creates a section "OLS details" in that Rd.
        *   In another function, instead of writing the same, you do:
            ```r
            #' @inheritSection regress OLS details
            ```
            and that will copy the content.
    *   Alternatively, use templates: Create `man-roxygen/ols_details.R` with the content, then each function does `@template ols_details`. This might be simpler if multiple files need it.
    *   For small packages, usually param inheritance is the main need. Use section or templates for anything larger.

6.  **Update vignettes or README if needed:** If you identified duplication between vignettes and docs that is problematic (e.g., you have some example in both that you want to ensure stays consistent), consider using `knitr::read_chunk` or linking to documentation. However, usually vignettes are more narrative, so it's okay if they overlap conceptually. We generally won’t try to remove that kind of duplication, because it serves different purposes (one is tutorial, one is reference). Focus on reference manual consistency.

7.  **Run Documentation checks:** After applying the above:
    *   Run `devtools::document()` to rebuild all documentation. Check for warnings in the process (roxygen will warn if it can’t inherit something, e.g., name mismatch).
    *   Build the package and then open some help files (`?functionName`) to see that everything still reads well. Ensure no sections got dropped inadvertently.
    *   Run `R CMD check` or `devtools::check()`. Pay attention to:
        *   **Help file completeness:** no missing descriptions for parameters (roxygen inheritance should fill them, but check for any `\value{}` sections empty or `@param` missing).
        *   **Non-standard things:** If you used templates, check that the `man-roxygen` folder is listed in `.Rbuildignore` (it usually is by default with usethis).
        *   No warnings about unknown tags.

    Tests are unlikely to be affected by doc changes (unless you had snapshot tests of docs, which is rare). So tests should still pass. If any test was checking, say, the length of an error message that included a parameter name, irrelevant here.

8.  **Commit documentation refactor:** Make a branch `refactor-docs-dry` for these changes. Commit with message like “Docs: use inherit to avoid duplicate parameter descriptions”. The diff will show removals of redundant text and additions of inherit tags. That’s great for reviewers to see; it’s clear we didn’t change meaning, just references.

    Merge this into main when ready (again, keeping the branch). Documentation changes might not need a separate branch from the code changes branches (they don’t conflict logically), but separating them is nice for review.

9.  **Enjoy easier docs maintenance:** Now, if you need to change the description of that `data` parameter, you edit it in one place and all functions will get the update. This reduces the chance of inconsistent docs (which is a common issue in multi-function packages). Good job on enforcing SSOT in documentation! 🎉

    Small celebration: The reference documentation is now as DRY as the code. This is often overlooked, so give yourself credit for doing it. Future you (or anyone writing docs) will be grateful that they only have to fix typos or update explanations in one spot instead of many.

## Phase 5: Cleanup and DRY in Tests

Our final phase is addressing any duplication in the test suite and other ancillary parts. Since tests don’t affect users, this is slightly lower priority, but it’s still worthwhile if the tests have a lot of repetition. DRY in tests means:

*   Easier to update tests when behavior changes (one place to change setup or expected values if applicable).
*   Shorter, more readable test code by removing boilerplate.

However, we must balance this with test clarity. Sometimes duplicating a bit of code in tests is fine if it makes each test self-contained and understandable. We don’t want to abstract tests so much that a new contributor can’t tell what’s being tested.

We will do some light refactoring of tests focusing on obvious wins:

*   Common setup code factored into a helper or `setup()` file.
*   Using loops or parameterized tests for very repetitive structures.

### 5.1 Identify Test Duplications

1.  **Scan tests for repetition:** Look at the `tests/testthat/` directory:
    *   Do multiple test files create the same kind of object or data frame at the top? e.g., maybe several test files construct a particular model object or use the same example dataset. If yes, that dataset or construction code can be in one place.
    *   Are there large blocks of very similar tests? e.g., the same expectation repeated with only minor differences in input. Those could potentially be combined using a loop or `testthat::expect_equal` inside a loop, or `testthat::with_parameters_test_that` (if using the newest testthat features).
    *   Check for copy-pasted expectation logic. For example, if testing a property of a result requires the same calculations repeated in multiple tests, maybe factor that calculation into a small function in the tests.
    *   If certain literal values or messages are repeated across tests (like the same error message string in many tests), you could define a constant in a test helper file for it (similar to what we did for code, but it's less crucial).

2.  **Plan which to refactor:** Focus on test duplication that, if not addressed, could cause maintenance pain. For instance, if a certain model output format changes slightly, and you have 10 tests that check `attr(obj, "something") == "X"`, you’d have to update 10 places. That’s a candidate to unify. If tests are mostly distinct, maybe no need to change. **Don’t refactor tests just for the sake of it if it doesn’t actually reduce meaningful duplication or if it makes tests harder to follow.** Remember, tests are also documentation for behavior.

### 5.2 Refactor Test Duplications

3.  **Use `testthat` helpers for setup:** If there is common setup for many tests, consider `tests/testthat/setup.R` file:
    *   In `setup.R`, you can create objects or set options that will be available to all tests. For example, if many tests use a particular small dataset, you can load it once in `setup.R` into a variable. All test files can then use that variable. This avoids re-reading or defining it repeatedly.
    *   Alternatively, you can create helper functions in a file like `tests/testthat/helper-utils.R`. For instance, define `expect_valid_model <- function(mod) { ... }` that runs a series of expectations on a model object (if many tests do the same checks on different models).
    *   Also consider `teardown.R` if needed (to clean up after tests, not often needed here).

4.  **Parametrize repetitive tests:** If you have a pattern like:
    ```r
    test_that("function works for case A", {
      result <- f(x1)
      expect_equal(result, y1)
    })
    test_that("function works for case B", {
      result <- f(x2)
      expect_equal(result, y2)
    })
    ...
    ```
    This can be simplified. Options:
    *   Use a loop inside one `test_that`:
        ```r
        test_that("function works for multiple cases", {
          cases <- list(
            list(input = x1, expected = y1),
            list(input = x2, expected = y2),
            list(input = x3, expected = y3)
          )
          for (case in cases) {
            expect_equal(f(case$input), case$expected)
          }
        })
        ```
        This will still run all expectations and if one fails, it will report which iteration (the error message will show the failing `expect_equal`). It’s slightly less granular (one test block instead of separate ones), but it avoids repeating code.
    *   Use `testthat::with_parameters_test_that()` (if using testthat 3.0+). This function allows you to supply a list of parameters and it generates multiple `test_that` blocks programmatically. It retains separate test reporting for each set of parameters. This is neat but a bit advanced. E.g.:
        ```r
        with_parameters_test_that("f(%s) returns correct result",
          .cases = list(
            A = list(input = x1, expected = y1),
            B = list(input = x2, expected = y2)
          ), {
            expect_equal(f(input), expected)
          }
        )
        ```
        This will generate two tests named "f(A) returns correct result" and "f(B) returns correct result". Use this if you’re comfortable; otherwise, a simple loop is fine.
    *   Only do this when it truly reduces a lot of duplication and the test logic is identical. If tests differ in more than one or two values, it might be clearer to keep them separate.

5.  **Remove redundant or obsolete tests:** Occasionally you find that some tests are effectively duplicates of each other (testing the same scenario). If you find any, you might remove one or merge them. However, be cautious: more tests usually don’t hurt (aside from maintenance), but exact duplicates are unnecessary.

6.  **Run tests frequently:** After each tweak in tests, run them. It’s easy to make a mistake in test refactoring (like scoping issues in helper functions, or a loop not actually iterating what you think). Running tests ensures your refactored tests still fail when they should and pass when they should.
    *   If you introduced a helper or setup, verify it’s working. For instance, try deliberately making a test fail by changing expected value, just to see that the output makes sense (then undo).
    *   If using `with_parameters_test_that`, verify the test names and count look correct in the output.

7.  **Ensure coverage remains \~100%:** Test refactoring shouldn’t reduce coverage of the *code*, but if you accidentally removed a test case thinking it was duplicate when it wasn’t, you could lose some coverage. Use `covr` again if needed to be sure. Or simply review that all critical branches are still being tested. The aim is not to change what we test, only how we organize tests.

8.  **Commit test changes:** On branch `refactor-tests-dry`, commit the adjusted tests. E.g., “Tests: factor out common setup and parametrize duplicate checks”. Merging this in will not affect any production code, but it’s part of the refactor deliverables (and should also pass CI). Keep the branch around after merge.

9.  **Final test and check run:** Run `devtools::check()` one last time on main with all changes (code + docs + tests). Everything should be green. No warnings, all tests pass, coverage high, linters happy.
    -   Final Documentation Check: Rebuild the package website with pkgdown::build_site() to ensure all inherited documentation, cross-references, and new helper function documentation render correctly without warnings.

10. **Delete `temp-refactor/` artifacts:** If you still have the `temp-refactor/` folder with reports or scripts and you don’t need them anymore, you can remove them. Since it was build-ignored, it doesn't affect anything, but for neatness you might clean it up. You could keep the duplication report CSV in version control history if useful, but having it in the repo long-term isn’t necessary. It's up to you; sometimes it's nice to keep it for project history. Given it's in a separate folder, it’s okay to delete it now. If you do, remove the line from `.Rbuildignore` as well. This can be its own commit “Chore: remove temp-refactor files”.

    (This is effectively housekeeping; the plan said the folder must be deletable at the end without affecting anything – which we have ensured by not using it in code. Deleting it now confirms we truly don’t need it.)

> 🎉 **Final milestone:** Refactoring complete! The package’s code, tests, and documentation are now DRY and follow Single Source of Truth principles everywhere possible. You’ve maintained 100% functionality parity – users will not notice any difference except perhaps subtle improvements (like maybe messages fixed by flir, if any). Meanwhile, the maintainers (you and your team) will notice that the codebase is leaner and cleaner. Each logical component is defined in one place, making future changes or bug fixes much easier.

## Conclusion and Next Steps

By following this plan, we performed a careful, CI-driven refactor that **never compromised on correctness or quality**. Every step was validated by the comprehensive test suite and checks. There were no shortcuts – no tests were skipped or silenced, no linters turned off. This ensures our refactoring did not introduce regressions. We also used a methodical git workflow, so each set of changes is isolated and traceable, which is ideal for code review and for bisecting in the unlikely event an issue is discovered later.

**What we achieved:**

*   **DRY Code:** We eliminated duplicate code segments by refactoring them into unified functions (or constants). According to the DRY principle, “every piece of knowledge *now* has a single, unambiguous, authoritative representation” in the code. If a formula or algorithm needs to change, we change it in one place, confident it updates everywhere.

*   **SSOT Configuration:** Important constant values and messages are defined once. The risk of inconsistent values (e.g., one function using 0.95 and another 0.90 by accident) is gone. The code clearly signals that these are intended to be the same across the package.

*   **Consistent Documentation:** Through roxygen inheritance, our documentation is both consistent and easier to maintain. The description for a common parameter or return value lives in one place. This prevents the situation where a doc gets updated in one function but not another, confusing users. Now, edits propagate to all relevant docs automatically.

*   **Streamlined Tests:** The test suite runs as before (all tests pass), but with less repetition. Common setup is done once. New test cases can be added with minimal boilerplate. This makes writing tests less tedious and reduces the chance of error in tests themselves. Importantly, we kept tests understandable – anyone reading them will still see what each test is doing, just without drowning in repetitive code.

*   **No regressions introduced:** Thanks to continuous testing, we maintained software correctness. This is supported by Fowler’s refactoring mantra: after each small refactor, the system is kept fully working. We followed that strictly.

*   **Robust Git history:** We have a sequence of commits (and branches) documenting each change. If a bug is reported in the future, we can quickly pinpoint which refactoring (if any) might be involved by reviewing these commits. Non-main branches remain in case we need to revisit any decision (for example, if we temporarily decided not to refactor something, the branch will show that context).

*   **No user-visible changes:** Throughout this process, no user-facing API or behavior has been changed – we’ve merely removed duplication and improved organization. The package should produce the same results as before, but the internals are now cleaner and adhere to DRY/SSOT principles (each piece of knowledge or logic is defined in one place)

**Tips for going forward:**

*   Continue the practice of **ultra-thinking** before making non-trivial changes. Now that the codebase is DRY/SSOT, any new duplication introduced should be viewed critically. If adding a new feature, plan its design so it doesn’t duplicate existing logic. If you find yourself copying code, consider refactoring or extending the abstraction instead.

*   Maintain high test coverage as a safety net for future refactors. Now that the code is cleaner, future modifications will be easier – but always write tests for new features and run the full check on any change.

*   Celebrate this achievement with the team! This kind of thorough refactoring in a well-tested environment is a textbook example of improving internal quality without user impact. It’s something to be proud of.

Finally, a big **congratulations** 🎉 on completing this important project. The package is now in a state of high internal quality, setting it up for a long and maintainable future.
