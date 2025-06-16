#' Internal string constants
#' @keywords internal
.hetid_strings <- function() {
  list(
    # Degrees of freedom adjustment methods
    df_adjust = list(
      ASYMPTOTIC = "asymptotic",
      FINITE = "finite"
    ),

    # Column names
    columns = list(
      STD_ERROR = "Std. Error",
      Y1 = "Y1",
      Y2 = "Y2",
      XK = "Xk",
      Z = "Z",
      X1 = "X1",
      X2 = "X2",
      Z1 = "Z1",
      Z2 = "Z2",
      # Remapped names
      Y_MAPPED = "y",
      P_MAPPED = "P",
      X1_MAPPED = "X1"
    ),

    # Variable names
    variables = list(
      DEFAULT_ENDOG_VAR = "Y2",
      DEFAULT_EXOG_VARS = "Xk"
    ),

    # Stata integration
    stata = list(
      PACKAGES = c("ranktest", "ivreg2", "ivreg2h"),
      EXECUTABLES_ALL = c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata"),
      EXECUTABLES_UNIX = c("stata", "stata-mp", "stata-se"),
      DO_EXTENSION = ".do",
      LOG_EXTENSION = ".log",
      MAC_PATHS = c(
        "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
        "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP",
        "/Applications/Stata/Stata.app/Contents/MacOS/Stata"
      )
    ),

    # Visualization
    plot_labels = list(
      OLS_BIASED = "OLS (Biased)",
      TSLS_LEWBEL = "2SLS (Lewbel)",
      OLS_COLUMN = "ols_gamma1",
      TSLS_COLUMN = "tsls_gamma1"
    ),

    # Plot colors (hex codes)
    plot_colors = list(
      OLS_COLOR = "#d95f02",
      TSLS_COLOR = "#1b9e77"
    ),

    # Environment variables
    env_vars = list(
      VERBOSE = "VERBOSE",
      VERBOSE_TRUE = "TRUE",
      VERBOSE_FALSE = "FALSE"
    )
  )
}
