#' Check if REndo is available
#' @export
has_rendo <- function() {
  requireNamespace("REndo", quietly = TRUE)
}

#' Check if curl is available
#' @export
has_curl <- function() {
  requireNamespace("curl", quietly = TRUE)
}

#' Check if haven is available
#' @export
has_haven <- function() {
  requireNamespace("haven", quietly = TRUE)
}

#' Check if RStata is available
#' @export
has_rstata <- function() {
  requireNamespace("RStata", quietly = TRUE)
}

#' Check if Stata is available via RStata
#' @export
has_stata <- function() {
  if (!has_rstata()) return(FALSE)
  
  # Check common Stata executables
  stata_paths <- c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata")
  any(nzchar(Sys.which(stata_paths)))
}