# Package startup and global variable declarations

# Suppress R CMD check notes about undefined global functions
# hetid_const is an internal function defined in options.R
utils::globalVariables("hetid_const")

# Suppress R CMD check notes for internal functions and environments
if (getRversion() >= "3.2.0") {
  utils::globalVariables(c(
    # Internal functions
    ".hetid_const",
    ".hetid_strings",  # For backward compatibility
    # Internal environments
    ".hetid_constants"
  ))
}
