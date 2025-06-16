# Package startup and global variable declarations

# Suppress R CMD check notes about undefined global functions
# hetid_const is an internal function defined in options.R
utils::globalVariables("hetid_const")

# Constants environment initialization happens in constants.R
