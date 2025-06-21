# Check and install required packages
packages <- c("dupree", "styler", "lintr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg)
  }
}

# Display versions
message("Package versions:")
message("dupree: ", packageVersion("dupree"))
message("styler: ", packageVersion("styler"))
message("lintr: ", packageVersion("lintr"))
