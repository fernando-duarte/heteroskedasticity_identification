#!/usr/bin/env Rscript

#' Pin GitHub Actions to Commit SHAs
#'
#' This script updates GitHub Actions workflow files to use commit SHAs
#' instead of tags or branches, preventing supply chain attacks.
#'
#' Usage: Rscript pin-github-actions.R [workflow_dir]
#'
#' Requires: gh CLI tool and GITHUB_TOKEN environment variable

library(jsonlite)

# Function to get the SHA for a given action and ref
get_action_sha <- function(action, ref) {
  # Parse the action string (e.g., "actions/checkout")
  parts <- strsplit(action, "/")[[1]]
  if (length(parts) != 2) {
    warning(paste("Invalid action format:", action))
    return(NULL)
  }

  owner <- parts[1]
  repo <- parts[2]

  # Use gh CLI to get the commit SHA
  cmd <- sprintf(
    paste0(
      "gh api repos/%s/%s/git/ref/tags/%s --jq '.object.sha' 2>/dev/null || ",
      "gh api repos/%s/%s/git/ref/heads/%s --jq '.object.sha' 2>/dev/null"
    ),
    owner, repo, ref, owner, repo, ref
  )

  sha <- system(cmd, intern = TRUE)

  if (length(sha) == 0 || sha == "") {
    warning(paste("Could not find SHA for", action, "ref", ref))
    return(NULL)
  }

  sha
}

# Function to update a workflow file
update_workflow_file <- function(file_path) {
  cat("Processing:", file_path, "\n")

  # Read the file
  lines <- readLines(file_path)
  modified <- FALSE

  # Pattern to match action uses
  action_pattern <- "^\\s*uses:\\s*([^@]+)@(.+)\\s*$"

  for (i in seq_along(lines)) {
    if (grepl(action_pattern, lines[i])) {
      matches <- regmatches(lines[i], regexec(action_pattern, lines[i]))[[1]]
      if (length(matches) == 3) {
        action <- matches[2]
        ref <- matches[3]

        # Skip if already using a SHA (40 characters)
        if (nchar(ref) == 40 && grepl("^[a-f0-9]+$", ref)) {
          next
        }

        # Skip r-lib actions (they manage their own security)
        if (startsWith(action, "r-lib/")) {
          cat("  Skipping r-lib action:", action, "\n")
          next
        }

        # Get the SHA
        sha <- get_action_sha(action, ref)

        if (!is.null(sha)) {
          # Update the line
          indent <- gsub("^(\\s*).*", "\\1", lines[i])
          new_line <- sprintf("%suses: %s@%s # %s", indent, action, sha, ref)
          lines[i] <- new_line
          modified <- TRUE
          cat("  Updated:", action, "from", ref, "to", sha, "\n")
        }
      }
    }
  }

  # Write back if modified
  if (modified) {
    writeLines(lines, file_path)
    cat("  ✓ File updated\n")
  } else {
    cat("  ✓ No changes needed\n")
  }
}

# Main function
main <- function() {
  # Check for gh CLI
  if (system("which gh > /dev/null 2>&1") != 0) {
    stop("gh CLI is required. Install from: https://cli.github.com/")
  }

  # Check for GITHUB_TOKEN
  if (Sys.getenv("GITHUB_TOKEN") == "") {
    stop("GITHUB_TOKEN environment variable is required")
  }

  # Get workflow directory
  args <- commandArgs(trailingOnly = TRUE)
  workflow_dir <- if (length(args) > 0) args[1] else ".github/workflows"

  if (!dir.exists(workflow_dir)) {
    stop("Workflow directory not found:", workflow_dir)
  }

  # Find all YAML files
  yaml_files <- list.files(
    workflow_dir,
    pattern = "\\.(yml|yaml)$",
    full.names = TRUE
  )

  if (length(yaml_files) == 0) {
    stop("No workflow files found in", workflow_dir)
  }

  cat("Found", length(yaml_files), "workflow files\n\n")

  # Process each file
  for (file in yaml_files) {
    update_workflow_file(file)
  }

  cat("\n✅ Done! Remember to review and test the changes before committing.\n")
}

# Run the script
if (!interactive()) {
  main()
}
