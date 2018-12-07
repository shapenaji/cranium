#!/usr/local/bin/Rscript

# Configure the repository using environment variables.

repo_name <- Sys.getenv("CRANIUM_REPO_NAME")
use_archive <- Sys.getenv("CRANIUM_USE_ARCHIVE")
use_hardlinks <- Sys.getenv("CRANIUM_USE_HARDLINKS")
latest_only <- Sys.getenv("CRANIUM_LATEST_ONLY")
fields <- Sys.getenv("CRANIUM_REPO_FIELDS")

if (any(nchar(c(repo_name, use_archive, use_hardlinks, fields)) == 0)) {
  cat("Error: One or more CRANIUM environment variables is unset.",
      file = stderr())
  quit(status = 1)
}

# Convert to R types.
use_archive <- use_archive == "true"
use_hardlinks <- use_hardlinks == "true"
latest_only <- latest_only == "true"
fields <- strsplit(fields, ",", fixed = TRUE)[[1]]

# Run the cranium server.

cranium::serve(
  repo = "/var/lib/cranium", repo_name = repo_name,
  host = '0.0.0.0', port = 8000,
  fields = fields, use_archive = use_archive, use_hardlinks = use_hardlinks
)
