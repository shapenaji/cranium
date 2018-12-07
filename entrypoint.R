#!/usr/local/bin/Rscript

# Configure the repository using environment variables.

repo_name <- Sys.getenv("CRANIUM_REPO_NAME")
use_archive <- Sys.getenv("CRANIUM_USE_ARCHIVE")
use_hardlinks <- Sys.getenv("CRANIUM_USE_HARDLINKS")
latest_only <- Sys.getenv("CRANIUM_LATEST_ONLY")
fields <- Sys.getenv("CRANIUM_REPO_FIELDS")
host_addr <- Sys.getenv("CRANIUM_HOST_ADDRESS", "0.0.0.0")
host_port <- Sys.getenv("CRANIUM_PORT", 8000)
repo_path <- Sys.getenv("CRANIUM_REPO_LOCATION", "/var/lib/cranium")

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
  repo = repo_path, 
  repo_name = repo_name,
  host = host_addr, 
  port = host_port,
  fields = fields, use_archive = use_archive, use_hardlinks = use_hardlinks
)
