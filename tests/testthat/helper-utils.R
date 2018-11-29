#' Create a Dummy Package Repository
#'
#' @param bundles A list of package tarballs to copy into the repository.
#'
#' @return The name of the directory containing the repository.
#'
#' @noRd
dummy_repo <- function(bundles, repo = tempfile("repo"), type = "source") {
  url <- contrib.url(repo, type = type)
  created <- dir.create(url, recursive = TRUE, showWarnings = FALSE)
  if (!created) {
    stop("Failed to create dummy repository directory: '", url, "'.")
  }
  copied <- file.copy(bundles, url)
  if (!all(copied)) {
    # Clean up in case of failure.
    unlink(repo, recursive = TRUE)
    stop("Failed to copy ", sum(copied), " packages to the dummy repository.")
  }
  repo
}

#' Check How Many Packages are Available from the Dummy Repository
#'
#' @return The number of packages.
#'
#' @noRd
available <- function(repo, type = "source") {
  nrow(utils::available.packages(repos = paste0("file://", repo), type = type))
}

# Set this to a real list of files to run the tests.
SAMPLE_BUNDLES <- character(0)

if (length(SAMPLE_BUNDLES) == 0) {
  stop("Point 'SAMPLE_BUNDLES' towards package tarballs to run the tests.")
}
