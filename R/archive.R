#' Move a Package to the Repository's Archive
#'
#' @param pkg The name of the package to archive, e.g. \code{"cranium"}.
#' @param version The package version to archive.
#' @param repo The location of the package repository.
#' @param type The type of the package. Only \code{"source"} is supported.
#' @param keep When \code{TRUE}, do not remove the package from the repository
#'   after it has been archived.
#'
#' @export
archive_pkg <- function(pkg, version, repo = get_repo_location(),
                        type = "source", keep = TRUE) {
  if (type != "source") {
    stop("Only 'source' type packages can be archived at present.")
  }

  # TODO: Check that the package is in the repo.
  # TODO: Check that the package name and version are valid.

  contrib_url <- contrib.url(repo, type = type)
  pkg_file <- sprintf("%s_%s.tar.gz", pkg, version)

  current_path <- file.path(contrib_url, pkg_file)
  archive_dir <- file.path(contrib_url, "Archive", pkg)
  archived_path <- file.path(archive_dir, pkg_file)

  # This should only happen once per package.
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }

  if (file.exists(archived_path)) {
    stop("An archived version of this package already exists.")
  }

  if (!file.exists(current_path)) {
    stop("No package present to archive.")
  }

  if (!file.copy(current_path, archived_path, copy.date = TRUE)) {
    stop("Failed to copy package to the archive.")
  }

  if (!keep && !file.remove(current_path)) {
    stop("Failed to remove existing package.")
  }

  # TODO: Regenerate the package index.

  invisible(NULL)
}
