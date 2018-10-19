#' Move Packages to the Repository's Archive
#'
#' @param pkg A vector of package names to archive, e.g. \code{"cranium"}.
#' @param version A vector of valid package versions to archive.
#' @param repo The location of the package repository.
#' @param type The type of the package. Only \code{"source"} is supported.
#' @param keep When \code{TRUE}, do not remove the package from the repository
#'   after it has been archived.
#' @param overwrite When \code{TRUE}, overwrite existing archived packages, if
#'   present.
#'
#' @return A logical vector indicating which packages were moved to the archive
#'   successfully.
#'
#' @export
archive_pkg <- function(pkg, version, repo = get_repo_location(),
                        type = "source", keep = TRUE, overwrite = FALSE) {
  stopifnot(length(pkg) == length(version))
  if (type != "source") {
    stop("Only 'source' type packages can be archived at present.")
  }

  contrib_url <- contrib.url(repo, type = type)
  pkg_files <- paste0(pkg, "_", version, ".tar.gz")

  current_paths <- file.path(contrib_url, pkg_files)

  archive_dirs <- file.path(contrib_url, "Archive", pkg)
  archived_paths <- file.path(archive_dirs, pkg_files)

  # Drop duplicates, just in case they were passed accidentally. Otherwise we
  # will get fairly obscure errors.
  current_paths <- current_paths[!duplicated(current_paths)]
  archive_dirs <- archive_dirs[!duplicated(archive_dirs)]
  archived_paths <- archived_paths[!duplicated(archived_paths)]

  if (!all(file.exists(current_paths))) {
    stop("Not all packages to archive are present.")
  }

  # This should only happen once per package.
  missing_dirs <- archive_dirs[!dir.exists(archive_dirs)]
  for (dir in missing_dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # TODO: Should this be an error? Or can we rely on the user checking the
  # returned value to see if things have been archived?
  # if (!overwrite && any(file.exists(archived_paths))) {
  #   stop("Archived versions of some packages already exist.")
  # }

  copied <- file.copy(current_paths, archived_paths, overwrite = overwrite)

  # NOTE: Sys.setFileTime is not vectorized (3.4.4), so we work around that.
  # When this changes in the future, add copy.date = TRUE above.
  fi <- file.info(current_paths[copied], extra_cols = FALSE)
  for (i in seq_along(current_paths[copied])) {
    Sys.setFileTime(archived_paths[copied][i], fi$mtime[i])
  }

  # TODO: Do we need this error? Best reason to fail here is to prevent
  # anything from being deleted if the copy failed.
  if (overwrite && !all(copied)) {
    stop("Failed to copy ", sum(copied == FALSE), " packages to the archive.")
  }

  if (!keep) {
    removed <- file.remove(current_paths)
    if (!all(removed)) {
      stop("Failed to remove all existing packages.")
    }
    # TODO: Regenerate the package index.
    warning("The repository index has not been updated, and may be ",
            "inconsistent with available packages.")
  }

  copied
}
