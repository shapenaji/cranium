#' Remove Packages from a Repository
#'
#' @param pkg A vector of package names to archive, e.g. \code{"cranium"}.
#' @param version A vector of valid package versions to archive.
#' @param repo The location of the package repository.
#' @param type The type of the package. Only \code{"source"} is supported.
#'
#' @return A logical vector indicating which packages were removed
#'   successfully.
#'
#' @export
remove_pkg <- function(pkg, version, repo = get_repo_location(),
                       type = "source") {
  stopifnot(length(pkg) == length(version))

  # Although this is likely a user error, let's be consistent with vector
  # inputs.
  if (length(pkg) == 0) {
    return(logical(0))
  }

  if (type != "source") {
    stop("Only 'source' type packages can be removed at present.")
  }

  contrib_url <- contrib.url(repo, type = type)
  pkg_files <- paste0(pkg, "_", version, ".tar.gz")

  current_paths <- file.path(contrib_url, pkg_files)
  current_paths <- current_paths[!duplicated(current_paths)]
  if (!all(file.exists(current_paths))) {
    stop("Not all packages to remove are present.")
  }

  removed <- file.remove(current_paths)

  removed
}
