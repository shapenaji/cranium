#' Add Packages to a Repository
#'
#' @param pkg A vector of package bundles to add, e.g. \code{"cranium.tar.gz"}.
#' @param repo The location of the package repository.
#' @param type The type of the package. Only \code{"source"} is supported.
#' @param overwrite When \code{TRUE}, overwrite any existing packages that match
#'   the input bundles.
#'
#' @return A logical vector indicating which packages were added successfully.
#'
#' @export
add_pkg <- function(pkg, repo = get_repo_location(), type = "source",
                    overwrite = FALSE) {
  stopifnot(length(pkg) == length(version))

  # Although this is likely a user error, let's be consistent with vector
  # inputs.
  if (length(pkg) == 0) {
    return(logical(0))
  }

  if (type != "source") {
    stop("Only 'source' type packages can be added at present.")
  }

  contrib_url <- contrib.url(repo, type = type)

  file.copy(
    pkg, file.path(contrib_url, basename(pkg)), overwrite = overwrite
  )
}
