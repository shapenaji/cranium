#' Installs a Package into a Repository
#'
#' @inheritParams write_modpac
#' @param pkg A path to a package, e.g. \code{"cranium.tar.gz"}.
#' @param type The type of the package. Only \code{"source"} is supported.
#' @param repo_name A name to give the repo, will be used by packrat to search for sources in user's options file
#' @param ... Further arguments passed on to \code{\link{write_modpac}}.
#'
#' @return A Descriptions Table of the packages made available
#' @export
install_package_to_repo <- function(pkg, repo = get_repo_location(),
                                    type = 'source', repo_name = get_repo_name(),
                                    ...) {

  if (type != "source") {
    stop("Only 'source' type packages can be added at present.")
  }

  # Modify Description to use Repo
  if(!is.null(repo_name)) {
    modify_description('Repository', repo_name, pkg)
  }

  if (!add_pkg(pkg, repo = repo, type = type, overwrite = TRUE)) {
    stop("Failed to add package bundle.")
  }

  write_modpac(repo, new_pkgs = basename(pkg), ...)
}
