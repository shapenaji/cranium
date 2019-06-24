#' Initializes a new repo with the given name in the given location
#'
#' @param repo a path
#'
#' @return NULL
#' @export
init_repo <- function(repo) {
  tmp <- dir.create(contrib.url(repo, type = 'source'),recursive = TRUE)
  
  # If creation was successful, set repo directory to initialized repo
  if(tmp) {
    message('Repository successfully initialized')
    message(sprintf('Setting repo to: %s', repo))
    set_repo_location(repo)
  }
}

#' CRAN-Like Repositories
#'
#' Identify and work with CRAN-like repositories, including those created by
#' \code{cranium}.
#'
#' @param path The directory containing the packages.
#'
#' @return An object of class \code{"cranium_repository"}, which points to a
#'   validated CRAN-like repository.
#'
#' @export
repository <- function(path) {
  if (inherits(path, "cranium_repository")) path
  if (!is.character(path) || length(path) != 1) {
    stop("A repository must be represented by a valid path.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No repository exists at '", path, "'.", call. = FALSE)
  }
  structure(list(path = path), class = "cranium_repository")
}

#' @rdname repository
#' @export
summary.cranium_repository <- function(object, ...) {
  out <- list(
    path = object$path, name = NA, src_pkgs = 0L, has_archive = FALSE,
    has_meta = FALSE, win_binary_pkgs = 0L, macosx_binary_pkgs = 0L,
    macos_binary_pkgs = 0L
  )
  src_dir <- contrib.url(out$path, type = "source")
  if (dir.exists(src_dir)) {
    out$src_pkgs <- length(list.files(
      src_dir, pattern = "^([a-zA-Z0-9\\.])+_([^_])+\\.tar\\.gz$"
    ))
    out$has_archive <- dir.exists(file.path(src_dir, "Archive"))
    out$has_meta <- dir.exists(file.path(src_dir, "Meta"))
  }
  win_binary_dir <- contrib.url(out$path, type = "win.binary")
  if (dir.exists(win_binary_dir)) {
    out$win_binary_pkgs <- length(list.files(
      win_binary_dir, pattern = "^([a-zA-Z0-9\\.])+_([^_])+\\.zip$",
      recursive = TRUE
    ))
  }
  macosx_binary_dir <- contrib.url(out$path, type = "mac.binary")
  if (dir.exists(macosx_binary_dir)) {
    out$macosx_binary_pkgs <- length(list.files(
      macosx_binary_dir, pattern = "^([a-zA-Z0-9\\.])+_([^_])+\\.tgz$",
      recursive = TRUE
    ))
  }
  macos_binary_dir <- contrib.url(out$path, type = "mac.binary.el-capitan")
  if (dir.exists(macos_binary_dir)) {
    out$macos_binary_pkgs <- length(list.files(
      macos_binary_dir, pattern = "^([a-zA-Z0-9\\.])+_([^_])+\\.tgz$",
      recursive = TRUE
    ))
  }
  structure(out, class = "cranium_repository_summary")
}

#' @rdname repository
#' @export
print.cranium_repository <- function(x, ...) {
  print(summary(x))
}

#' @rdname repository
#' @export
print.cranium_repository_summary <- function(x, ...) {
  header <- if (is.na(x$name)) "CRAN-Like Repository" else {
    sprintf("Repository: %s", x$name)
  }
  cat(
    header, "\n",
    "Source packages: ", x$src_pkgs, "\n",
    "Has archive: ", if (x$has_archive) "yes" else "no", "\n",
    "Has metadata: ", if (x$has_meta) "yes" else "no", "\n",
    "Windows binary packages: ", x$win_binary_pkgs, "\n",
    "MacOS binary packages: ", x$macos_binary_pkgs, "\n",
    "Mac OSX binary packages: ", x$macosx_binary_pkgs, "\n",
    sep = ""
  )
}
