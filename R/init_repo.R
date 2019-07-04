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
    path = object$path, name = NA, has_archive = FALSE, has_meta_dir = FALSE,
    src_pkgs = 0L, win_binary_pkgs = integer(), mac_binary_pkgs = integer(),
    mac_binary_el_capitan_pkgs = integer()
  )

  # Handle source and binary packages differently.
  src_dir <- contrib.url(out$path, type = "source")
  out$src_pkgs <- length(list.files(src_dir, pattern = "\\.tar\\..*$"))
  out$has_archive <- dir.exists(file.path(src_dir, "Archive"))
  out$has_meta_dir <- dir.exists(file.path(src_dir, "Meta"))

  out$win_binary_pkgs <- sapply(list_binary_pkg_files(out, "win.binary"), length)
  out$mac_binary_pkgs <- sapply(list_binary_pkg_files(out, "mac.binary"), length)
  out$mac_binary_el_capitan_pkgs <- sapply(
    list_binary_pkg_files(out, "mac.binary.el-capitan"), length
  )

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
    header, "\n\n",
    "Source Packages\n  Available: ", x$src_pkgs, "\n",
    "  Has archive: ", if (x$has_archive) "yes" else "no", "\n",
    "  Has meta directory: ", if (x$has_meta) "yes" else "no", "\n",
    sep = ""
  )
  if (length(x$win_binary_pkgs) > 0) {
    cat("\nWindows Binary Packages\n", paste0(
      "  Available for R ", names(x$win_binary_pkgs), ": ", x$win_binary_pkgs,
      collapse = "\n"
    ), "\n", sep = "")
  }
  if (length(x$mac_binary_el_capitan_pkgs) > 0) {
    cat("\nMacOS (El Capitan and Later) Binary Packages\n", paste0(
      "  Available for R ", names(x$mac_binary_el_capitan_pkgs), ": ",
      x$mac_binary_el_capitan_pkgs, collapse = "\n"
    ), "\n", sep = "")
  }
  if (length(x$mac_binary_pkgs) > 0) {
    cat("\nMacOS (Prior to El Capitan) Binary Packages\n", paste0(
      "  Available for R ", names(x$mac_binary_pkgs), ": ", x$mac_binary_pkgs,
      collapse = "\n"
    ), "\n", sep = "")
  }
}

list_binary_pkg_files <- function(repo, type) {
  stopifnot(type %in% c("win.binary", "mac.binary", "mac.binary.el-capitan"))
  ext <- switch(type, "win.binary" = "\\.zip$", "\\.tgz$")
  bin_dirs <- list.files(
    file.path(contrib.url(repo$path, type = type), ".."), full.names = TRUE
  )
  out <- lapply(bin_dirs, list.files, pattern = ext)
  names(out) <- basename(bin_dirs)
  out
}
