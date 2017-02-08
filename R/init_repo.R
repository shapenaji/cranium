#' Initializes a new repo with the given name in the given location
#'
#' @param repo a path
#'
#' @return NULL
#' @export
init_repo <- function(repo) {
  dir.create(contrib.url(repo, type = 'source'),recursive = TRUE)
}
