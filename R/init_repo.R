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
