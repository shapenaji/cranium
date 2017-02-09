#' Checks packages stored at location, wrapper around available.packages from utils
#'
#' @param repo a local or external location, relative and absolute paths accepted
#'
#' @return a Descriptions table
#' @export
#'
#' @examples
#'
#' # Get Current Repository's Packages
#' pkgs_available(getOption('repos'))
#'
#'
pkgs_available <- function(repo) {
  if(grepl('^(file|http|https|ftp):', repo)) {
    available.packages(repos = repo)
  } else if(.Platform$OS.type == 'unix' & substring(repo, 1L, 1L) == '/') {
    available.packages(repos = file.path('file:\\',repo))
  } else if(.Platform$OS.type == 'windows' & grepl('^[A-z]{1}:/',repo)) {
    stop('unsupported filename type, use file:// instead')
    #available.packages(repos = gsub('^[A-z]{1}:\\','file://',repo))
  } else {
    available.packages(repos = file.path('file:/',getwd(),repo))
  }
}
