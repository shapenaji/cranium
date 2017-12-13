# Utils and Environment variables
.cranium <- new.env(hash = TRUE)
.cranium[['package_fields']] <- c("Package", "Version", "Priority", "Depends", "Imports",
                                  "LinkingTo", "Suggests", "Enhances", "License", "License_is_FOSS",
                                  "License_restricts_use", "OS_type", "Archs", "MD5sum", "NeedsCompilation",
                                  "Path",'Repository','Header')

.cranium[['repo']] <- NULL
.cranium[['repo_name']] <- NULL
#' set repo location (normalizes path)
#'
#' @param x a relative filepath
#'
#' @return the value of x (invisible)
#' @export
set_repo_location <- function(x) {
  .cranium[['repo']] <- normalizePath(x)
  invisible(.cranium[['repo']])
}

#' Get Currently set repo location
#'
#' @return the filepath of the current repo
#' @export
get_repo_location <- function() {
  if(is.null(.cranium[['repo']])) stop('No repo location set, use set_repo_location.')
  .cranium[['repo']]
}

#' set repo name (optional)
#'
#' @param x a name for the repo, this will be used by packrat when searching for a repository
#'
#' @return the value of x (invisible)
#' @export
set_repo_name <- function(x) {
  .cranium[['repo_name']] <- x
  invisible(.cranium[['repo_name']])
}

#' get repo name (optional)
#'
#' @return the name of the current repo
#' @export
get_repo_name <- function() {
  .cranium[['repo_name']]
}

# Get the base directory of a tarball
getbasedir <- function(x) {
  strsplit(untar(x, list = TRUE)[[1]],'/')[[1]][1]
}

# Remove extension from tar.gz
noEXT <- function(x) {
  gsub('(^.*_[0-9]+.*?)\\.tar\\.gz$', '\\1', basename(x))
}

# Get Max Version of several versions
max_ver <- function(z) {
  z <- as.character(z)
  Reduce(function(x,y) {
    if(utils::compareVersion(x,y) + 1L) x else y
  },z)
}

# Is maximum version
is_max_ver <- function(z) {
  max_ver(z) == as.character(z)
}

# Is the shortest string
is_shortest_string <- function(z) {
  nchar(z) == min(nchar(z))
}

# Create a hardlink that meets requirements of download.packages
hardlink_name <- function(Path, Version) {
  sprintf(gsub('(.*)(\\.tar\\.gz)','\\1_%s\\2',Path),Version)
}

# Checks the base address of an url against a string to see if they match
check_base_address <- function(x,url) {
  m = nchar(url)
  substr(x,1,m) == url
}

# Sets attribute, by reference
addAttr <- function(x, name, value) {
  data.table::setattr(x, name, c(attr(x,name,exact = TRUE), value))
}

# Checks if is git
is_git <- function(x) {
  substr(x, y <- nchar(x) - 3, y + 3) == '.git'
}



# Defines intended location
addr_class <- function(x) {
  x <- copy(x)
  
  # Check ends
  if(is_git(x)) addAttr(x, 'class', 'git')
  
  # Check known addresses
  if(check_base_address(x, 'https://github.com')) addAttr(x, 'class', 'github')
  
  # Check protocols
  if(check_base_address(x, 'https://')) addAttr(x, 'class', c('web','ssl'))
  if(check_base_address(x, 'http://')  |
     check_base_address(x, 'ftp://')) addAttr(x, 'class', 'web')
  if(!inherits(x, 'web')) addAttr(x, 'class', 'local')
  x
}

