# Utils and Environment variables
.cranium <- new.env(hash = TRUE)
.cranium[['package_fields']] <- c("Package", "Version", "Priority", "Depends", "Imports",
                                  "LinkingTo", "Suggests", "Enhances", "License", "License_is_FOSS",
                                  "License_restricts_use", "OS_type", "Archs", "MD5sum", "NeedsCompilation",
                                  "Path")

.cranium[['repo']] <- NULL

#' set repo location
#'
#' @param x a relative filepath
#'
#' @return the value of x (invisible)
#' @export
set_repo_location <- function(x) {
  .cranium[['repo']] <- x
  invisible(.cranium[['repo']])
}


#' Get Currently set repo location
#'
#' @return the filepath of the current repo
#' @export
get_repo_location <- function() {
  if(is.null(.cranium[['repo']])) set_repo_location(readline('No Repo Location Set:\n'))
  .cranium[['repo']]
}

# Get the base directory of a path
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

# Sets attribute, not by reference
addAttr <- function(x, name, value) {
  data.table::setattr(x, name, c(attr(x,name,exact = TRUE), value))
}

# Checks if it is a git address
is_git <- function(x) {
  substr(x, y <- nchar(x) - 3, y + 3) == '.git'
}

# Defines intended location
classer <- function(x) {
  x <-  copy(x)
  # Check ends
  if(is_git(x)) addAttr(x, 'class', 'git')
  
  # Check known addresses
  if(check_base_address(x, 'https://github.com')) addAttr(x, 'class', 'github')
  
  # Check protocols
  if(check_base_address(x, 'https://') | 
     check_base_address(x, 'http://')  |
     check_base_address(x, 'ftp://')) addAttr(x, 'class', 'web')
  else addAttr(x, 'class', 'local')
  x
}

