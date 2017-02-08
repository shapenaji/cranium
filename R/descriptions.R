#' Extract description from source package(s)
#'
#' @param x a .tar.gz package
#'
#' @return a data.table/data.frame object containing Description info
#' @importFrom data.table rbindlist
#' @export
#'
#' @examples
#' extract_description('test.tar.gz')
extract_description <- function(x) {

  # unpack description files
  tmp <-
    Map(function(tarfile, exdir) {
      untar(
        tarfile = tarfile,
        files = sprintf('%s/DESCRIPTION',getbasedir(tarfile)),
        exdir = exdir
      )}
      ,tarfile = x
      ,exdir = file.path(tempdir(), 'DESC', noEXT(x))
    )

  DESCfiles <-
    list.files(file.path(tempdir(), 'DESC', noEXT(x)),
               recursive = TRUE,
               full.names = TRUE,
               include.dirs = FALSE)

  # Bind them together to get directory
  out <-
    data.table::rbindlist(
      lapply(DESCfiles,
             function(file)
               data.frame(read.dcf(file, fields = NULL))),
      fill = TRUE)
  out
}

#' Extract descriptions from a repository
#'
#' @param repo a path to a repository
#'
#' @return a data.table/data.frame object containing Description info
#' @export
#'
#' @examples
#' repo_descriptions()
repo_descriptions <- function(repo = get_repo_location()) {
  require(data.table)

  # Location of source files
  rurl <- contrib.url(repo)

  # Get All source packages
  pacs <- list.files(rurl, pattern = '\\.tar\\.gz$', full.names = TRUE)

  # Extract only description files from tarballs, use the base directory name
  # in the tarball, store them in tempdir
  out <- extract_description(pacs)

  # Add originating location
  out$Path <- pacs

  out
}
