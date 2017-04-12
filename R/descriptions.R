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

#' Modify field in every description
#'
#' @param field example: Repository
#' @param value example: internal
#'
#' @return A new available packages table
#' @export
modify_descriptions <- function(field, value) {
  source_files <- list.files(contrib.url(get_repo_location()), 
                             full.names = TRUE, 
                             pattern = '\\.tar\\.gz$')
  exit_dir <- file.path(tempdir(),'package/')
  tmp <- 
    Map(function(tarfile, exdir) {
      untar(
        tarfile = tarfile,
        exdir = exdir
      )}
      ,tarfile = source_files
      ,exdir = file.path(exit_dir, noEXT(source_files))
    )
  
  pac_files <-
    list.files(file.path(exit_dir, noEXT(source_files)),
               recursive = TRUE,
               full.names = TRUE,
               include.dirs = FALSE)
  
  # Select only the description files
  unpacked <- gsub(exit_dir,'',pac_files, fixed = TRUE)
  unpacked_descs <- grepl('^/.*?/.*?/DESCRIPTION$', unpacked)
  build_locations <- gsub('^/(.*?/.*?/.*?).*','\\1', unpacked[unpacked_descs])
  
  descs <- lapply(pac_files[unpacked_descs], 
                  function(file)
                    data.frame(read.dcf(file, fields = NULL)))
  
  descs <- lapply(descs, function(x) {x[[field]] <- value;x})
  
  # Update all description files
  Map(write.dcf, descs, pac_files[unpacked_descs])
  
  # Rebuild packages
  Map(function(pkg, path) devtools::build(pkg), 
      pkg = file.path(exit_dir, build_locations))
  
  
  builds <- list.files(list.files(exit_dir, full.names = TRUE), 
                       full.names = TRUE, pattern = '\\.tar\\.gz$')
  
  file.rename(builds, source_files)
  write_modpac(get_repo_location())
}

#' Modify description of a single zipped package
#'
#' @param field example: Repository
#' @param value example: internal
#' @param file example: /path/to/cranium_0.3.0.tar.gz 
#'
#' @return A new available packages table
#' @export
modify_description <- function(field, value, file) {
  source_files <- file
  exit_dir <- file.path(tempdir(),'package/')
  tmp <- 
    Map(function(tarfile, exdir) {
      untar(
        tarfile = tarfile,
        exdir = exdir
      )}
      ,tarfile = source_files
      ,exdir = file.path(exit_dir, noEXT(source_files))
    )
  
  pac_files <-
    list.files(file.path(exit_dir, noEXT(source_files)),
               recursive = TRUE,
               full.names = TRUE,
               include.dirs = FALSE)
  
  # Select only the description files
  unpacked <- gsub(exit_dir,'',pac_files, fixed = TRUE)
  unpacked_descs <- grepl('^/.*?/.*?/DESCRIPTION$', unpacked)
  build_locations <- gsub('^/(.*?/.*?/.*?).*','\\1', unpacked[unpacked_descs])
  
  descs <- lapply(pac_files[unpacked_descs], 
                  function(file)
                    data.frame(read.dcf(file, fields = NULL)))
  
  descs <- lapply(descs, function(x) {x[[field]] <- value;x})
  
  # Update all description files
  Map(write.dcf, descs, pac_files[unpacked_descs])
  
  # Rebuild packages
  Map(function(pkg, path) devtools::build(pkg), 
      pkg = file.path(exit_dir, build_locations))
  
  
  builds <- list.files(list.files(exit_dir, full.names = TRUE), 
                       full.names = TRUE, pattern = '\\.tar\\.gz$')
  
  file.rename(builds, source_files)
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
