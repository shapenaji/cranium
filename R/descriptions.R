#' Extract description from source package(s)
#'
#' @param x a .tar.gz package
#' @param fields A vector of DESCRIPTION file fields, or \code{NULL} to extract
#'   all of them.
#'
#' @return a data.table/data.frame object containing Description info
#' @export
#'
#' @examples
#' extract_description('test.tar.gz')
extract_description <- function(x, fields = NULL) {

  
  # unpack description files
  tmp <-
    Map(function(tarfile, exdir) {
      # list files in tarball
      files_in_tarball <- untar(tarfile, list = TRUE)
      extractfiles <- grep('DESCRIPTION$',files_in_tarball,ignore.case = FALSE,value = TRUE)
      # if multiple, take the shortest one
      extractfiles <- extractfiles[which.min(nchar(extractfiles))]
      message('extracting ', extractfiles)
      untar(
        tarfile = tarfile,
        files = extractfiles,
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
               as.data.frame(read.dcf(file, fields = fields),
                             stringsAsFactors = FALSE)),
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
  # source_files <- list.files(contrib.url(get_repo_location()), 
  #                            full.names = TRUE, 
  #                            pattern = '\\.tar\\.gz$')
  exit_dir <- file.path(tempdir(),'package/')
  prds_loc <- file.path(contrib.url(get_repo_location()),'PACKAGES.rds')
  if(!file.exists(prds_loc))
    stop('No PACKAGES.rds found. Run write_modpac on this directory and then rerun.')
  current_repo <- readRDS(prds_loc)
  current_repo <- setDT(data.frame(current_repo))
  # Convert Reserved Names
  current_repo[, PackageRoot := gsub('^(.*?)_.*$','\\1',Package)]
  nohardlinks <- current_repo[, .SD[which.min(nchar(as.character(Package)))], by = .(PackageRoot, Version)]
  locations <- file.path(contrib.url(get_repo_location()), paste0(nohardlinks$PackageRoot,'_',nohardlinks$Version,'.tar.gz'))
  
  tmp <- 
    Map(function(tarfile, exdir) {
      message('Untarring ', tarfile, ' into ', exdir)
      untar(
        tarfile = tarfile,
        exdir = exdir
      )}
      ,tarfile = locations
      ,exdir = file.path(exit_dir, noEXT(locations))
    )
  
  pac_files <-
    list.files(file.path(exit_dir, noEXT(locations)),
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
  
  # Update all description files in their original location?
  Map(write.dcf, descs, pac_files[unpacked_descs])
  
  # Retar 
  # Rebuild packages
  # Try catch, if building fails, move on but do not write file. 
  # It's okay to leave broken packages in the repo, we just need to warn
  rebuilt <- Map(function(pkg) {
    message('Rebuilding ', pkg)
    tryCatch({
      devtools::build(pkg, quiet = TRUE)
    }, error = function(e) {
      message('Failed to build package: ', e)
      message('Skipping')
      return(NULL)
    })
    }, 
      pkg = file.path(exit_dir, build_locations))
  
  
  builds <- unlist(rebuilt[lengths(rebuilt) > 0])
  locations <- locations[lengths(rebuilt) > 0]
  ops <- file.copy(builds, file.path(contrib.url(get_repo_location()),basename(builds)),overwrite = TRUE)
  
  if(any(!ops)) {
    # inform the user that these packages were not updated
    message('The following packages failed to copy')
  }
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
  if (!grepl("\\.tar\\.gz$", file)) {
    stop("'file' must be a package bundle.")
  }

  exdir <- file.path(tempdir(), "extracted", noEXT(file))
  on.exit(unlink(exdir, recursive = TRUE))

  # We don't want to accidentally add anything to the archive later, so be sure
  # to clean up first.
  unlink(exdir, recursive = TRUE)
  untar(tarfile = file, exdir = exdir, compressed = "gzip", restore_times = FALSE)

  pkgdir <- list.dirs(exdir, full.names = TRUE, recursive = FALSE)

  desc <- as.data.frame(
    read.dcf(file.path(pkgdir, "DESCRIPTION"), fields = NULL),
    stringsAsFactors = FALSE
  )
  desc[[field]] <- value
  write.dcf(desc, file = file.path(pkgdir, "DESCRIPTION"))

  new_bundle <- file.path(exdir, basename(file))
  # To mimic R CMD build, we need to operate tar() on the current directory.
  curr_dir <- getwd()
  on.exit(setwd(curr_dir))
  setwd(exdir)
  tar(
    basename(file),
    files = list.dirs(),
    compression = "gzip"
  )
  # file may be a relative path
  setwd(curr_dir)
  on.exit()

  # We want to fail if we fail to modify, not return a warning
  stopifnot(file.rename(new_bundle, file))
}


#' Extract descriptions from a repository
#'
#' @param repo a path to a repository
#' @param fields A vector of DESCRIPTION file fields, or \code{NULL} to extract
#'   all of them.
#'
#' @return a data.table/data.frame object containing Description info
#' @export
#'
#' @examples
#' repo_descriptions()
repo_descriptions <- function(repo = get_repo_location(), fields = NULL) {
  # Location of source files
  rurl <- contrib.url(repo)

  # Get All source packages
  pacs <- list.files(rurl, pattern = '\\.tar\\.gz$', full.names = TRUE)

  # Extract only description files from tarballs, use the base directory name
  # in the tarball, store them in tempdir
  out <- extract_description(pacs, fields = fields)

  # Add originating location
  out$Path <- pacs

  out
}
