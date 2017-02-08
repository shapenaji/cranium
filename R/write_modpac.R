#' Writes modified PACKAGES files to allow install.packages to access versions
#'
#' @param repo Repository Directory
#' @param loc visible location (External might be http://server/path/to/repo, internal might be file://path/to/repo)
#'
#' @return A Descriptions Table of the packages made available
#' @import data.table
#' @export
write_modpac <- function(repo = get_repo_location()) {


  out <- file(sprintf('%s/%s',contrib.url(repo), "PACKAGES"), "wt")
  outgz <- gzfile(sprintf('%s/%s',contrib.url(repo), "PACKAGES.gz"), "wt")

  DESCs <- repo_descriptions(repo)
  cranDESCs <- DESCs[,mget(.cranium[['package_fields']], ifnotfound = NA)]

  cranDESCs <- cranDESCs[,if(.N > 1) rbind(.SD,.SD[is_max_ver(Version)]) else .SD,
                         by = Package]


  # Define the newest as the
  cranDESCs[,IsNewest := (.N == frank(ifelse(is_max_ver(Version), 1, 0),
                                      ties.method = 'first')),
            by = Package]
  cranDESCs[,IsHardLink := !is_shortest_string(Path),
            by = .(Package, Version)]

  cranDESCs <- cranDESCs[(!IsHardLink)]
  cranDESCs[(!IsNewest), Package := interaction(Package, Version, sep = '_')]
  cranDESCs[(!IsNewest), HardLinkLocation := hardlink_name(Path, Version)]

  # in order to deal with download.packages,
  # make hard links to the resulting packages
  # that are created by it's frankenstein string op
  Linkable <- cranDESCs[(!IsNewest & !file.exists(HardLinkLocation))]
  if(nrow(Linkable) > 0) Linkable[,Map(file.link, Path, HardLinkLocation)]

  # Clean Up before conversion to matrix
  cranDESCs[,IsNewest := NULL]
  cranDESCs[,HardLinkLocation := NULL]
  cranDESCs[,IsHardLink := NULL]

  # License data
  license_info <- tools:::analyze_licenses(cranDESCs$License)
  cranDESCs$License <- ifelse(license_info$is_standardizable,
                              license_info$standardization, NA)

  cranDESCs[, MD5sum := sapply(Path, md5sum)]
  cranDESCs[, Path := NULL]

  desc <- as.matrix(cranDESCs)

  for (i in seq_len(nrow(desc))) {
    #browser()
    desci <- desc[i, !(is.na(desc[i,]) | (desc[i,] == "")), drop = FALSE]

    write.dcf(desci, file = out)
    # if (nzchar(path[i]))
    #   cat("Path: ", path[i], "\n", sep = "", file = out)
    cat("\n", file = out)
    write.dcf(desci, file = outgz)
    # if (nzchar(path[i]))
    #   cat("Path: ", path[i], "\n", sep = "", file = outgz)
    cat("\n", file = outgz)
  }

  close(out)
  close(outgz)
  cranDESCs
}


