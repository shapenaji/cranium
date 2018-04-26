#' Writes modified PACKAGES files to allow install.packages to access versions
#'
#' @param repo Repository Directory (by default, this uses the repo location set with "set_repo_location")
#' @param fields A vector of DESCRIPTION file fields to use in the PACKAGES file.
#' @param new_pkgs A vector of packages that need to be added to the index. When
#'   \code{NULL}, the entire index will be reconstructed instead.
#'
#' @return A Descriptions Table of the packages made available
#' @import data.table
#' @export
write_modpac <- function(repo = get_repo_location(),
                         fields = get_packages_fields(), new_pkgs = NULL) {


  # We write three versions of the PACKAGES file, two in dcf format, 
  # one in native rds format
  out <- file(sprintf('%s/%s',contrib.url(repo), "PACKAGES"), "wt")
  outgz <- gzfile(sprintf('%s/%s',contrib.url(repo), "PACKAGES.gz"), "wt")
  outrds <- file.path(contrib.url(repo), "PACKAGES.rds")

  # Close connections on exit
  on.exit({
    close(out)
    close(outgz)
  })
  
  update <- !is.null(new_pkgs) && file.exists(outrds)

  if (update) {
    message("Updating PACKAGES indices.")
    index <- as.data.frame(readRDS(outrds), stringsAsFactors = FALSE)

    # Drop existing version duplicates, so we can update the newest version
    # later.
    index$Package <- gsub("_[\\.0-9]+", "", index$Package)
    index <- index[!duplicated(index),]

    # We don't store this, so we have to reconstruct it.
    index$Path <- file.path(contrib.url(repo),
                            paste0(index$Package, "_", index$Version,
                                   ".tar.gz"))

    new_desc <- extract_description(file.path(contrib.url(repo), new_pkgs),
                                    fields = fields)
    new_desc$Path <- file.path(contrib.url(repo), new_pkgs)
    new_desc$MD5sum <- tools::md5sum(new_desc$Path)
    new_desc$License <- ifelse(is.null(new_desc$License), NA_character_,
                               new_desc$License)

    # Drop existing entries for new packages. There is probably a better way to
    # do this.
    for (row in 1:nrow(new_desc)) {
      existing_entry <- index$Package == new_desc$Package[row] &
        index$Version == new_desc$Version[row]
      index <- index[!existing_entry,]
    }
    cranDESCs <- rbind(index, new_desc, fill = TRUE)
  } else {
    message("Regenerating PACKAGES indices.")
    cranDESCs <- repo_descriptions(repo, fields = fields)
  }

  cranDESCs <- cranDESCs[,if(.N > 1) rbind(.SD,.SD[is_max_ver(Version)]) else .SD,
                         by = Package]


  # Define the newest as the
  cranDESCs[,IsNewest := (.N == frank(ifelse(is_max_ver(Version), 1, 0),
                                      ties.method = 'first')),
            by = Package]

  # Drop hardlink entries if we're regenerating the index from a file list
  # before we actually compute metadata. Remember that md5sum is expensive!
  if (!update) {
    cranDESCs[,IsHardLink := !is_shortest_string(Path),
              by = .(Package, Version)]
    cranDESCs <- cranDESCs[(!IsHardLink)]
    cranDESCs[,IsHardLink := NULL]
    cranDESCs[,MD5sum := md5sum(Path)]
  }

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
  cranDESCs[,Path := NULL]

  # License data
  license_info <- tools:::analyze_licenses(cranDESCs$License)
  cranDESCs$License <- ifelse(license_info$is_standardizable,
                              license_info$standardization, NA_character_)

  cranDESCs <- cranDESCs[order(Package, Version),]

  desc <- as.matrix(cranDESCs)

  for (i in seq_len(nrow(desc))) {
    #browser()
    desci <- desc[i, !(is.na(desc[i,]) | (desc[i,] == "")), drop = FALSE]

    # Save a 
    write.dcf(desci, file = out)
    cat("\n", file = out)
    
    # Save a gz version
    write.dcf(desci, file = outgz)
    cat("\n", file = outgz)
    
    # as of R 3.4, we also save an rds version
    
  }

  saveRDS(desc, outrds)
  # Return descriptions, so we can see what's in there
  cranDESCs
}


