#' Writes modified PACKAGES files to allow install.packages to access versions
#'
#' @param repo Repository Directory (by default, this uses the repo location set with "set_repo_location")
#' @param fields A vector of DESCRIPTION file fields to use in the PACKAGES file.
#' @param new_pkgs A vector of packages that need to be added to the index. When
#'   \code{NULL}, the entire index will be reconstructed instead.
#' @param use_archive When \code{TRUE}, create archive entries for older
#'   package versions.
#'
#' @return A Descriptions Table of the packages made available
#' @import data.table
#' @export
write_modpac <- function(repo = get_repo_location(),
                         fields = get_packages_fields(), new_pkgs = NULL,
                         use_archive = TRUE) {


  outrds <- file.path(contrib.url(repo), "PACKAGES.rds")
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

  if (use_archive) {
    cranDESCs[,CanArchive := !is_max_ver(Version), by = Package]
    old <- cranDESCs[(CanArchive)]
    out <- archive_pkg(old$Package, old$Version, repo = repo)
    message("Archived ", sum(out), " packages.")
  }

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

  np <- write_PACKAGES_files(cranDESCs, contrib.url(repo))
  message("Indexed ", np, " packages.")

  # Return descriptions, so we can see what's in there
  invisible(cranDESCs)
}

#' Write the Package Indices to Disk
#'
#' Write the package indices to disk in the format compatible with
#' \code{\link[tools]{write_PACKAGES}}.
#'
#' @param index A data frame of package metadata fields.
#' @param dir The directory to write the indices in.
#'
#' @return The number of packages in the index, invisibly.
#'
#' @noRd
write_PACKAGES_files <- function(index, dir) {
  db <- as.matrix(index)

  # Copied from tools::write_PACKAGES() as of R 3.5.1.
  np <- NROW(db)
  if (np > 0L) {
    db[!is.na(db) & (db == "")] <- NA_character_
    con <- file(file.path(dir, "PACKAGES"), "wt")
    write.dcf(db, con)
    close(con)
    con <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
    write.dcf(db, con)
    close(con)
    rownames(db) <- db[, "Package"]
    saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = "xz")
  }
  invisible(np)
}


