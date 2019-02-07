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
                         use_archive = TRUE, use_hardlinks = TRUE,
                         latest_only = TRUE) {
  url <- contrib.url(repo, type = "source")
  cranDESCs <- index_repository(
    repo = repo, fields = fields, type = "source", new_pkgs = new_pkgs
  )

  setDT(cranDESCs)
  cranDESCs[, Basename := gsub("_[^_]+", "", Package)]
  cranDESCs[, IsNewest := is_max_ver(Version), by = Basename]
  cranDESCs[, Archived := FALSE]
  cranDESCs[, Source := file.path(url, paste0(Basename, "_", Version, ".tar.gz"))]

  if (use_archive) {
    # TODO: Decide whether we should also put the newest version in the
    # archive.

    cranDESCs[, CanArchive := !IsNewest & !IsHardLink, by = Package]
    old <- cranDESCs[(CanArchive),]
    out <- archive_pkg(old$Basename, old$Version, repo = repo, keep = FALSE,
                       overwrite = FALSE)
    cranDESCs[(CanArchive), Archived := out]
    cranDESCs[, CanArchive := NULL]
    message("Archived ", sum(out), " packages.")
  }

  if (use_hardlinks) {
    links <- cranDESCs[(!IsHardLink),]
    links[, IsHardLink := TRUE]
    links[, Path := file.path(
      url, paste0(Basename, "_", Version, "_", Version, ".tar.gz")
    )]
    links[, Package := paste(Basename, Version, sep = "_")]

    # Point hardlinks towards archived packages, if present.
    links[(Archived), Source := file.path(
      url, "Archive", Basename, paste0(Package, ".tar.gz")
    )]
    links[(!Archived), Source := file.path(url, paste0(Package, ".tar.gz"))]

    # Handle previous hardlinks.
    all_links <- merge(
      cranDESCs[(IsHardLink),], links, all = TRUE,
      by = setdiff(names(links), c("Archived", "Source"))
    )
    all_links[, Source := Source.x]
    all_links[(Archived.y | is.na(Source)), Source := Source.y]
    all_links[, Archived := FALSE]
    all_links[, c("Archived.x", "Archived.y", "Source.x", "Source.y") := NULL]

    cranDESCs <- rbind(cranDESCs[(!IsHardLink)], all_links)
    cranDESCs <- cranDESCs[(!duplicated(cranDESCs))]

    Linkable <- cranDESCs[(IsHardLink & !file.exists(Path)),]
    if (nrow(Linkable) > 0) {
      if (!all(file.exists(Linkable$Source))) {
        stop("Can't create hardlinks; not all source paths exist.")
      }
      linked <- file.link(Linkable$Source, Linkable$Path)
      message("Linked ", sum(linked), " packages.")
    }
  } else {
    cranDESCs <- cranDESCs[(!IsHardLink),]
  }

  if (latest_only) {
    # Drop anythign that won't be visible to available.packages().
    cranDESCs <- cranDESCs[(IsNewest | IsHardLink | file.exists(Path)),]
  } else {
    # Drop anything we've archived.
    cranDESCs <- cranDESCs[(file.exists(Path)),]
  }

  # Clean Up before conversion to matrix
  cranDESCs[, IsNewest := NULL]
  cranDESCs[, Path := NULL]
  cranDESCs[, IsHardLink := NULL]
  cranDESCs[, Source := NULL]
  cranDESCs[, Basename := NULL]
  cranDESCs[, Archived := NULL]
  cranDESCs <- cranDESCs[order(Package, Version),]

  np <- write_PACKAGES_files(cranDESCs, url)
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

standardize_license <- function(license) {
  # We want to match the internal tools:::analyze_licenses().
  fn <- get("analyze_licenses", envir = asNamespace("tools"))
  info <- fn(license)
  ifelse(info$is_standardizable, info$standardization, NA_character_)
}

#' Index the Contents of a Repository
#'
#' Constructs a data frame of a CRAN-like repository's contents from a
#' combination of (1) the \code{PACKAGES} file, if it exists; (2) the
#' \code{DESCRIPTION} files from new package bundles; and (3) any changes due
#' to removed (or archived) packages.
#'
#' @param repo The location of the repository.
#' @param fields A vector of fields to use in the \code{"PACKAGES"} files.
#' @param type The type of packages in the repository. Only \code{"source"} is
#'   supported at present.
#' @param new_pkgs An optional vector of package bundles to recompute metadata
#'   for. This is intended to indicate package bundles that have replaced
#'   existing packages in the index, likely a rare operation for stable repos.
#'
#' @return A data frame of package metadata specified by \code{fields}.
#'
#' @export
index_repository <- function(repo = get_repo_location(),
                             fields = get_packages_fields(),
                             type = "source",
                             new_pkgs = NULL) {
  stopifnot(type == "source")
  url <- contrib.url(repo, type = type)
  outrds <- file.path(url, "PACKAGES.rds")

  if (file.exists(outrds)) {
    index <- as.data.frame(readRDS(outrds), stringsAsFactors = FALSE)
    # The Path refers to the location of the tarball for the package, which is
    # the file that is actually accessed by file.download(). The Source points
    # to the location of the original tarball for hardlinks, and to the Path
    # otherwise.
    index$Path <- file.path(
      url, paste0(index$Package, "_", index$Version, ".tar.gz")
    )
    index$IsHardLink <- grepl("_", index$Package, fixed = TRUE)
    index$Source <- ifelse(
      index$IsHardLink,
      # TODO: Account for using the Archive.
      file.path(url, paste0(index$Package, ".tar.gz")),
      file.path(url, paste0(index$Package, "_", index$Version, ".tar.gz"))
    )
  } else {
    # Create an empty data frame if PACKAGES is missing.
    index <- lapply(seq_along(fields), function(x) character(0))
    names(index) <- fields
    index <- as.data.frame(index, stringsAsFactors = FALSE)
    index$Path <- character(0)
    index$IsHardLink <- logical(0)
    index$Source <- character(0)
  }

  # Create entries for the sources of hardlinks, if they exist, so that we do
  # not need to reconstruct this from the DESCRIPTION files.
  links <- index[index$IsHardLink,]
  if (nrow(links) > 0) {
    links$Package <- gsub("_[^_]+", "", links$Package)
    links$Path <- file.path(
      url, paste0(links$Package, "_", links$Version, ".tar.gz")
    )
    links$Source <- links$Path
    links$IsHardLink <- FALSE
    links <- links[file.exists(links$Path),]
    index <- rbind(index, links)
    index <- index[!duplicated(index),]
  }

  # Grab all tarballs that don't have the hardlink format (e.g. they contain
  # only one underscore).
  pkg_bundles <- list.files(
    url, pattern = "^([a-zA-Z0-9\\.])+_([^_])+\\.tar\\.gz$", full.names = TRUE
  )

  removed <- setdiff(index$Source, pkg_bundles)
  index <- index[!index$Source %in% removed,]

  new <- setdiff(pkg_bundles, index$Source)
  new <- union(new, file.path(url, new_pkgs))

  if (length(new) > 0) {
    new_desc <- extract_description(new, fields = fields)
    new_desc$Path <- new
    new_desc$IsHardLink <- FALSE
    new_desc$Source <- new
    new_desc$MD5sum <- tools::md5sum(new)
    new_desc$License <- ifelse(is.null(new_desc$License), NA_character_,
                               new_desc$License)
    new_desc$License <- standardize_license(new_desc$License)

    # Drop existing entries for new packages. There is probably a better way to
    # do this.
    for (row in 1:nrow(new_desc)) {
      existing_entry <- index$Package == new_desc$Package[row] &
        index$Version == new_desc$Version[row]
      index <- index[!existing_entry,]
    }

    index <- rbind(index, new_desc, fill = TRUE)
  }

  index
}
