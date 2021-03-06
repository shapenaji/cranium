% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_modpac.R
\name{write_modpac}
\alias{write_modpac}
\title{Writes modified PACKAGES files to allow install.packages to access versions}
\usage{
write_modpac(repo = get_repo_location(),
  fields = get_packages_fields(), new_pkgs = NULL,
  use_archive = TRUE, use_hardlinks = TRUE, latest_only = TRUE)
}
\arguments{
\item{repo}{Repository Directory (by default, this uses the repo location set with "set_repo_location")}

\item{fields}{A vector of DESCRIPTION file fields to use in the PACKAGES file.}

\item{new_pkgs}{A vector of packages that need to be added to the index. When
\code{NULL}, the entire index will be reconstructed instead.}

\item{use_archive}{When \code{TRUE}, create archive entries for older
package versions.}
}
\value{
A Descriptions Table of the packages made available
}
\description{
Writes modified PACKAGES files to allow install.packages to access versions
}
