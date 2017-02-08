#' Installs package from repo/github into repository (Not Vectorized)
#'
#' @param pkg a package, if using a cranium repository and multiple packages are available, versions can be selected with `_version`
#' @param repo the repo to install from, if this is at a github location, install github will be used
#' @param destdir the directory of the repository (NOT the contrib.url)
#'
#' @return A Descriptions Table of the packages made available
#' @import utils
#' @import tools
#' @export
#' @examples
#' # Install to default apache location
#' install_package_to_repo('data.table',destdir = '/var/www/html/')
install_package_to_repo <- function(pkg,
                                    repos = getOption('repos'),
                                    destdir = get_repo_location(),
                                    type = 'source') {

  isGithub <- check_base_address(repos, 'https://github.com')
  isGit <- grepl( '\\.git$', pkg)
  isInternal <- is.null(repos)

  # Support building from non-github gits
  if(isGit) {
    if(!require(git2r)) {
      warning('git2r is missing, cannot install from git')
    } else {
      # Check SSL
      isSecure <- check_base_address(pkg,'https://')
      fileloc <- file.path(contrib.url(destdir),basename(pkg))
      if(isSecure) {
        message('Starting Git Pull')
        git2r::clone(pkg,
                     local_path = fileloc,
                     credentials = cred_user_pass(readline(prompt = 'Username: '),
                                                  openssl::askpass(prompt = 'Password: ')))
        message(sprintf('Building in Directory'))
        devtools::build(fileloc)
        message(sprintf('Removing Build directory'))
        unlink(fileloc, recursive = TRUE)
      } else {
        # If they're sure....
        message('"https://" not detected, username:password will be sent in plaintext!')
        ans <- readline('Are you sure you want to do this (this is not recommended)? (N/y): ')
        if(!ans %in% c('Y','y','Yes','yes')) stop('Stopping.')

        message('Starting Git Pull')
        git2r::clone(pkg,
                     local_path = fileloc,
                     credentials = cred_user_pass(readline(prompt = 'Username: '),
                                                  openssl::askpass(prompt = 'Password: ')))
        message(sprintf('Building in Directory'))
        devtools::build(fileloc)
        message(sprintf('Removing Build directory'))
        unlink(fileloc, recursive = TRUE)
      }
    }

    # If repo is at Github, check that the package is available, then pull it
  } else if(any(isGithub)) {
    if(!require(git2r) | !require(httr)) {
      warning('git2r AND/OR httr is missing, cannot install from git repos')
    } else {
      # Check location to see if file is present
      #browser()
      gitloc <- sprintf('%s%s.git',repos[isGithub], pkg)
      if(httr::HEAD(gitloc)$status_code == 200) {
        # If so, clone and build



        message(sprintf('Found Github Repo %s', gitloc))
        fileloc <- sprintf('%s/%s',contrib.url(destdir),pkg)
        message('Starting Git Pull')
        git2r::clone(gitloc, fileloc)
        message(sprintf('Building in Directory'))
        devtools::build(fileloc)
        message(sprintf('Removing Build directory'))

        unlink(fileloc, recursive = TRUE)
      } else {
        warning(sprintf('%s not found at %s',pkg, repos[isGithub]))
      }
    }
    # If package is internal, copy it from it's current location
  } else if(any(isInternal)) {
    # Copy file from original location to repo
    out <- file.copy(pkg, file.path(contrib.url(destdir), basename(pkg)),
                     overwrite = TRUE)
    if(!out) stop('Failed to copy.')
  } else {
    out <-
      utils::download.packages(pkg,
                               destdir = contrib.url(destdir),
                               repos = repos,
                               type = type)
    print(out)
  }

  write_modpac(destdir)
}
