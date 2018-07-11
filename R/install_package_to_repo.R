#' Installs package from repo/github into repository (Not Vectorized)
#'
#' @param pkg Options:
#' \enumerate{
#'  \item a package name, (Example: 'cranium'), if using a cranium repository and multiple packages are available, versions can be selected with `_version`,
#'  \item a path to a package, (Example: '~/Downloads/cranium_0.2.0.tar.gz'), requires repos = NULL 
#' }
#' @param repos Accepts several options:
#' \itemize{
#' \item Repos: (DEFAULT: getOption('repos')) A vector of repos that will be searched for the package
#' \item Github: An address of the form: 'https://github/<accountname>/'
#' \item Secure Gits: An address of the form: 'https://<path>/<to>/<repository>.git'
#' \item Insecure Gits: An address of the form: 'http://<path>/<to>/<repository>.git' 
#' With a STRONG warning that you should not be authenticating over http:, 
#' but we understand that some networks aren't perfect. 
#' \item Local Pkgs: NULL
#' }
#' a list of repos that will be checked for the package (getOption('repos')) OR an  OR a '
#' @param repo by default, the value in get_repo_location(), but this can be any repository directory
#' @param type Which type of package, Currently: 'source' is the only one supported. Plans are to include binaries.
#' @param repo_name default: NULL, A name to give the repo, will be used by packrat to search for sources in user's options file
#' @param fields A vector of DESCRIPTION file fields to use in the PACKAGES file.
#'
#' @return A Descriptions Table of the packages made available
#' @import utils
#' @import tools
#' @export
#' @examples
#' # Install from CRAN repositories to default apache2 location
#' install_package_to_repo('data.table',destdir = '/var/www/html/')
install_package_to_repo <- 
  function(pkg,
           repos = getOption('repos'),
           repo = get_repo_location(),
           type = 'source',
           repo_name = get_repo_name(),
           fields = get_packages_fields()
  ) {
    
    # Fileloc is used for temp files
    # We automatically remove this on exit
    fileloc <- NULL
    on.exit({
      message(sprintf('Removing Build directory'))
      unlink(fileloc, recursive = TRUE)
    })
    
    isGithub <- check_base_address(repos, 'https://github.com') | check_base_address(pkg, 'https://github.com')
    isGit <- grepl( '\\.git$', pkg)
    isInternal <- is.null(repos)
    
    
    
    # If repo is at Github, check that the package is available, then pull it
    if(any(isGithub)) {
      if (!requireNamespace("git2r", quietly = TRUE) | !requireNamespace("httr", quietly = TRUE)) {
        warning('git2r AND/OR httr is missing, cannot install from git repos')
      } else {
        # Check location to see if file is present
        
        gitloc <- 
          if(check_base_address(pkg, 'https://github.com')) {
            pkg
          } else {
            sprintf('%s%s.git', repos[isGithub], pkg)
          }
        
        if(httr::HEAD(gitloc)$status_code == 200) {
          
          # If so, clone and build
          message(sprintf('Found Github Repo %s', gitloc))
          fileloc <- sprintf('%s/%s',contrib.url(repo),pkg)
          message('Starting Git Pull')
          git2r::clone(gitloc, fileloc)
          message(sprintf('Building in Directory'))
          targz <- devtools::build(fileloc)
          
          # if repo_name set, rebuild
          if(!is.null(repo_name)) {
            message('Adding repo_name and rebuilding')
            modify_description('Repository', repo_name, targz)
          }
          
          
          
        } else {
          warning(sprintf('%s not found at %s',pkg, repos[isGithub]))
        }
      }
      
      # Support building from non-github gits
    } else if(isGit) {
      if (!requireNamespace("git2r", quietly = TRUE)) {
        stop('git2r is missing, cannot install from git')
      } else {
        # Check SSL
        isSecure <- check_base_address(pkg,'https://')
        fileloc <- file.path(contrib.url(repo),basename(pkg))
        if(isSecure) {
          # Pull from location
          message('Starting Git Pull')
          git2r::clone(pkg,
                       local_path = fileloc,
                       credentials = 
                         cred_user_pass(
                           readline(prompt = 'Username: '),
                           if(requireNamespace("getPass", quietly = TRUE)) {
                             getPass::getPass('Password: ', forcemask = TRUE)
                           } else {
                             warning('openssl does not mask terminal password entry!\n We recommend you install getPass')
                             openssl::askpass('Password: ')
                           }
                         )
          )
          
          # Build in directory
          message(sprintf('Building in Directory'))
          targz <- devtools::build(fileloc)
          
          # if repo_name set, rebuild
          if(!is.null(repo_name)) {
            message('Adding repo_name and rebuilding')
            modify_description('Repository', repo_name, targz)
          }
          
          unlink(fileloc, recursive = TRUE)
        } else {
          # Check if they're sure....
          message('"https://" not detected, username:password will be sent in plaintext!')
          ans <- readline('Are you sure you want to do this (this is not recommended)? (N/y): ')
          
          if(!ans %in% c('Y','y','Yes','yes')) stop('Stopping.')
          
          # Pull from location
          message('Starting Git Pull')
          git2r::clone(
            pkg,
            local_path = fileloc,
            credentials = 
              cred_user_pass(
                readline(prompt = 'Username: '),
                if(requireNamespace("getPass", quietly = TRUE)) {
                  getPass::getPass('Password: ', forcemask = TRUE)
                } else {
                  warning('openssl does not mask terminal password entry!\n We recommend you install getPass')
                  openssl::askpass('Password: ')
                }
              )
          )
          
          # Build in directory
          message(sprintf('Building in Directory'))
          targz <- devtools::build(fileloc)
          
          # if repo_name set, rebuild
          if(!is.null(repo_name)) {
            message('Adding repo_name and rebuilding')
            modify_description('Repository', repo_name, targz)
          }
        }
      }
      
      
      # If package is internal, copy it from it's current location
    } else if(any(isInternal)) {
      
      
      # Modify Description to use Repo
      if(!is.null(repo_name)) {
        modify_description('Repository', repo_name, pkg)
      }
      # Copy file from original location to repo
      out <- 
        file.copy(
          from = pkg, 
          to = file.path(contrib.url(repo), basename(pkg)),
          overwrite = TRUE
        )
      
      if(!out) stop('Failed to copy.')
    } else {
      out <-
        utils::download.packages(
          pkg,
          destdir = contrib.url(repo),
          repos = repos,
          type = type
        )
      
      
      # Modify Description to use Repo
      if(!is.null(repo_name)) {
        modify_description('Repository', repo_name, out[,2])
      }
      
      print(out)
    }
    
    # FIXME: Need to ensure that pkg is the actual tarball name.
    write_modpac(repo, fields = fields, new_pkgs = basename(pkg))
  }
