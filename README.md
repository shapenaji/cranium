# Construct your Cranium
Nicholas Jhirad  



## The cranium package

This document covers version >= 0.2.0 of the package and is intended to be an  
introduction to the package, 



The `cranium` package functions similarly to the excellent [miniCRAN](https://github.com/RevolutionAnalytics/miniCRAN) with a few exceptions:  

* Multiple versions of a package can be stored in a repo, and users can install 
  versions by running `install.packages('<package_name>_<version_number>', repos = <your_repo>)`

* `install_package_to_repo(...)` functions as an omnitool, and installs from other repos, 
                             .git locations, local directories or from github


Not all packages are free to distribute on github, and even if they are,  
you may want your own stable working repository for your applications  

The cranium package aims to make creating/populating/maintaining a private repository  
quick and painless, while storing older versions of packages that can be requested on demand.  

## Package Details

The package depends on:
 
 * `data.table` version >= 1.10
 * `tools`
 * `utils`

the package suggests:
 
 * `git2r`     for github and git
 * `httr`      for checking sources
 * `openssl`   for hashes and password entry

It requires no installation for users.

but a private server running apache may be recommended.  

## Creating a Repository


```r
library(cranium)

# For the purposes of this vignette, we'll use a temporary dir
# But this is intended to be something viewable to a team,
init_repo(repo = tempdir())
```

That's it!

This will create the basic folder structure for your repo.

For my usecase, I started a web-server, and deployed the repository to my
live directory.


## Populating a Repository

Installing another version will NOT overwrite the existing one,   
the index updating function will manage this and create new install versions
for the other versions. The repository will always link to the newest version 
**by default**, but older versions can be installed by adding the suffix:
`_<version>`.





```r
###################################################################
# For the purposes of this vignette, I will be using the following
# CRAN mirror, you should not need to set this
options(repos = 'https://cloud.r-project.org/')

###################################################################
# We can forceably set our repo location:
###################################################################
set_repo_location(tempdir())

###################################################################
# Let's add the miniCRAN package
###################################################################
install_package_to_repo(pkg = 'miniCRAN', 
                        repos = 'https://github.com/RevolutionAnalytics/')

###################################################################
# And the version from CRAN to compare
# miniCRAN
###################################################################
install_package_to_repo(pkg = 'miniCRAN')

###################################################################
# We can also install directly from git 
# (It will prompt you for your username/pw)
###################################################################
install_package_to_repo('https://path/to/package.git')

###################################################################
# Or we can install from a local directory
###################################################################
install_package_to_repo('path/to/package.tar.gz', repos = NULL)
```


## Checking the repository


```r
# We can use the available.packages function from utils
# to browse repos
available.packages(contrib.url(file.path('file:/',tempdir())))
```


## Using the repository

As said earlier, cranium requires no setup for your users, 
it just needs to be visible to them, whether on a network drive or on a webserver.


```r
# Install from local
install.packages('miniCRAN',file.path('file:/',tempdir()))

# Install from a server (I have mine running off my shiny server)
install.packages('miniCRAN', 'http://hostname/repolocation')

# Install a version:
# The Following will error unless the two above packages have different versions,
# we only store alternate versions when there are multiple copies of the file in the repo)
install.packages('miniCRAN_0.2.7', 'http://hostname/repolocation')
```

Easiest of all, however, is to include this repository among your options
in your `.Rprofile` or `Rprofile.site` file.


```r
# Add to .RProfile
local({
    repos <- getOption("repos")
    # Replace your repo address here
    repos["Internal_Repo"] <- "http://0.0.0.0/reponame"
    repos["CRAN_Rstudio"] <- "https://cran.rstudio.com/"
    repos["CRAN_rproject"] <- "https://cloud.r-project.org/"
    options(repos = repos)
})
```
