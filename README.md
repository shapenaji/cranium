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
for the other versions. 

The repository will always link the base package name to the newest version 
by default, but if there are multiple versions, these are available via

`<package>_<version>`




We always start by setting our repo location.

### Set Repo Location First


```r
set_repo_location(tempdir())
```

### Install from github:


```r
install_package_to_repo(pkg = 'miniCRAN', 
                        repos = 'https://github.com/RevolutionAnalytics/')
```

### Install the CRAN version:


```r
install_package_to_repo(pkg = 'miniCRAN')
```

### Installing from git:


```r
install_package_to_repo('https://path/to/package.git')
```

### Installing a local package:


```r
install_package_to_repo('path/to/package.tar.gz', repos = NULL)
```

### Install using repository "name" (for packrat/RStudio Connect)

Or we can install using a Repository "name" (as of v0.4.1)

packrat uses this name to lookup the repo location in installs to RStudio Connect

(it will check getOption('repos')['REPO_NAME'])


```r
install_package_to_repo(pkg = 'path/to/package.tar.gz', 
                        repos = NULL, 
                        repo_name = 'MyRepoName')
```


We can also set the repo name globally:


```r
set_repo_name('MyRepoName')

# Everything gets installed with Repo "MyRepoName"
install_package_to_repo(pkg = 'miniCRAN')
```


## Checking the repository


```r
# We can use the available.packages function from utils
# to browse repos
available.packages(contrib.url(file.path('file:/',tempdir())))
```


## Using the repository

the cranium package does not need to be installed by your users.
They only need to see the final product, the repo, whether on a network drive or on a webserver.


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

Easiest of all, however, is to include this repository among your/their options
in your `.Rprofile` or `Rprofile.site` file.


```r
# Add to .RProfile
local({
    repos <- getOption("repos")
    # Replace your repo address here
    repos["MyRepo"] <- "http://0.0.0.0/reponame"
    repos["CRAN"] <- "@CRAN@"
    options(pkgType = "source")
    options(repos = repos)
})
```

## Modifying the repository

We also include functions for maintaining the repository,

`modify_descriptions` will adjust every description in the repository. 
Adding the given field with the given value. This can be handy for setting Repository name

`modify_description` can be used to modify a single tar.gz file

**Note: Both functions rebuild packages. Make sure you modify under the desired R version.**


```r
set_repo_location('/var/www/html/mini_CRAN_Repo')

# Set the repository for all packages to be the current repo_name
modify_descriptions('Repository', get_repo_name())
```
