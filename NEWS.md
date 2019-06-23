# cranium 2.0.0

* Added a `NEWS.md` file to track changes to the package.

Breaking Changes:

* removed `modify_descriptions` in lieu of just using `modify_description`

# cranium 2.0.0 (Release date: 2019-??-??)

Changes:
* Removes `modify_descriptions` (`modify_description`)

# cranium 1.0.0 (Release date: 2018-07-11)


Changes:
* Package index is not rebuilt on every new package (Thanks atheriel!)

# cranium 0.4.5 (Release date: 2017-12-16)


Changes:
* Adding new fields for Available packages, "Headline" for example
* Added functions to modify the fields used in the repo

# cranium 0.4.0 (Release date: 2017-4-12)


Changes:
* repo_name supported in install_package_to_repo, can be used with packrat
* modify_descriptions changes all descriptions in the repo.
* modify_description changes a single package

# cranium 0.3.0 (Release date: 2017-2-08)


Changes:
* modify_description modifies repo descriptions
* intended to work better with packrat (A LA RStudio Connect)
* Took off fedora

# cranium 0.2.0 (Release date: 2017-2-08)


Changes:
* Changes to argument names in install_package_to_repo.R, 
'destdir' has been changed to 'repo', to make the function less misleading,
updated documentation. 
* init_repo now lets us know what it's done. 
Hopefully the other children will learn from its fine example.
* Adjusted fedora.

# cranium 0.1.0 (Release date: 2017-2-08)


Changes:
* Initial Release, Documentation Spotty