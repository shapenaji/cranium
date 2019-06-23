context("test-description-extract_description")

testthat('DESCRIPTION extraction/manipulation works', {
  
  test_description <-
    list(
      Type = "Package",
      Package = "PackageName",
      Title = "This is a package title",
      Version = "1.0.0",
      `Authors@R` = "c(\nperson(\"XXXXXXXX\", \"YYYYYYYY\", role = c(\"aut\", \"cre\")))",
      Description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
      License = "Commonly Admired License",
      URL = "https://github.com/user/package",
      BugReports = "https://github.com/user/package/issues",
      Suggests = "testthat",
      Encoding = "UTF-8",
      LazyLoad = "yes",
      RoxygenNote = "6.1.1",
      NeedsCompilation = "yes",
      Packaged = "2019-06-20 22:46:08 UTC; someguy",
      Author = "XXXXXXXX YYYYYYYY [aut, cre]",
      Maintainer = "XXXXXXXX YYYYYYYY <ZZZZZZZ@WWW.com>",
      Repository = "Internal",
      `Date/Publication` = "2019-06-21 08:00:11 UTC"
    )
  
  dir <- tempdir()
  file <- file.path(dir, 'DESC')
  
  write.dcf(test_description, file)
  
  targz <- tempfile(fileext = '.tar.gz')
  tar(targz, files = file, compression = 'gz')
  
  extracted <- extract_description(targz)
  
})