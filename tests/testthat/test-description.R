context("test-description-extract_description")

testthat::test_that('DESCRIPTION extraction/manipulation works', {
  
  test_description <-
    data.frame(
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
      `Date/Publication` = "2019-06-21 08:00:11 UTC",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  
  dir <- tempfile("description-test")
  dir.create(file.path(dir, "pkgname"), showWarnings = FALSE, recursive = TRUE)
  file <- file.path(dir, "pkgname", "DESCRIPTION")

  write.dcf(test_description, file)
  testthat::expect_true(file.exists(file))

  # To mimic R CMD build, we need to operate tar() on the current directory.
  targz <- tempfile("description-test", fileext = ".tar.gz")
  curr_dir <- getwd()
  on.exit(setwd(curr_dir))
  setwd(dir)
  tar(targz, files = list.dirs(), compression = "gzip")
  setwd(curr_dir)
  on.exit()

  extracted <- extract_description(targz)
  testthat::expect_equal(test_description, extracted)
})
