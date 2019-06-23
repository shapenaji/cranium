testthat::context("test-add-package")

testthat::test_that("adding package(s) works as expected", {
  
  skip_if(Sys.getenv('CRANIUM_TEST_PACKAGES_DIR') == '', 'CRANIUM_TEST_PACKAGES_DIR env variable is unset. No packages to test.')
  # Since removal is destructive, create a separate repo.
  repo <- dummy_repo(bundles = character(0))
  url <- contrib.url(repo, type = "source")
  
  added <- add_pkg(character(0), repo = repo)
  testthat::expect_equal(length(added), 0)
  testthat::expect_error(add_pkg("incorrect", repo = repo))
  
  # Add two packages
  PKGS <- SAMPLE_BUNDLES[1:2]
  NAME <- sub(regex_package_tar, "\\1", basename(PKGS))
  VERSION <- sub(regex_package_tar, "\\2", basename(PKGS))
  
  added <- add_pkg(PKGS, repo = repo)
  
  
  # Check that the output looks correct.
  testthat::expect_true(all(added))
  testthat::expect_equal(sum(added), 2)
  
  # Check that the files are present.
  testthat::expect_equal(
    length(list.files(url, "\\.tar\\.gz$")), 2
  )
  
  # Add all packages and overwrite
  PKGS <- SAMPLE_BUNDLES
  
  # Check that overwrite changes package
  added <- add_pkg(PKGS, repo = repo, overwrite = TRUE)
  
  testthat::expect_true(all(added), label = 'All bundles were overwritten')
  
  # Check that the total number of packages is 3
  testthat::expect_equal(
    length(list.files(url, "\\.tar\\.gz$")), 3
  )
})

