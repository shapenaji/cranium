testthat::context("test-removal")

testthat::test_that("individual package removal works as expected", {
  # Since removal is destructive, create a separate repo.
  repo <- dummy_repo(bundles = SAMPLE_BUNDLES)
  url <- contrib.url(repo, type = "source")

  removed <- remove_pkg(character(0), character(0), repo = repo)
  testthat::expect_equal(length(removed), 0)
  testthat::expect_error(remove_pkg("incorrect", character(0), repo = repo))

  # Remove two packages.
  PKGS <- SAMPLE_BUNDLES[1:2]
  NAME <- sub(regex_package_tar, "\\1", basename(PKGS))
  VERSION <- sub(regex_package_tar, "\\2", basename(PKGS))

  removed <- remove_pkg(NAME, VERSION, repo = repo)

  # Check that the output looks correct.
  testthat::expect_true(all(removed))
  testthat::expect_equal(sum(removed), 2)

  # Check that the files are gone.
  testthat::expect_equal(
    length(list.files(url, "\\.tar\\.gz$")), length(SAMPLE_BUNDLES) - 2
  )
  testthat::expect_true(all(!file.exists(file.path(url, basename(PKGS)))))
})
