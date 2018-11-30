testthat::context("test-archiving")

testthat::test_that("individual package archiving works as expected", {
  # Since archival is destructive, create a separate repo.
  repo <- dummy_repo(bundles = SAMPLE_BUNDLES)
  url <- contrib.url(repo, type = "source")
  archive <- file.path(url, "Archive")

  archived <- archive_pkg(character(0), character(0), repo = repo)
  testthat::expect_equal(length(archived), 0)
  testthat::expect_error(archive_pkg("incorrect", character(0), repo = repo))

  # Archive two packages.
  PKGS <- SAMPLE_BUNDLES[1:2]
  NAME <- sub("^([^_]+)_([^_]+)\\.tar\\.gz$", "\\1", basename(PKGS))
  VERSION <- sub("^([^_]+)_([^_]+)\\.tar\\.gz$", "\\2", basename(PKGS))

  # Archive files only; do not remove originals.
  archived <- archive_pkg(NAME, VERSION, repo = repo, keep = TRUE)

  # Check that the output looks correct.
  testthat::expect_true(all(archived))
  testthat::expect_equal(sum(archived), 2)

  # Check that the files are in the archive.
  testthat::expect_equal(
    length(list.files(archive, "\\.tar\\.gz$", recursive = TRUE)), 2
  )
  # Check that the original files are intact.
  testthat::expect_true(all(file.exists(file.path(url, basename(PKGS)))))

  # Now archive destructively, removing the original files.
  archived <- archive_pkg(NAME, VERSION, repo = repo, keep = FALSE)
  testthat::expect_true(all(archived))
  testthat::expect_equal(sum(archived), 2)
  testthat::expect_true(all(!file.exists(file.path(url, basename(PKGS)))))
})
