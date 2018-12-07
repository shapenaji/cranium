#' Serve a CRAN-like Package Repository
#'
#' Starts a local web server to serve packages from the repository.
#'
#' @param repo The location of the package repository.
#' @param repo_name The name of the repository, written to \code{DESCRIPTION}
#'   files.
#' @param host An IPv4 address owned by the server. Defaults to localhost.
#' @param port The port to run the server on.
#' @param detach Whether the server should run in the foreground or return
#'   immediately and run in the background.
#' @param ... Further arguments passed on to \code{\link{write_modpac}}.
#'
#' @return
#'
#' When \code{detach = TRUE}, this function returns a handle that can be passed
#' to \code{\link{stopServer}}. Otherwise it will not return at all, and must
#' be interrupted from the console.
#'
#' @export
serve <- function(repo, repo_name = "Cranium", host = "127.0.0.1", port = 8000,
                  detach = FALSE, ...) {
  if (!requireNamespace("httpuv", quietly = TRUE) ||
      !requireNamespace("webutils", quietly = TRUE) ||
      !requireNamespace("mime", quietly = TRUE)) {
    stop(paste0("The 'httpuv', 'webutils', and 'mime' packages are required ",
                "to run the cranium server."))
  }

  args <- list(...)
  config <- list()
  env <- new.env(FALSE, size = 1L)

  config$use_archive <- args$use_archive %||% TRUE
  config$use_hardlinks <- args$use_hardlinks %||% FALSE
  config$latest_only <- args$latest_only %||% FALSE
  config$fields <- args$fields %||% required_fields
  config$repo_name <- repo_name

  # Keep the package index in memory, loading it from the file if present and
  # using an empty index otherwise.

  url <- contrib.url(repo, type = "source")
  if (!dir.exists(url)) {
    dir.create(url, recursive = TRUE, showWarnings = FALSE)
  }

  index_path <- file.path(url, "PACKAGES.rds")
  if (!file.exists(index_path)) {
    empty <- matrix(nrow = 0, ncol = length(config$fields))
    colnames(empty) <- config$fields
    env$index <- empty
    message("Created an empty repository index.")
  } else {
    env$index <- readRDS(index_path)
    message("Using the existing repository contents.")
  }

  if (!detach) {
    # FIXME: This approach works with runServer but not with startServer.
    on.exit({
      message("Updating the on-disk repository index.")
      saveRDS(env$index, index_path, compress = "xz")
    })

    httpuv::runServer(host, port, list(
      call = router(repo, config, env)
    ))
  } else {
    httpuv::startServer(host, port, list(
      call = router(repo, config, env)
    ))
  }
}

router <- function(repo, config, env) {
  function(req) {
    path <- httpuv::decodeURIComponent(req$PATH_INFO)
    Encoding(path) <- "UTF-8"

    if (path == "/_ping") {
      return(ping())
    }

    # Serve the index out of memory so that we can be sure it is always up-to-
    # date vis-a-vis this server.
    if (req$REQUEST_METHOD %in% c("GET", "HEAD") &&
        path == "/src/contrib/PACKAGES.rds") {
      if (req$REQUEST_METHOD == "GET") {
        # NOTE: We're not using the traditional compression here.
        body <- serialize(env$index, connection = NULL)
      } else {
        body <- raw(0)
      }
      return(list(
        status = 200L,
        headers = list(
          "Content-Type" = "application/octet-stream",
          "Content-Length" = length(body)
        ),
        body = body
      ))
    }

    # No support for pre-3.0 clients, i.e. raw PACKAGES or PACKAGES.gz files.
    if (req$REQUEST_METHOD %in% c("GET", "HEAD") &&
        grepl("src/contrib/PACKAGES(|.gz)$", path)) {
      return(bad_request("Pre-3.0 clients are not supported."))
    }

    # Here's a nickel kid, use a real web server instead.
    if (req$REQUEST_METHOD %in% c("GET", "HEAD") && grepl("^/src", path)) {
      location <- file.path(repo, sub("^/", "", path))
      res <- if (file.exists(location)) {
        size <- file.info(location)[, "size"]
        if (req$REQUEST_METHOD == "GET") {
          con <- file(location, "rb", raw = TRUE)
          on.exit(close(con))
          body <- readBin(con, "raw", n = size)
        } else {
          body <- raw(0)
        }
        list(
          status = 200L,
          headers = list(
            "Content-Type" = mime::guess_type(location),
            # Send the real content length for HEAD requests.
            # See: https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13
            "Content-Length" = size
          ),
          body = body
        )
      } else {
        not_found()
      }
      return(res)
    }

    if (req$REQUEST_METHOD == "POST" && path == "/") {
      parsed <- webutils::parse_http(req$rook.input$read(), req$CONTENT_TYPE)
      if (!"file" %in% names(parsed)) {
        return(bad_request("Request must contain a 'file' in the form."))
      }

      # Don't rely on the filename to infer the package name and version.
      # Extract it from the archive instead.
      temp_file <- tempfile(fileext = ".tar.gz")
      pkg <- try({
        con <- file(temp_file, open = "wb", raw = TRUE)
        on.exit(close(con))
        writeBin(parsed$file$value, con)
        # Make sure we wrote the whole file (even if it is large) before we
        # attempt to extract metadata from it.
        flush(con)
        extract_description(temp_file, fields = c("Package", "Version"))
      })
      if (inherits(pkg, "try-error")) {
        # TODO: Log the error.
        return(bad_request("Invalid package bundle."))
      }

      bundle <- sprintf("%s_%s.tar.gz", pkg$Package, pkg$Version)
      location <- file.path(contrib.url(repo, type = "source"), bundle)
      res <- if (file.exists(location)) {
        list(
          status = 409L,
          headers = list(
            "Content-Type" = "text/plain; charset=utf-8",
            "Location" = paste0("/src/contrib/", bundle)
          ),
          body = "Package already exists on the server. Use PUT to replace it."
        )
      } else {
        # TODO: Actually copy the file to the repository.
        list(
          status = 201L,
          headers = list(
            "Content-Type" = "text/plain; charset=utf-8",
            "Location" = paste0("/src/contrib/", bundle)
          ),
          body = ""
        )
      }
      return(res)
    }

    if (req$REQUEST_METHOD == "PUT" && grepl("^/src", path)) {
      location <- file.path(repo, sub("^/", "", path))
      if (dirname(location) != contrib.url(repo, type = "source")) {
        return(bad_request("URI does not match the repository structure."))
      }

      parsed <- webutils::parse_http(req$rook.input$read(), req$CONTENT_TYPE)
      if (!"file" %in% names(parsed)) {
        return(bad_request("Request must contain a 'file' in the form."))
      }

      # Don't rely on the filename to infer the package name and version.
      # Extract it from the archive instead.
      temp_file <- tempfile(fileext = ".tar.gz")
      pkg <- try({
        con <- file(temp_file, open = "wb", raw = TRUE)
        on.exit(close(con))
        writeBin(parsed$file$value, con)
        # Make sure we wrote the whole file (even if it is large) before we
        # attempt to extract metadata from it.
        flush(con)
        extract_description(temp_file, fields = c("Package", "Version"))
      })
      if (inherits(pkg, "try-error")) {
        # TODO: Log the error.
        return(bad_request("Invalid package bundle."))
      }

      bundle <- sprintf("%s_%s.tar.gz", pkg$Package, pkg$Version)
      if (basename(location) != bundle) {
        return(bad_request("URI does not match the upload contents."))
      }

      res <- if (file.exists(location)) {
        # TODO: Actually copy the file to the repository.
        list(
          status = 200L,
          headers = list(
            "Content-Type" = "text/plain; charset=utf-8",
            "Location" = paste0("/src/contrib/", bundle)
          ),
          body = ""
        )
      } else {
        # TODO: Actually copy the file to the repository.
        list(
          status = 201L,
          headers = list(
            "Content-Type" = "text/plain; charset=utf-8",
            "Location" = paste0("/src/contrib/", bundle)
          ),
          body = ""
        )
      }
      return(res)
    }

    if (req$REQUEST_METHOD == "DELETE" && grepl("^/src", path)) {
      location <- file.path(repo, sub("^/", "", path))

      res <- if (file.exists(location)) {
        # TODO: Decide how to implement this.
        list(
          status = 403L,
          headers = list(
            "Content-Type" = "text/plain; charset=utf-8",
            "Location" = path
          ),
          body = "Package deletion is not permitted."
        )
      } else {
        not_found()
      }
      return(res)
    }

    not_found()
  }
}

ping <- function() {
  list(status = 200L, body = "", headers = list(
    "Content-Type" = "text/plain; charset=utf-8"
  ))
}

not_found <- function() {
  list(status = 404L, body = "", headers = list(
    "Content-Type" = "text/plain; charset=utf-8"
  ))
}

bad_request <- function(msg) {
  list(status = 400L, body = msg, headers = list(
    "Content-Type" = "text/plain; charset=utf-8"
  ))
}

date <- function() {
  format.POSIXlt(
    as.POSIXlt(Sys.time(), tz = "GMT"), format = "%a, %d %b %Y %H:%M:%S %Z",
    usetz = FALSE
  )
}

`%||%` <- function(lhs, rhs) if (!is.null(lhs)) lhs else rhs

# From tools:::.get_standard_repository_db_fields().
required_fields <- c(
  "Package", "Version", "Priority", "Depends", "Imports", "LinkingTo",
  "Suggests", "Enhances", "License", "License_is_FOSS", "License_restricts_use",
  "OS_type", "Archs", "MD5sum", "NeedsCompilation"
)
