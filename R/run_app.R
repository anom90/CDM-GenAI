#' Run CDM Analysis App
#' @export
run_app <- function(port = 8000, open_browser = TRUE) {
  api_path <- system.file("plumber.R", package = "cdm.gen.ai")
  www_path <- system.file("www", package = "cdm.gen.ai")

  if (api_path == "") {
    stop("plumber.R tidak ditemukan di dalam package")
  }

  if (www_path == "") {
    stop("Folder inst/www tidak ditemukan di dalam package")
  }

  # router API
  pr <- plumber::plumb(api_path)

  # ================================
  # HTTP Server
  # ================================
  app <- list(
    call = function(req) {
      path <- req$PATH_INFO

      # ------------------------------
      # 1. API ROUTE
      # ------------------------------
      if (startsWith(path, "/api")) {
        return(pr$call(req))
      }

      # ------------------------------
      # 2. ROOT
      # ------------------------------
      if (path == "/" || path == "") {
        path <- "/index.html"
      }

      # ------------------------------
      # 3. STATIC FILE CHECK
      # ------------------------------
      file_path <- file.path(www_path, sub("^/", "", path))

      # mapping /data -> data.html
      if (!file.exists(file_path)) {
        html_path <- paste0(file_path, ".html")

        if (file.exists(html_path)) {
          file_path <- html_path
        }
      }

      # ------------------------------
      # 4. FILE EXISTS
      # ------------------------------
      if (file.exists(file_path) && !dir.exists(file_path)) {
        return(list(
          status = 200L,
          headers = list(
            "Content-Type" = mime::guess_type(file_path)
          ),
          body = readBin(
            file_path,
            "raw",
            file.info(file_path)$size
          )
        ))
      }

      # ------------------------------
      # 5. SPA FALLBACK
      # ------------------------------
      index_path <- file.path(www_path, "index.html")

      return(list(
        status = 200L,
        headers = list(
          "Content-Type" = "text/html"
        ),
        body = readBin(
          index_path,
          "raw",
          file.info(index_path)$size
        )
      ))
    }
  )

  # ================================
  # OPEN BROWSER
  # ================================
  if (open_browser) {
    later::later(function() {
      utils::browseURL(
        sprintf("http://localhost:%s", port)
      )
    }, 1)
  }

  message("CDM Analysis running at http://localhost:", port)

  httpuv::runServer(
    host = "0.0.0.0",
    port = port,
    app = app
  )
}
