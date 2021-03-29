#' @importFrom cli style_bold
#' @importFrom crayon blue yellow green red
#' @importFrom curl curl curl_download
#' @importFrom glue glue
NULL


# HELPERS
glue_url <- function(...) glue(..., .sep = "/", .envir = parent.frame(1))
chelsea_v1_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1"
chelsea_v2_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2"

# from inSilecoMisc

msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(blue(txt), appendLF = appendLF)
  invisible(txt)
}

msgError <- function(...) {
  txt <- paste(cli::symbol$cross, ...)
  message(red(txt))
  invisible(txt)
}

msgSuccess <- function(...) {
  txt <- paste(cli::symbol$tick, ...)
  message(green(txt))
  invisible(txt)
}

msgWarning <- function(...) {
  txt <- paste(cli::symbol$warning, ...)
  message(yellow(txt))
  invisible(txt)
}

dl_check <- function(url, destfile, ...) {
  if (file.exists(destfile)) {
    msgWarning("skipped (already dowloaded)")
  } else {
    msgInfo("Accessing", url)
    curl::curl_download(url, destfile, ...)
    msgSuccess("file downloaded!")
  }
  invisible(TRUE)
}

