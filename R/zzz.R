#' @importFrom cli style_bold
#' @importFrom crayon blue yellow green red
#' @importFrom curl curl curl_download curl_fetch_memory parse_headers
#' @importFrom curl new_handle handle_setopt
#' @importFrom glue glue
NULL

# HELPERS
glue_path <- function(...) {
  glue(..., .sep = .Platform$file.sep, .envir = parent.frame(1))
}
glue_url <- function(...) {
  glue(..., .sep = "/", .envir = parent.frame(1))
}


# URL
chl_url <- "https://os.zhdk.cloud.switch.ch"
chl_v1_url <- glue_url(chl_url, "chelsav1")
chl_v2_url <- glue_url(chl_url, "chelsav2")
chl_v2_eur_url <- glue_url(chl_v2_url, "EUR11")
chl_v2_eur_obs_url <- glue_url(chl_v2_eur_url, "obs")
chl_v2_glb_url <- glue_url(chl_v2_url, "GLOBAL")
chl_v2_glb_mod_url <- glue_url(chl_v2_glb_url, "climatologies")

# from inSilecoMisc

msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(blue(txt), appendLF = appendLF)
  invisible(txt)
}

msgError <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$cross, ...)
  message(red(txt), appendLF = appendLF)
  invisible(txt)
}

msgSuccess <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$tick, ...)
  message(green(txt), appendLF = appendLF)
  invisible(txt)
}

msgWarning <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$warning, ...)
  message(yellow(txt), appendLF = appendLF)
  invisible(txt)
}

dl_check <- function(url, destfile, ...) {
  if (file.exists(destfile)) {
    msgWarning("skipped (already downloaded)")
  } else {
    msgInfo("Accessing", url, glue("({get_remote_file_size(url)})"))
    curl::curl_download(url, destfile, ...)
    msgSuccess("file downloaded!")
  }
  invisible(TRUE)
}

dl_data <- function(base_url, file, path) {
  url <- glue_url(base_url, file)
  dl_check(url, destfile = glue_path(path, file))
  url
}

get_remote_file_size <- function(url) {
  # https://github.com/r-lib/httr/issues/612
  h <- new_handle()
  handle_setopt(h, nobody = TRUE)
  req <- curl_fetch_memory(url, handle = h)
  hdr <- parse_headers(req$headers)
  tmp <- gsub("\\D", "", hdr[grepl("^Content-Length:", hdr)])
  if (length(tmp)) {
    format(
      structure(as.numeric(tmp), class = "object_size"),
      "auto",
      standard = "SI"
    )
  } else {
    "unknown"
  }
}
