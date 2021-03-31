#' @importFrom cli style_bold
#' @importFrom crayon blue yellow green red
#' @importFrom curl curl curl_download
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
chl_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa"
chl_v1_url <- glue_url(chl_url, "chelsa_V1")
chl_v2_url <- glue_url(chl_url, "chelsa_V2")
chl_v2_eur_url <- glue_url(chl_v2_url, "EUR11")
chl_v2_eur_obs_url <- glue_url(chl_v2_eur_url, "obsv")
chl_v2_glb_url <- glue_url(chl_v2_url, "GLOBAL") 
chl_v2_glb_mod_url <- glue_url(chl_v2_glb_url, "climatologies") 

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
  hdr <- curlGetHeaders(url)
  tmp <- as.numeric(
    gsub("\\D", "", hdr[grepl("^Content-Length:", hdr)])
  )
  class(tmp) <- "object_size"
  format(tmp, "auto", standard = "SI")
}



