#' Access CHELSA timeseries
#'
#' @param var variable name.
#' @param year year. 
#' @param val optional.
#' @param path path to the folder where files will be stored.
#'
#' @details
#' "The CHELSA timeseries data consists of monthly downscaled model output temperature and precipitation estimates at a horizontal resolution of 30 arc sec. from 1979-2013. The resulting data consist of a mean monthly temperature and precipitation amounts." (Karger et al. 2017)
#'
#' @references
#' * <https://chelsa-climate.org>
#' * Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P., Kessler, M.(2017) Data from: Climatologies at high resolution for the earth’s land surface areas. Dryad Digital Repository. doi.org/10.5061/dryad.kd1d4.
#' 
#' @export
#' @examples
#' \dontrun{
#'  chl_ts("prec", 1982, 3)
#'  chl_ts("gts30", 1982)
#' }

chl_ts <- function(var, year, val = NULL, path = ".") {
  fl <- get_filename_ts(var, year, val)
  url <- glue_url(chl_v1_url, "timeseries", var)
  invisible(dl_data(url, fl, path))
}

# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/prec/CHELSA_prec_1979_01_V1.2.1.tif
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/gts30/CHELSA_gts30_1982_V1.2.1.tif

get_filename_ts <- function(var, year, val = NULL) {
  if (is.null(val)) {
    glue("CHELSA_{var}_{year}_V1.2.1.tif")
  } else {
    glue("CHELSA_{var}_{year}_{sprintf('%02d', val)}_V1.2.1.tif")
  }
}