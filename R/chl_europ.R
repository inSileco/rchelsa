#' Access high resolution climate data for Europe.
#'
#' @param period time period (see below) or "doc" for documentation.
#' @param var variable name (see XXX).
#' @param year year only available when period is set to 'daily' or 'yearly'.
#' @param path path to the folder where files will be stored.

#' @details 
#' Time periods (`periods`) are as follows: 
#' * `daily`: Data files on a daily resolution (usually years available are 1981-2005)
#' * `yearly`: Annual aggregations of a variable (usually years available are 1981-2005). 
#' * `monthly`: Data files on a monthly resolution.
#' * `normal`: Long term, climatological, means of a variable over a Normals period.
#'
#' @references
#' * <https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/EUR11/documentation/CHELSA_EUR11_technical_documentation.pdf>
#' * Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2017) Climatologies at high resolution for the earth’s land surface areas. Scientific Data 4, 170122 
#' * Karger, Dirk Nikolaus; Dabaghchian, Babek; Lange, Stefan; Thuiller, Wilfried; Zimmermann, Niklaus E.; Graham, Catherine H. (2020). High resolution climate data for Europe. EnviDat. doi:10.16904/envidat.150. 
#'
#' @export
#' @examples 
#' \dontrun{
#' chl_europ_obs("normal", "pr")
#  chl_europ_obs("daily", "pr", 1989)
#  chl_europ_obs("monthly", "pr", 1989)
#' }


chl_europ_obs <- function(period = "year", var, year, path = ".") {
  # TODO add check here.
  switch(period,
    doc = dl_data(glue_url(chl_v2_eur_url, "documentation"), 
      "CHELSA_EUR11_technical_documentation.pdf", path),
    daily = chl_europ_obs_daily(var, year, path = "."),
    yearly = chl_europ_obs_yearly(var, year, path = "."),
    monthly = chl_europ_obs_monthly(var, year, path = "."),
    normal = chl_europ_obs_normals(var, path = "."),
    msgError("Period should be one of 'daily', 'yearly', 'monthly' or 'normal'") 
  )
}

chl_europ_obs_daily <- function(var, year, path = ".") {
  fl <- glue("CHELSA_EUR11_{var}_day_{year}_V1.1.nc")
  invisible(dl_data(glue_url(chl_v2_eur_obs_url, "daily"), fl, path))
}

chl_europ_obs_yearly <- function(var, year, path = ".") {
  fl <- glue("CHELSA_EUR11_{var}_{year}_V1.1.nc")
  invisible(dl_data(glue_url(chl_v2_eur_obs_url, "annual"), fl, path))
}

chl_europ_obs_monthly <- function(var, year, path = ".") {
  fl <- glue("CHELSA_EUR11_{var}_mon_1981-2005_V1.1.nc")
  invisible(dl_data(glue_url(chl_v2_eur_obs_url, "monthly"), fl, path))
}

chl_europ_obs_normals <- function(var, path = ".") {
  fl <- glue("CHELSA_EUR11_{var}_norm_1981-2005_V1.1.nc")
  invisible(dl_data(glue_url(chl_v2_eur_obs_url, "normals"), fl, path))
}



