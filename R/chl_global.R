#' Chelsa V2
#' 
#' @param horizon time horizons.
#' @param var variable.
#' @param val values.
#' @param model models.
#' @param scenario scenarios.
#' @param path path to the folder where files will be stored.
#' @param day day.
#' @param month month.
#' @param year year.
#'  
#' @details
#' * Models:
#'    - `mpi-esm1` : MPI-ESM1-2-HR
#'    - `gfdl-esm4` : GFDL-ESM4
#'    - `ipsl-cm6a` : IPSL-CM6A-LR
#'    - `mri-esm2` : MRI-ESM2-0
#'    - `ukesm1` : UKESM1-0-LL
#' * Scenarios (`scenarios`): SSP (Shared Socioeconomic Pathways)
#'    - `ssp126`: SSP 1, 2.6 W.m-2
#'    - `ssp370`: SSP 3, 7.0 W.m-2
#'    - `ssp585`: SSP 5, 8.5 W.m-2
#' * Time horizons (`horizons`):
#'    - `2010`: 1981-2010 ('model' and 'scenario' are ignored).
#'    - `2040`: 2011-2040
#'    - `2070`: 2041-2070
#'    - `2100`: 2071-2100
#' @export
#' @examples
#' \dontrun{
#'  chl_global_mod(var = "pr", val = 1, model = "ukesm1", scenario = "ssp370", horizon = 2010) 
#'  chl_global_mod(var = "pr", val = 1, model = "ukesm1", scenario = "ssp370", horizon = 2040) 
#'  chl_global_obsv_monthly(var = "pr")
#' }

chl_global_mod <- function(horizon = 2040, var = "bio", val = 10, model = "mpi-esm1", scenario = "ssp126", path = ".") {
  stopifnot(scenario %in% c("ssp126", "ssp370", "ssp585"))
  hz <- get_horizon(horizon)
  md <- get_model(model)
  if (hz == "1981-2010") {
    url <- glue_url(chl_v2_glb_mod_url, hz, var)
  } else {
    url <- glue_url(chl_v2_glb_mod_url, hz, md, scenario, var)
  }
  invisible(dl_data(url, get_filename_mod(var, val, md, hz, scenario), path))
}

#' @describeIn chl_global_mod observed data daily (for 1980).
chl_global_obsv_daily <- function(var, month = 1, day = 1, path = ".") {
  m <- sprintf("%02d", month)
  d <- sprintf("%02d", day)
  url <- glue_url(chl_v2_glb_url, "daily", var)
  fl <- glue("CHELSA_{var}_{m}_{d}_1980_V.2.1.tif")
  invisible(dl_data(url, fl, path))
}

#' @describeIn chl_global_mod observed data monthly (from 1980 up to 2018) .
chl_global_obsv_monthly <- function(var, month = 1, year = 1980, path = ".") {
  m <- sprintf("%02d", month)
  url <- glue_url(chl_v2_glb_url, "monthly", var)
  fl <- glue("CHELSA_{var}_{m}_{year}_V.2.1.tif")
  invisible(dl_data(url, fl, path))
}

# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/CHELSA_pr_01_1979_V.2.1.tif



# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio10_1981-2010_V.2.1.tif
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tas/CHELSA_tas_03_1981-2010_V.2.1.tif
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/daily/pr/CHELSA_pr_01_01_1980_V.2.1.tif
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/CHELSA_pr_01_1984_V.2.1.tif


get_horizon <- function(horizon) {
  switch(as.character(horizon),
    "2010" = {
      msgInfo("'model' and 'scenario' are ignored")
      "1981-2010"
    },
    "2040" = "2011-2040",
    "2070" = "2041-2070",
    "2100" = "2071-2100",
    {
      msgError("Not a valid time horizon")
      invisible(NULL)
    }
  )
}

get_model <- function(model) {
  switch(model,
    "mpi-esm1" = "MPI-ESM1-2-HR",
    "gfdl-esm4" = "GFDL-ESM4",
    "ipsl-cm6a" = "IPSL-CM6A-LR",
    "mri-esm2" = "MRI-ESM2-0",
    "ukesm1" = "UKESM1-0-LL",
    {
      msgError("Not a valid model")
      invisible(NULL)
    }
  )
}

get_filename_mod <- function(var, val, model, horizon, scenario) {
  switch(var, 
    bio = get_filename_mod_bio(val, model, horizon, scenario),
    pr = ,
    tas = ,
    tasmax = ,
    tasmin = get_filename_mod_oth(var, val, model, horizon, scenario),
    {
      msgError("Not a valid variable name.")
      invisible(NULL)
    }
  )
}

get_filename_mod_bio <- function(val, model, horizon, scenario) {
  stopifnot(val %in% 1:19)
  vl <- sprintf("%02d", val)
  if (horizon == "1981-2010") {
    glue("CHELSA_bio{vl}_1981-2010_V.2.1.tif")
  } else {
    glue("CHELSA_bio{vl}_{horizon}_{tolower(model)}_{scenario}_V.2.1.tif")
  }
}

get_filename_mod_oth <- function(var, val, model, horizon, scenario) {
  stopifnot(val %in% 1:12)
  vl <- sprintf("%02d", val)
  if (horizon == "1981-2010") {
    glue("CHELSA_{var}_{vl}_1981-2010_V.2.1.tif")
  } else {
    hz <- sub("-", "_", horizon)
    glue("CHELSA_{tolower(model)}_r1i1p1f1_w5e5_{scenario}_{var}_{vl}_{hz}_norm.tif")
  }
}
