#' Download Chelsea data
#'
#' @param categ category. 
#' @param type type.
#' @param id resource id.
#' @param path path to the folder where files will be stored.
#' 
#' @references
#' <https://chelsa-climate.org>
#' 
#' @export 

get_chelsea_data <- function(categ = "clim", type = "bio", id = 1, path = ".") {
  
  switch(
    categ, 
    clim = get_chelsea_clim(type, id, path),
    msgError("'categ' not recognized (only 'clim' is currently available).")
  )
  
}

# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/tmin/CHELSA_tmin10_01_1979-2013_V1.2_land.tif
# 
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/nolandsea/CHELSA_prec_10_1979-2013_V1.2.tif
# 
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio/CHELSA_bio10_04.tif
# 
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/prec/CHELSA_prec_02_V1.2_land.tif

get_chelsea_clim <- function(type, id, path) {
  base_url <- glue_url(chl_v1_url, "climatologies")
  switch(type,
    bio = {
      stopifnot(id %in% 1:19)
      id <- sprintf("%02d", id)
      get_chelsea_url(
        base_url, type, glue("CHELSA_{type}10_{id}.tif"), path
      )
    },
    prec = {
      stopifnot(id %in% 1:12)
      id <- sprintf("%02d", id)
      get_chelsea_url(
        base_url, type, glue("CHELSA_{type}_{id}_V1.2_land.tif"), path
      )
    },
    tmean = {
      stopifnot(id %in% 1:12)
      id <- sprintf("%02d", id)
      get_chelsea_url(
        base_url, type, glue("CHELSA_{type}10_{id}_1979-2013_V1.2_land.tif"), 
        path)
    },
    tmin = {
      stopifnot(id %in% 1:12)
      id <- sprintf("%02d", id)
      get_chelsea_url(
        base_url, type, glue("CHELSA_{type}10_{id}_1979-2013_V1.2_land.tif"), 
        path)
    },
    tmax = {
      stopifnot(id %in% 1:12)
      id <- sprintf("%02d", id)
      get_chelsea_url(
        base_url, type, glue("CHELSA_{type}10_{id}_1979-2013_V1.2_land.tif"), 
        path)
    },
    msgError("'type' not recognized (available types are 'bio', 'prec', 
      'tmean', 'tmin', 'tmax')")
  )
}

get_chelsea_url <- function(base_url, type, file, path) {
  url <- glue_url(base_url, type, file)
  dl_check(
    glue_url(base_url, type, file), 
    destfile = glue(path, "/", file)
  )
}