#' @title Get list of GCM, RCP and climate projections from CHELSA-CMIP5 datasets
#'
#' @description
#' Return a list of GCM, RCP and climate projections from CHELSA-CMIP5 datasets (\url{http://chelsa-climate.org/}).
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @export
#'
#' @return a list of 4 vectors:
#'   - a vector of RCP names
#'   - a vector of GCM names
#'   - a vector of GCM-RCP combination for 2041-2060
#'   - a vector of GCM-RCP combination for 2061-2080
#'
#' @examples
#'
#' cmip5_infos <- list_chelsa_cmip5()
#'
#' length(cmip5_infos)
#' ## [1] 4
#'
#' names(cmip5_infos)
#' ## [1] "rcp" "gcm" "2041-2060" "2061-2080"
#'
#' cmip5_infos$rcp
#' ## [1] "rcp26" "rcp45" "rcp60" "rcp85"


list_chelsa_cmip5 <- function() {

  horizons <- c("2041-2060", "2061-2080")

  projections <- list()

  for (horizon in horizons) {

    prj_list <- readLines(
      paste0(
        "https://www.wsl.ch/lud/chelsa/data/cmip5/",
        horizon,
        "/bio/"
      )
    )

    prj_list <- prj_list[grep("CHELSA_bio_mon_", prj_list)]
    prj_list <- strsplit(prj_list, "a href=")
    prj_list <- unlist(lapply(prj_list, function(x) x[2]))
    prj_list <- strsplit(prj_list, "\"")
    prj_list <- unlist(lapply(prj_list, function(x) x[2]))

    prj_list <- strsplit(prj_list, "_")

    prj_list <- unique(
      paste(
        unlist(lapply(prj_list, function(x) x[4])),
        unlist(lapply(prj_list, function(x) x[5])),
        sep = "__"
      )
    )

    projections[[horizon]] <- prj_list
  }


  cmip5 <- list(
    sort(
      unique(
        unlist(
          lapply(
            strsplit(
              unlist(
                projections
              ),
              "__"
            ),
            function(x) {
              x[2]
            }
          )
        )
      )
    ),
    sort(
      unique(
        unlist(
          lapply(
            strsplit(
              unlist(
                projections
              ),
              "__"
            ),
            function(x) {
              x[1]
            }
          )
        )
      )
    ),
    sort(
      projections[["2041-2060"]]
    ),
    sort(
      projections[["2061-2080"]]
    )
  )
  names(cmip5) <- c("rcp", "gcm", names(projections))

  return(cmip5)
}

