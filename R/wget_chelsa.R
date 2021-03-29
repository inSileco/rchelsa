#' @title Bulk download of CHELSA climate datasets
#'
#' @description
#' This function downloads climate normals from the Chelsa database (\url{http://chelsa-climate.org/}).
#' User can select specific bioclimatic variables, time slices, GCMs and RCPs.
#'
#' @param path a character vector of the path to stored dowloaded files.
#' @param vars a character/numeric vector of bioclimatic variable(s) id (from 1 to 19).
#' @param horizons a character vector of time slices ("1979-2013", "2041-2060", "2061-2080").
#' @param gcms a character vector of CMIP5 GCM names (see \url{http://chelsa-climate.org/future/} for a complete list). Only required if \code{time = "2041-2060"} or \code{time = "2061-2080"}.
#' @param rcps a character vector of RCP ("rcp26", "rcp45", "rcp60", "rcp85"). Only required if \code{time = "2041-2060"} or \code{time = "2061-2080"}.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @export
#'
#' @return No console output (files are written on the hard drive).
#'
#' @note For future horizons, if no values RCP are specified, projections for all available RCP will downloaded for the chosen GCMs. If no values GCM are specified, projections for all available GCM will downloaded for the chosen RCPs.
#'
#' @examples
#'
#' wget_chelsa(
#'   vars     = c(1, 7, 12, 15),
#'   horizons = c("1979-2013", "2041-2060"),
#'   gcms     = c("CESM1-BGC", "MIROC5"),
#'   rcps     = "rcp85"
#' )


wget_chelsa <- function(path = ".", vars = NULL, horizons = NULL, gcms = NULL, rcps = NULL){


  ## Check if PATH exists   --------

  if (!dir.exists(path)) {

    stop("\nArgument <path>: Path does not exist.")
  }


  ## Check VARIABLES names   --------

  if (is.null(vars)) {

    stop("\nArgument <vars>: Please provide at least one bioclimatic variable between 1 and 19.")
  }

  vars <- gsub("[[:alpha:]]|[[:punct:]]|[[:space:]]", "", vars)

  if (sum(as.numeric(vars) %in% c(1:19)) != length(vars)) {

    stop("\nArgument <vars>: Some bioclimatic variables are misspelled (only 1 to 19 allowed).")
  }


  ## Check GCM / RCP names   --------

  if (!is.null(gcms) || !is.null(rcps)) {

    cmip5_list <- list_chelsa_cmip5()

    if (!is.null(gcms)) {

      if (sum(gcms %in% cmip5_list$gcm) != length(gcms)) {

        stop("\nArgument <gcms>: Some GCM names are misspelled. Run `list_chelsa_cmip5()` for a complete list.")
      }
    }

    if (!is.null(rcps)) {

      if (sum(rcps %in% cmip5_list$rcp) != length(rcps)) {

        stop("\nArgument <rcps>: Some RCP names are misspelled. Run `list_chelsa_cmip5()` for a complete list.")
      }
    }
  }

  if (sum(horizons %in% c("2041-2060", "2061-2080")) > 0) {

    if (is.null(gcms) && is.null(rcps)) {

      stop("Please provide GCM and/or RCP for future horizons.")
    }
  }


  ## Check HORIZONS names   --------

  if (is.null(horizons)) {

    stop("\nArgument <horizons>: Please provide at least one horizon. Possible values: 1979-2013, 2041-2060, 2061-2080.")
  }

  if (sum(horizons %in% c("1979-2013", "2041-2060", "2061-2080")) != length(horizons)) {

    stop("\nArgument <horizons>: Some horizons are misspelled. Possible values: 1979-2013, 2041-2060, 2061-2080.")
  }


  ## Download climate rasters   --------

  miss <- 0

  for (horizon in horizons) {

    if (horizon == "1979-2013") {

      dir.create(
        path          = paste(path, horizon, sep = .Platform$file.sep),
        showWarnings  = FALSE,
        recursive     = TRUE
      )

      # base_url <- "https://www.wsl.ch/lud/chelsa/data/bioclim/integer/CHELSA_bio10_"
      base_url <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_"

      for (var in vars) {

        if (nchar(var) == 1) {

          var <- paste0("0", var)
        }

        download.file(
          url = paste0(
            base_url,
            var,
            ".tif"
          ),
          destfile = paste0(
            paste(
              path,
              horizon,
              "CHELSA_bio10_",
              sep = .Platform$file.sep
            ), var, ".tif"
          )
        )
      }

    } else {

      if (is.null(gcms)) {

        gcms <- cmip5_list$gcm
      }

      if (is.null(rcps)) {

        rcps <- cmip5_list$rcps
      }

      for (gcm in gcms) {

        for (rcp in rcps) {

          if (sum(paste(gcm, rcp, sep = "__") %in% cmip5_list[[horizon]]) == 1) {

            dir.create(
              path          = paste(path, horizon, gcm, rcp, sep = .Platform$file.sep),
              showWarnings  = FALSE,
              recursive     = TRUE
            )

            base_url <- paste0(
              "https://www.wsl.ch/lud/chelsa/data/cmip5/",
              horizon,
              "/bio/CHELSA_bio_mon"
            )

            for (var in vars) {

              if (nchar(var) == 1) {

                varname <- paste0("0", var)

              } else {

                varname <- var
              }

              download.file(
                url       = paste(
                  base_url,
                  gcm,
                  rcp,
                  "r1i1p1",
                  "g025.nc",
                  var,
                  horizon,
                  "V1.2.tif",
                  sep = "_"
                ),
                destfile  = paste0(
                  paste(
                    path,
                    horizon,
                    gcm,
                    rcp,
                    "CHELSA_bio10_",
                    sep = .Platform$file.sep
                  ), varname, ".tif"
                )
              )
            }

          } else {

            miss <- miss + 1
          }
        }
      }
    }
  }

  if (miss > 0) {

    print("Some GCM-RCP combination are not available.")
  }
}

