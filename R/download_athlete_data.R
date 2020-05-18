#' @inheritParams get_athlete_ids
#' @param athlete_id
#' @param dir
#' @param extract
download_athlete_data <- function(athlete_id,
                                  dir = tempdir(),
                                  extract = FALSE,
                                  mirror = "S3",
                                  verbose = TRUE,
                                  ...) {
    if (!dir.exists(dir)) {
        stop("'", dir, "' is not a valid path")
    }
    mirror <- match.arg(mirror, c("OSF", "S3"))
    ## Download
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        object <- paste0("data/", athlete_id, ".zip")
        path <- file.path(dir, basename(object))
        for (j in seq_along(object)) {
            if (verbose) {
                cat(path[j], "\n")
                cat("Downloading... ")
            }
            save_object(object[j],
                        bucket = gc_bucket,
                        file = path[j], ...)           
            if (isTRUE(extract)) {
                extraction_dir <- paste0(dir, "/", athlete_id[j])
                if (verbose) {
                    cat("Extracting... ")
                }
                unzip(path[j],
                      overwrite = TRUE,
                      exdir = extraction_dir)
            }
            if (verbose) {
                cat("Done.\n")
            }
        }
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented yet")
    }
    ## extraction
    path
}

get_athlete_data <- function(athlete_id) {
    gc_bucket <- 'goldencheetah-opendata'
    object <- paste0("data/", athlete_id, ".zip")
    tmp_dir <- tempdir()
    tmp_file <- paste0(tmp_dir, "/", basename(object))
    tmp_extraction_dir <- paste0(tmp_dir, "/", athlete_id)
    json <- paste0("{", athlete_id, "}.json")
    tmp_json <- paste0(tmp_extraction_dir, "/", json)
    save_object(object, bucket = gc_bucket, file = tmp_file)
    unzip(tmp_file, overwrite = TRUE, exdir = tmp_extraction_dir)
    js <- jsonlite::read_json(tmp_json)
    json_dates <- as.POSIXct(sapply(js$RIDES, function(x) x$date),
                             format = "%Y/%m/%d %H:%M:%S UTC",
                             tz = "UTC")
    zip_files <- dir(tmp_extraction_dir)
    zip_files <- zip_files[-match(json, zip_files)]
    zip_dates <- as.POSIXct(zip_files,
                            format = "%Y_%m_%d_%H_%M_%S.csv",
                            tz = "")
    ## ensure zip_dates and json_dates are ordered and assume 1-1 match
    o_zip_dates <- order(zip_dates)
    zip_dates <- zip_dates[o_zip_dates]
    zip_files <- zip_files[o_zip_dates]
    o_json_dates <- order(json_dates)
    json_dates <- json_dates[o_json_dates]
    sports <- sapply(js$RIDES, function(x) trackeR:::guess_sport(x$sport))
    sports <- sports[o_json_dates]
    spec <- cols(
        secs = col_double(),
        km = col_double(),
        power = col_double(),
        hr = col_double(),
        cad = col_double(),
        alt = col_double())
    nsess <- length(sports)
    sessions <- as.list(numeric(nsess))
    for (j in seq_along(sports)) {
        if (is.na(sports[j])) next
        cat(paste0(j, "/", nsess), "Reading file", zip_files[j], "|", "sport", sports[j], "|\n")
        current_data <- read_csv(paste0(tmp_extraction_dir, "/", zip_files[j]),
                                 col_types = spec)
        n <- nrow(current_data)
        current_data <- with(current_data, {
            data.frame(
                time = json_dates[j] + secs,
                latitude = rep(NA, n),
                longitude = rep(NA, n),
                altitude = alt,
                distance = km,
                heart_rate = hr,
                speed = rep(NA, n),
                cadence_running = rep(NA, n),
                cadence_cycling = cad,
                power = power,
                temperature = rep(NA, n))
        })
        attr(current_data, "file") <- zip_files[j]
        units0 <- generate_units(variable = "distance", unit = "km", sport = sports[j])
        current_session <- trackeRdata(current_data, units = units0, sport = sports[j])
        sessions[[j]] <- current_session
    }
    sessions <- do.call("c", sessions)
    ## delete files, but keep zip if necessaryt
    ## add progress option
}


