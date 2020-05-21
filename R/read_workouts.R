#' Read the workout `.csv` files that have been extracted using [`extract_workouts()`] and organized them into lists of [`trackeR::trackeRdata`] objects for further processing into the `trackeR` R package.
#'
#' @param object an object of class `GCOD_files` as produced from [`download_workouts()`] or [`extract_workouts()`].
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param ... other arguments to be passed to [`extract_workouts()`]
#' @rdname read_workouts
#'
#' @details
#' If `object$extracted` is `FALSE`, then the workout files are extracted automatically using [`extract_workouts()`].
#'
#' It is assumed that the filename for each workout corresponds to the timestamp where the first observation is made for the session. Timestamps are in UTC.
#'
#' If the number of workout files in the archive of a particular athlete ID does not match the number of workouts recorded in the json files within that archive, then the workout files for the ID are not read.
#'
#' @return
#' A list of [`trackeR::trackeRdata`] objects.
#'
#' @seealso
#' [`trackeR::trackeRdata()`]
#'
#' @references
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in R. *Journal
#' of Statistical Software*, **82**(7),
#' 1--29. [doi:10.18637/jss.v082.i07](https://doi.org/10.18637/jss.v082.i07)
#'
#' @export
read_workouts.GCOD_files <- function(object, verbose = FALSE, delete = FALSE, ...) {
    if (!isTRUE(object$extracted)) {
        extract_workouts.GCOD_files(object, verbose = FALSE, ...)
    }
    path <- object$path
    athlete_id <- gsub(".zip", "", basename(path))
    n_ids <- length(athlete_id)
    extraction_dir <- paste0(dirname(path), "/", athlete_id)

    units0 <- trackeR::generate_units(
                           variable = c(rep(c("distance", "speed", "pace", "altitude"), 3),  "cadence_running"),
                           unit = c(rep(c("km", "km_per_h", "min_per_km", "m"), 3), "steps_per_min"),
                           sport = c(rep(c("cycling", "running", "swimming"), each = 4), "running"))


    process_id <- function(extraction_dir, athlete_id) {
        json <- paste0("{", athlete_id, "}.json")
        js <- jsonlite::read_json(file.path(extraction_dir, json))
        json_dates <- as.POSIXct(sapply(js$RIDES, function(x) x$date),
                                 format = "%Y/%m/%d %H:%M:%S UTC",
                                 tz = "UTC")
        csv_files <- dir(extraction_dir)
        csv_files <- csv_files[-match(json, csv_files)]
        csv_dates <- as.POSIXct(csv_files,
                                format = "%Y_%m_%d_%H_%M_%S.csv",
                                tz = "")
        if (length(json_dates) != length(csv_dates)) {
            warning("Number of workout files for ID ", athlete_id,  " does not match the number of workouts in ", json, ". Workouts have not been read.")
            return(NA)
        }
        ## ensure csv_dates and json_dates are ordered and assume 1-1 match
        o_csv_dates <- order(csv_dates)
        csv_dates <- csv_dates[o_csv_dates]
        csv_files <- csv_files[o_csv_dates]
        o_json_dates <- order(json_dates)
        json_dates <- json_dates[o_json_dates]
        sports <- sapply(js$RIDES, function(x) trackeR:::guess_sport(x$sport))
        sports <- sports[o_json_dates]
        spec <- readr::cols(
            secs = readr::col_double(),
            km = readr::col_double(),
            power = readr::col_double(),
            hr = readr::col_double(),
            cad = readr::col_double(),
            alt = readr::col_double())
        nsess <- length(sports)
        sessions <- as.list(numeric(nsess))
        ## If js rides are more than files stop

        ## Read the files sequentially
        for (j in seq_along(sports)) {
            if (is.na(sports[j])) {
                warning("missing sport information for ",
                        paste("ID", athlete_id), " file ",
                        csv_files[j], ". Skipping.")

                next
            }
            if (verbose) {
                message(paste("ID", athlete_id, "|",
                              "reading", csv_files[j],
                              paste0("(", j, "/", nsess, ")"),
                              "|", sports[j]), appendLF = TRUE)
            }
            csv_file <- file.path(extraction_dir, csv_files[j])
            current_data <- readr::read_csv(csv_file,
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
                    cadence_running = if (sports[j] == "running") 2 * cad else rep(NA, n),
                    cadence_cycling = if (sports[j] == "cycling") cad else rep(NA, n),
                    power = power,
                    temperature = rep(NA, n))
            })
            attr(current_data, "file") <- csv_file
            current_session <- trackeR::trackeRdata(current_data, units = units0, sport = sports[j])
            sessions[[j]] <- current_session
        }

        inds <- identical(sessions, as.list(numeric(nsess)))
        if (inds) {
            return(NA)
        }
        else {
            return(do.call("c", sessions[!inds]))
        }
    }

    out <- list()
    for (k in seq.int(n_ids)) {

        out[[athlete_id[k]]] <- process_id(extraction_dir[k], athlete_id[k])
    }
    names(out) <- athlete_id
    out

}