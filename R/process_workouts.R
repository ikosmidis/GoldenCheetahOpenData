#'
#'
process_workouts.GCOD_files <- function(object, verbose = FALSE) {
    if (!isTRUE(object$extracted)) {
        extract_workouts.GCOD_files(object, verbose = FALSE)
    }
    path <- object$path
    n_paths <- length(path)
    athlete_id <- gsub(".zip", "", basename(path))
    extraction_dir <- paste0(dirname(path), "/", athlete_id)

    process_id <- function(extraction_dir, athlete_id) {
        json <- paste0(extraction_dir, "/", "{", athlete_id, "}.json")
        js <- jsonlite::read_json(json)
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
        ## Read the files sequentially
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
}


