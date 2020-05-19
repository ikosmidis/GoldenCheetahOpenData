#' Download and, optionally, extract the archive with the workouts for a particular athlete ID
#'
#' @inheritParams get_athlete_ids
#' @param athlete_id a character string with the athlete ID or the first few characters of it, or an object of class `GCOD_df` as constructed by `get_athlete_ids()`.
#' @param dir the directory to download the zip files for the selected athleted IDs.
#' @param pattern character string containing a regular expression to be matched with the athlete IDs in `athlete_id`. Only applicable if `athlete_id` is an object of class `GCOD_df`. Default is `NULL`, which selects all IDs in `athlete_id`.
#' @param extract logical determining whether the workout files in the downloaded archives should be extracted. Default is `FALSE`. If `TRUE`, then the archives are extractred in sub-directories unded `dir`. The sub-directories are named according to the athlete ID.
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param confirm logical determining whether the user should be asked whether they should continue with the download or not. Default is `TRUE`.
#' @param ... extra arguments to be passed to `aws.s3::save_object()`.
#'
#' @examples
#' \donttest{
#' oo <- download_workouts("000d")
#' }
download_workouts <- function(athlete_id,
                              dir = tempdir(),
                              pattern = NULL,
                              extract = FALSE,
                              mirror = "S3",
                              verbose = TRUE,
                              confirm = FALSE,
                              ...) {
    mirror <- match.arg(mirror, c("OSF", "S3"))
    if (!dir.exists(dir)) {
        stop("'", dir, "' is not a valid path.")
    }
    if (inherits(athlete_id, "GCOD_df")) {
        sizes <- athlete_id$size
        athlete_id <- athlete_id$athlete_id
        if (!is.null(pattern)) {
            athlete_id <- athlete_id[grepl(pattern, athlete_id)]
        }
    }
    else {
        if (length(athlete_id) > 1) {
            stop("length(athlete_id) should be 1.")
        }
        athlete_id <- get_athlete_ids(mirror = mirror, prefix = athlete_id)
        sizes <- athlete_id$size
        athlete_id <- athlete_id$athlete_id
    }
    ## Download
    n_ids <- length(athlete_id)
    if (n_ids == 0) {
        stop("There are no athlete IDs to download.")
    }   
    if (isTRUE(confirm)) {
        total_size <- sum(sizes)
        class(total_size) <- "object_size"
        out <- askYesNo(paste("Continue downloading", format(total_size, units = "auto"), "of workout data for", n_ids, "athlete IDs?"))
        if (!isTRUE(out)) {
            return(NULL)
        }
    }
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        object <- paste0("data/", athlete_id, ".zip")
        file_names <- basename(object)
        path <- file.path(dir, file_names)
        for (j in seq.int(n_ids)) {
            if (verbose) {
                message(paste("Downloading", file_names[j], "... "), appendLF = FALSE)
            }
            save_object(object[j],
                        bucket = gc_bucket,
                        file = path[j], ...)
            if (verbose) {
                message("Done.")
            }
        }
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented yet.")
    }   
    out <- list(path = path,
                extracted = FALSE,
                mirror = mirror)
    class(out) <- "GCOD_files"
    if (isTRUE(extract)) {
        out <- extract_workouts.GCOD_files(out, verbose)
    }    
    out
}


#' @param object
#' @param verbose
#' @param overwrite
#' @details
#' Athelte IDs will be infered from `object$path`
extract_workouts.GCOD_files <- function(object, verbose = FALSE, overwrite = TRUE) {
    path <- object$path
    n_paths <- length(path)
    athlete_id <- gsub(".zip", "", basename(path))
    for (j in seq.int(n_paths)) {
        current_path <- path[j]
        current_dir <- dirname(current_path)
        extraction_dir <- paste0(current_dir, "/", athlete_id[j])
        if (verbose) {
            message(paste("Extracting", current_path, "... "), appendLF = FALSE)
        }
        if (!isTRUE(overwrite)) {
            if (file.exists(extraction_dir)) {
                stop("Directory", extraction_dir, "exists. Use `overwrite = TRUE` to overwrite.")
            }
        }        
        unzip(current_path,
              overwrite = overwrite,
              exdir = extraction_dir)
        if (verbose) {
            message("Done.\n")
        }
    }
    object$extracted <- TRUE
    object
}

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


