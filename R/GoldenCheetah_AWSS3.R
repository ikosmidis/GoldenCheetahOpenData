#' List available athletes
#'
#' @param details if `FALSE` a vector with the available athlete IDs is returned. If `TRUE`, then a data frame with athlete IDs and accompnying details is returned. See Details.
#'
#' @details
#'
#' The `data.frame` that is returned if `details = TRUE` has
#' "Key", "LastModified", "ETag", "Size", "Size", "Owner_ID",
#' "Owner_DisplayName", "StorageClass", "Bucket", "athlete_id"
#'
get_athlete_ids <- function(details = TRUE,
                            source = "S3") {
    source <- match.arg(source, c("OSF", "S3"))

    if (source == "S3") {
        gc_bucket <- 'goldencheetah-opendata'
        athlete_prefix <- "data/"
        athlete_dir <- get_bucket_df(bucket = gc_bucket,
                                     prefix = athlete_prefix,
                                     max = Inf)
        ## Get athlete id's
        athlete <- gsub(".zip|data/", "", athlete_dir[, 1])
        base_dir_index <- match("", athlete, nomatch = 0)
        athlete_dir$athlete_id <- athlete
        ## Assuming tz in amazon S3 is UTC
        athlete_dir$LastModified <- as.POSIXct(athlete_dir$LastModified, tz = "UTC",
                                               format = "%Y-%m-%dT%H:%M:%S.000Z")
        athlete_dir <- athlete_dir[-base_dir_index, ]
    }
    if (source == "OSF") {
    }
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@14@"]]));##:ess-bp-end:##



    class(out) <- c("gc_opendata_athletes", class(out))
    attr(out, "details") <- details
    out
}

#' Return athlete IDs that have been modified between two dates
#'
#' @inheritParams get_athlete_ids
#' @param object an `object` of class `gc_opendata_athletes`. If `NULL` (default) then the output of `get_athletes(TRUE)` is used
#' @param from a character string of the form "YYYY-MM-DD" specifying the from date. If `-Inf` (default) then the minimum date from `object$LastModified`
#' @param to a character string of the form "YYYY-MM-DD" specifying the to date. If `Inf` (default) then the maximum date from `object$LastModified`
between.gc_opendata_athletes <- function(object = NULL, from = -Inf, to = +Inf, details = TRUE) {
    stopifnot("object is not of class gc_opendata_athletes" = inherits(object, "gc_opendata_athletes"))
    stopifnot("Use details = TRUE in get_athletes" = attr(object, "details"))
    if (is.null(object)) {
        object <- get_athletes(details = TRUE)
    }
    from <- if (from == -Inf)
                min(object$LastModified)
            else
                as.POSIXct(from, tz = "UTC", format = "%Y-%m-%d")
    to <- if (from == Inf)
              max(object$LastModified)
          else
              ## to + 1 day - 1 second
              as.POSIXct(to, tz = "UTC", format = "%Y-%m-%d") + 60 * 60 * 24 - 1
    out <- subset(object, (LastModified >= from) & LastModified <= to)
    if (!details) {
        out <- out$athlete_id
    }
    class(out) <- c("gc_opendata_athletes", class(out))
    attr(out, "details") <- details
    out
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


