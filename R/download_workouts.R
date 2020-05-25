#' Download and, optionally, extract the workout archives the athlete IDs in the remote perspective of a `gcod_id` object.
#'
#' @inheritParams get_athlete_ids
#' @param athlete_id a character string with the athlete ID or the first few characters of that, or alternatively an object of class `gcod_db` as produced by [`get_athlete_ids()`].
#' @param local_dir the directory to download the workout archives for the selected athlete IDs.
#' @param pattern character string containing a regular expression to be matched with the athlete IDs in `athlete_id`. Only applicable if `athlete_id` is an object of class `gcod_db`. Default is `NULL`, which selects all IDs in `athlete_id`.
#' @param extract logical determining whether the workout files in the downloaded archives should be extracted. Default is `FALSE`. If `TRUE`, then the archives are extractred in sub-directories unded `local_dir`, named according to the athlete IDs. See Details.
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param confirm logical determining whether the user should be asked whether they should continue with the download or not. Default is `TRUE`.
#' @param overwrite logical determining whether existing archives with the same names as the ones selected for download should be overwritten. Default is `TRUE`.
#' @param ... extra arguments to be passed to [`aws.s3::save_object()`].
#'
#' @details
#' If `extract = TRUE`, then [`extract_workouts()`] is called with `clean_up = TRUE` and `overwrite = TRUE`.
#'
#' `mirror = OSF` currently returns an error and will be supported in future versions.
#'
#' @return
#'
#' If `athlete_id` is a character string then a `gcod_db` object is
#' return with corresponding remote and local perspective. If
#' `athlete_id` is an object of class `gcod_db`, then `athlete_id` is
#' returned with the elements of `local(object)$downloaded` set to
#' `TRUE` or `FALSE` depending on whether the corresponding workout
#' archives were downloaded successfully or not.
#'
#' @seealso
#' [`get_athlete_ids()`] [`extract_workouts()`]
#'
#' @references
#' Liversedge, M. (2020). GoldenCheetah OpenData Project. OSF. \url{https://doi.org/10.17605/OSF.IO/6HFPZ}
#'
#' @examples
#' \donttest{
#' ## Using the first few letters of the athlete ID
#' if (interactive) {
#'    files_007_1 <- download_workouts("007", confirm = TRUE)
#' }
#'
#' ## Using a `gcod_db` object and fitering using regex
#' ids_00 <- get_athlete_ids(prefix = "00")
#' if (interactive) {
#'    files_007_2 <- download_workouts(ids_00, pattern = "007", confirm = TRUE)
#' }
#'
#' }
download_workouts <- function(object,
                              local_dir = tempdir(),
                              pattern = NULL,
                              extract = FALSE,
                              mirror = "S3",
                              verbose = FALSE,
                              confirm = FALSE,
                              overwrite = FALSE,
                              ...) {
    mirror <- match.arg(mirror, c("OSF", "S3"))
    if (!dir.exists(local_dir)) {
        stop("'", local_dir, "' does not exist.")
    }
    if (inherits(object, "gcod_db")) {

        sizes <- remote(object)$size
        athlete_id <- remote(object)$athlete_id
        if (!is.null(pattern)) {
            inds <- grepl(pattern, athlete_id)
            sizes <- sizes[inds]
            athlete_id <- athlete_id[inds]
            object$remote_db <- object$remote_db[inds, ]
            attr(object$remote_db, "mirror") <- mirror
        }
    }
    else {
        if (length(object) > 1) {
            stop("Vectors of character strings are not supported for `object`.")
        }
        object <- get_athlete_ids(mirror = mirror, prefix = object)
        sizes <- remote(object)$size
        athlete_id <- athlete_id(object, perspective = "remote")
    }
    ## Download
    n_ids <- length(athlete_id)
    downloaded <- logical(n_ids)
    if (n_ids == 0) {
        stop("There are no athlete IDs to download.")
    }
    if (isTRUE(confirm)) {
        total_size <- sum(sizes)
        out <- askYesNo(paste("Attempting to download", format_object_size(total_size), "of workout data for", n_ids, "athlete IDs. Procced?"))
        if (!isTRUE(out)) {
            return(NULL)
        }
    }
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        s3_path <- paste0("data/", athlete_id, ".zip")
        file_names <- basename(s3_path)
        path <- file.path(local_dir, file_names)
        for (j in seq.int(n_ids)) {
            if (file.exists(path[j]) & !overwrite) {
                if (verbose) {
                    message(paste(file_names[j], "exists and `overwrite = FALSE`. Skipping."),
                            appendLF = TRUE)
                }
                downloaded[j] <- TRUE
                next
            }
            if (verbose) {
                current_size <- to_object_size(sizes[j])
                message(paste("Downloading", file_names[j],
                              paste0("(", format_object_size(sizes[j]),")"), "... "),
                        appendLF = FALSE)
            }
            ## Test object exists
            s3_attempt <- try(object_exists(s3_path[j],
                                            bucket = gc_bucket),
                              silent = TRUE)
            if (inherits(s3_attempt, "try-error")) {
                warning(paste("Failed to download", file_names[j]))
                if (verbose) {
                    message("Failed.", appendLF = TRUE)
                }
                next
            }
            else {
                save_object(s3_path[j],
                            bucket = gc_bucket,
                            file = path[j],
                            overwrite = TRUE,
                            ...)
                downloaded[j] <- TRUE
            }
            if (verbose) {
                message("Done.", appendLF = TRUE)
            }
        }
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented in the curretn version of GoldenCheetahOpenData.")
    }

    finfo <- file.info(path)
    local_db <- data.frame(path = path,
                           last_modified = finfo$mtime,
                           size = finfo$size,
                           extracted = FALSE,
                           downloaded = downloaded,
                           athlete_id = athlete_id,
                           stringsAsFactors = FALSE)

    object <- make_gcod_db(remote(object), local_db,
                           attr(remote(object), "mirror"))

    if (isTRUE(extract)) {
        object <- extract_workouts(object, verbose, clean_up = TRUE, overwrite = TRUE)
    }

    object
}
