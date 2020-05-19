#' Download and, optionally, extract the archive with the workouts for a particular athlete ID
#'
#' @rdname download_workouts
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
