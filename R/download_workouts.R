#' Download and, optionally, extract the archive with the workouts for a particular athlete ID in the GoldenCheetah OpenData project.
#'
#' @inheritParams get_athlete_ids
#' @param athlete_id a character string with the athlete ID or the first few characters of it, or alternatively an object of class `gcod_db` as constructed by [`get_athlete_ids()`].
#' @param dir the directory to download the zip files for the selected athlete IDs.
#' @param pattern character string containing a regular expression to be matched with the athlete IDs in `athlete_id`. Only applicable if `athlete_id` is an object of class `gcod_db`. Default is `NULL`, which selects all IDs in `athlete_id`.
#' @param extract logical determining whether the workout files in the downloaded archives should be extracted. Default is `FALSE`. If `TRUE`, then the archives are extractred in sub-directories unded `dir`. The sub-directories are named according to the athlete ID. See Details.
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param confirm logical determining whether the user should be asked whether they should continue with the download or not. Default is `TRUE`.
#' @param overwrite logical determining whether existing archives with the same names as the ones to be downloaded should be overwritten. Default is `TRUE`.
#' @param ... extra arguments to be passed to [`aws.s3::save_object()`].
#'
#' @details
#' If `extract = TRUE`, then [`extract_workouts()`] is called with `clean_up = TRUE` and `overwrite = TRUE`.
#'
#' @seealso
#' [`extract_workouts()`]
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
        sizes <- object$remote_db$size
        athlete_id <- object$remote_db$athlete_id
        if (!is.null(pattern)) {
            inds <- grepl(pattern, athlete_id)
            sizes <- sizes[inds]
            athlete_id <- athlete_id[inds]
            object$remote_db <- object$remote_db[inds, ]
        }
    }
    else {
        if (length(object) > 1) {
            stop("Vectors of character strings are not supported for `object`.")
        }
        object <- get_athlete_ids(mirror = mirror, prefix = object)
        sizes <- object$remote_db$size
        athlete_id <- object$remote_db$athlete_id
    }
    ## Download
    n_ids <- length(athlete_id)
    if (n_ids == 0) {
        stop("There are no athlete IDs to download.")
    }
    if (isTRUE(confirm)) {
        total_size <- sum(sizes)
        out <- askYesNo(paste("Continue downloading", format_object_size(total_size), "of workout data for", n_ids, "athlete IDs?"))
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
                message(file_names[j], " exists and `overwrite = FALSE`. Skipping.")
                next
            }
            if (verbose) {
                current_size <- to_object_size(sizes[j])
                message(paste("Downloading", file_names[j],
                              paste0("(", format_object_size(sizes[j]),")"), "... "),
                        appendLF = FALSE)
            }
            save_object(s3_path[j],
                        bucket = gc_bucket,
                        file = path[j],
                        overwrite = TRUE,
                        ...)
            if (verbose) {
                message("Done.")
            }
        }
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented yet.")
    }

    finfo <- file.info(path)
    object$local_db <- data.frame(path = path,
                                  last_modified = finfo$mtime,
                                  size = finfo$size,
                                  extracted = FALSE,
                                  downloaded = TRUE,
                                  athlete_id = athlete_id,
                                  stringsAsFactors = FALSE)

    if (isTRUE(extract)) {
        object <- extract_workouts(object, verbose, clean_up = TRUE, overwrite = TRUE)
    }
    object
}
