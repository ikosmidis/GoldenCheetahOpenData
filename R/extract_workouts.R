#' Extract workouts from the archives downloaded using [`download_workouts()`].
#'
#' @rdname extract_workouts
#' @param object an object of class `GCOD_files` as produced from [`download_workouts()`].
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param clean_up logical determining whether the workout directories should be deleted before extraction, if they already exist. Default is `FALSE`.
#' @param overwrite logical determining whether the workout directories should be overwritten, if they already exist. Default is `TRUE`.
#'
#' @return
#' An object of class `GCOD_files` which is the same as `object` except that `object$extracted = TRUE`.
#'
#' @details
#' Athlete IDs are infered from `object$path`.
#'
#' @seealso
#' [`download_workouts()`]
#'
#' @export
extract_workouts.GCOD_files <- function(object,
                                        verbose = FALSE,
                                        clean_up = FALSE,
                                        overwrite = TRUE) {
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
        if (isTRUE(clean_up)) {
            unlink(extraction_dir, recursive = TRUE, force = TRUE)
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
