#' Extract workouts from the archives downloaded using [`download_workouts()`].
#'
#' @rdname extract_workouts
#' @param object an object of class `gcod_db` as produced from [`download_workouts()`].
#' @param verbose logical determining whether progress information should be printed. Default is `FALSE`.
#' @param clean_up logical determining whether the workout directories should be deleted before extraction, if they already exist. Default is `FALSE`.
#' @param overwrite logical determining whether the workout directories should be overwritten, if they already exist. Default is `TRUE`.
#'
#' @return
#' An object of class `gcod_db` which is the same as `object` except that `object$extracted = TRUE`.
#'
#' The workouts are extracted and put in sub-directories in the same directory as the original archive (i.e. where the files `object$path` are). These sub-directories have exactly the same name as the archives (excluding the file extension).
#'
#' @details
#' Athlete IDs are infered from `object$path`.
#'
#' Only modifies `local(object)$extracted` depending on whether the workout archives, were extracted successfully or not.
#'
#' @seealso
#' [`download_workouts()`]
#'
#' @export
extract_workouts.gcod_db <- function(object,
                                     verbose = FALSE,
                                     clean_up = TRUE,
                                     overwrite = TRUE) {
    path <- local_path(object)
    if (length(path) == 0) {
        stop("The are no references to local files in `object`. Run `download_workouts(object)` first.")
    }
    n_paths <- length(path)
    athlete_id <- athlete_id(object, db = "local")
    for (j in seq.int(n_paths)) {
        current_path <- path[j]
        current_dir <- dirname(current_path)
        extraction_dir <- paste0(current_dir, "/", athlete_id[j])
        if (verbose) {
            message(paste("Extracting", current_path, "... "), appendLF = FALSE)
        }
        if (!isTRUE(overwrite)) {
            if (file.exists(extraction_dir)) {
                message("Exists.", appendLF = TRUE)
                next
            }
        }
        if (isTRUE(clean_up)) {
            unlink(extraction_dir, recursive = TRUE, force = TRUE)
        }
        unzip_attempt <- tryCatch({
            unzip(current_path,
                  overwrite = overwrite,
                  exdir = extraction_dir)
        },
        warning = function(w) {
            w
        })

        if (inherits(unzip_attempt, "warning")) {
            object$local_db[athlete_id == athlete_id[j], "extracted"] <- FALSE
            if (verbose) {
                message("Failed.", appendLF = TRUE)
            }
            warning(unzip_attempt)
            next
        }
        else {
            object$local_db[athlete_id == athlete_id[j], "extracted"] <- TRUE
            if (verbose) {
                message("Done.", appendLF = TRUE)
            }
        }
    }
    object
}
