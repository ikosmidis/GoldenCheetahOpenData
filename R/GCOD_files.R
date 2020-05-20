construct_GCOD_files <- function(path, extracted, downloaded, mirror, db_state) {
    stopifnot("Wrong `db_state`. Use `get_athlete_ids()` to define `db_state`" = inherits(db_State, "GCOD_df"))
    out <- list(path = path,
                extracted = extracted,
                downloaded = downloaded,
                mirror = mirror,
                db_state = db_state)
    class(out) <- "GCOD_files"
    out
}

#' @param object a `GCOD_df` object
GCOD_files.GCOD_df <- function(object, dir) {
    if (attr(object, "mirror") == "S3") {
        path <- file.path(dir, gsub("data/", "", object$key))
    }
    if (attr(object, "mirror") == "OSF") {
        stop("OSF is not implemented yet.")
    }
    construct_GCOD_files(path = path,
                         extracted = FALSE,
                         downloaded = FALSE,
                         mirror = attr(object, "mirror"),
                         db_state = object)
}


#' @param object `GCOD_df` object
#'
#' @export
check_local_db <- function(object, dir, check_size = TRUE) {
    gcod_files <- GCOD_files.GCOD_df(object, dir)
    file_exists <- file.exists(gcod_files$path)
    equal_sizes <- rep(TRUE, length(file_exists))
    if (isTRUE(check_size)) {
        local_file_info <- file.info(gcod_files$path)
        equal_sizes <- object[, "size"] == local_file_info[, "size"]
        equal_sizes[is.na(equal_sizes)] <- FALSE
    }
    missing_or_different <- !file_exists | !equal_sizes
    object[]



}

sync_local_db <- function(object, dir) {

}
