construct_gcod_db <- function(remote_db, local_db) {
    out <- list(remote_db = remote_db, local_db = local_db)
    class(out) <- c("gcod_db", class(out))
    out
}

#' Print methods for objects of class `gcod_db`, as produced by [`get_athlete_ids()`].
#'
#' @name print.gcod_db
#' @param object an object of class `gcod_db`, `gcod_db` or `gcod_db`
#' @param txtplot logical indicating whether or not text barplots should be printed for the remote and local databases showing the percentage of athlete ID records modified per year quarter. Default is `FALSE`.
#' @param ... currently not used.
#'
#' @aliases print.gcod_remote_db print.gcod_local_db
#' @export
print.gcod_db <- function(object, txtplot = FALSE, ...) {
    cat("Remote db\n")
    print.gcod_remote_db(object$remote_db, txtplot, ...)
    cat("\n")
    cat("Local db\n")
    print.gcod_local_db(object$local_db, txtplot, ...)
}

#' @rdname print.gcod_db
#' @export
print.gcod_remote_db <- function(object, txtplot = FALSE, ...) {
    object <- object
    n_ids <- nrow(object)
    if (isTRUE(n_ids == 0)) {
        cat("Number of athlete IDs:", 0, "\n")
    }
    else {
        sizes <- object$size
        ## Not using the min_size, max_size, etc, methods here
        total_size <- sum(sizes)
        min_size <- min(sizes)
        max_size <- max(sizes)
        date_range <- range(object$last_modified)
        cat("Mirror:", attr(object, "mirror"), "\n")
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(object$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' @rdname print.gcod_db
#' @export
print.gcod_local_db <- function(object, txtplot = FALSE, ...) {
    n_ids <- nrow(object)
    if (isTRUE(n_ids == 0)) {
        cat("Number of athlete IDs:", 0, "\n")
    }
    else {
        sizes <- object$size
        ## Not using the min_size, max_size, etc, methods here
        total_size <- sum(sizes)
        min_size <- min(sizes)
        max_size <- max(sizes)
        date_range <- range(object$last_modified)
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(object$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' Extract information from an object of class `gcod_db`, as produced by [`get_athlete_ids()`].
#' @name gcod_db_extractors
#' @param object an object of class `gcod_db`, as produced by [`get_athlete_ids()`].
#'
#' @details
#' File sizes are reported in bytes but inherit from class `object_size`. So, `format.object_size()` can be used for pretty units, etc.
#'
#' @seealso [`get_athlete_ids()`]
#' @aliases min_size max_size mean_size total_size n_ids
NULL

#' @rdname gcod_db_extractors
#' @export
n_ids.gcod_db <- function(object, db = "remote") {
    switch(db,
           "remote" = nrow(object$remote_db),
           "local" = nrow(object$local_db),
           "`db` should be one of 'remote', 'local'")
}

#' @rdname gcod_db_extractors
#' @export
min_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, min(object$remote_db$size), 0),
                  "local" = ifelse(n > 0, min(object$local_db$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
max_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, max(object$remote_db$size), 0),
                  "local" = ifelse(n > 0, max(object$local_db$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
total_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, sum(object$remote_db$size), 0),
                  "local" = ifelse(n > 0, sum(object$local_db$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}


#' @rdname gcod_db_extractors
#' @export
total_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, sum(object$remote_db$size), 0),
                  "local" = ifelse(n > 0, sum(object$local_db$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}


#' @rdname gcod_db_extractors
#' @export
mean_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, mean(object$remote_db$size), 0),
                  "local" = ifelse(n > 0, mean(object$local_db$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
local_path <- function(object) {
    object$local_db$path
}



## #' @export
## update_local_db.gcod_db <- function(object) {
##     path <- local_path(object)
##     athlete_id <- gsub(".zip", "", basename(path))
##     json_file <- paste0(athlete_id, "/{", athlete_id, "}.json")
##     extracted <- file.exists(file.path(dirname(path), json_file))
##     finfo <- file.info(path)


##     object
## }
## udpate:
## 1. file exists in remote but not in local
## 2. files have been extracted

## #' @export
## check_local_db <- function(object, dir, check_size = TRUE) {
##     gcod_files <- GCOD_files.GCOD_df(object, dir)
##     file_exists <- file.exists(gcod_files$path)
##     equal_sizes <- rep(TRUE, length(file_exists))
##     if (isTRUE(check_size)) {
##         local_file_info <- file.info(gcod_files$path)
##         equal_sizes <- object[, "size"] == local_file_info[, "size"]
##         equal_sizes[is.na(equal_sizes)] <- FALSE
##     }
##     missing_or_different <- !file_exists | !equal_sizes
##     object[]
