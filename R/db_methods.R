#' Print methods for objects of class `gcod_db`, as produced by [`get_athlete_ids()`].
#'
#' @param x an object of class `gcod_db`
#' @param txtplot logical indicating whether or not text barplots should be printed for the remote and local perspectives showing the percentage of athlete ID records modified per year quarter. Default is `FALSE`.
#' @param ... currently not used.
#'
#' @aliases print.gcod_remote_db print.gcod_local_db
#'
#' @seealso [`get_athlete_ids()`] [`download_workouts()`] [`extract_workouts()`]
#' @export
print.gcod_db <- function(x, txtplot = FALSE, ...) {
    cat("Remote perspective\n")
    print.gcod_remote_db(remote(x), txtplot, ...)
    cat("\n")
    cat("Local perspective\n")
    print.gcod_local_db(local(x), txtplot, ...)
}

#' @rdname print.gcod_db
#' @export
print.gcod_remote_db <- function(x, txtplot = FALSE, ...) {
    n_ids <- nrow(x)
    if (isTRUE(n_ids == 0)) {
        cat("Number of athlete IDs:", 0, "\n")
    }
    else {
        sizes <- x$size
        ## Not using the min_size, max_size, etc, methods here
        total_size <- sum(sizes)
        min_size <- min(sizes)
        max_size <- max(sizes)
        date_range <- range(x$last_modified)
        cat("Mirror:", attr(x, "mirror"), "\n")
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(x$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' @rdname print.gcod_db
#' @export
print.gcod_local_db <- function(x, txtplot = FALSE, ...) {
    n_ids <- nrow(x)
    if (isTRUE(n_ids == 0)) {
        cat("Number of athlete IDs:", 0, "\n")
    }
    else {
        sizes <- x$size
        ## Not using the min_size, max_size, etc, methods here
        total_size <- sum(sizes)
        min_size <- min(sizes)
        max_size <- max(sizes)
        date_range <- range(x$last_modified)
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(x$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' Extract information from an object of class `gcod_db`, as produced
#' by [`get_athlete_ids()`].
#'
#' @name gcod_db_extractors
#'
#' @param object an object of class `gcod_db`, as produced by
#'     [`get_athlete_ids()`].
#' @param perspective either `"remote"` (default) or `"local"`, for
#'     the perspective to use for the extractor function.
#' @param ... currently not used.
#' 
#' @details
#'
#' File sizes from the `min_size()`, `max_size()`, `total_size()`,
#' `mean_size()` extractors are reported in bytes but inherit from
#' class `object_size`. So, `format.object_size()` can be used
#' directly for pretty units, etc.
#'
#' `local_path()` extracts the local paths from the `gcod_db` file.
#'
#' `remote(object)` and `local(object)` are equivalent to
#' `object$remote_db` and `object$local_db`, respectively.
#'
#' `n_ids()` returns the number of athlete ids in the specified
#' perspective, and `athlete_id()` the athlete IDs themselves.
#' 
#' @seealso [`get_athlete_ids()`]
NULL

#' @rdname gcod_db_extractors
#' @export
n_ids.gcod_db <- function(object, perspective = "remote", ...) {
    switch(perspective,
           "remote" = nrow(remote(object)),
           "local" = nrow(local(object)),
           "`perspective` should be one of 'remote', 'local'")
}

#' @rdname gcod_db_extractors
#' @export
min_size.gcod_db <- function(object, perspective = "remote", ...) {
    n <- n_ids(object, perspective = perspective)
    out <- switch(perspective,
                  "remote" = ifelse(n > 0, min(remote(object)$size), 0),
                  "local" = ifelse(n > 0, min(local(object)$size), 0),
                  "`perspective` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
max_size.gcod_db <- function(object, perspective = "remote", ...) {
    n <- n_ids(object, perspective = perspective)
    out <- switch(perspective,
                  "remote" = ifelse(n > 0, max(remote(object)$size), 0),
                  "local" = ifelse(n > 0, max(local(object)$size), 0),
                  "`perspective` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
total_size.gcod_db <- function(object, perspective = "remote", ...) {
    n <- n_ids(object, perspective = perspective)
    out <- switch(perspective,
                  "remote" = ifelse(n > 0, sum(remote(object)$size), 0),
                  "local" = ifelse(n > 0, sum(local(object)$size), 0),
                  "`perspective` should be one of 'remote', 'local'")
    to_object_size(out)
}


#' @rdname gcod_db_extractors
#' @export
mean_size.gcod_db <- function(object, perspective = "remote", ...) {
    n <- n_ids(object, perspective = perspective)
    out <- switch(perspective,
                  "remote" = ifelse(n > 0, mean(remote(object)$size), 0),
                  "local" = ifelse(n > 0, mean(local(object)$size), 0),
                  "`perspective` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
local_path.gcod_db <- function(object, ...) {
    local(object)$path
}

#' @rdname gcod_db_extractors
#' @export
remote.gcod_db <- function(object, ...) {
    object$remote_db
}

#' @rdname gcod_db_extractors
#' @export
local.gcod_db <- function(object, ...) {
    object$local_db
}

#' @rdname gcod_db_extractors
#' @export
athlete_id.gcod_db <- function(object, perspective = "remote", ...) {
    switch(perspective,
           "remote" = remote(object)$athlete_id,
           "local" = local(object)$athlete_id,
           "`perspective` should be one of 'remote', 'local'")
}
