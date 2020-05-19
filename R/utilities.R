#' Extract information from an object of class `GCOD_df`, as produced by [`get_athlete_ids()`].
#' @name GCOD_df_info
#' @param object an object of class `GCOD_df`, as produced by [`get_athlete_ids()`].
#'
#' @details
#' File sizes are reported in bytes but inherit from class `object_size`. So `format.object_size()` can be used for pretty units, etc.
#'
#' @seealso [`get_athlete_ids()`]
#' @aliases min_size max_size mean_size total_size n_ids
NULL

#' @rdname GCOD_df_info
#' @export
n_ids.GCOD_df <- function(object) {
    nrow(object)
}

#' @rdname GCOD_df_info
#' @export
min_size.GCOD_df <- function(object) {
    out <- min(object$size)
    class(out) <- "object_size"
    out
}

#' @rdname GCOD_df_info
#' @export
max_size.GCOD_df <- function(object) {
    out <- max(object$size)
    class(out) <- "object_size"
    out
}

#' @rdname GCOD_df_info
#' @export
total_size.GCOD_df <- function(object) {
    out <- sum(object$size)
    class(out) <- "object_size"
    out
}

#' @rdname GCOD_df_info
#' @export
mean_size.GCOD_df <- function(object) {
    out <- mean(object$size)
    class(out) <- "object_size"
    out
}

#' Return athlete IDs that have been modified between two dates.
#'
#' @param object an `object` of class `GCOD_df`, as produced by [`get_athlete_ids`]. If `NULL` (default) then the output of a call to [`get_athletes()`] is used.
#' @param from a character string of the form "YYYY-MM-DD" specifying the from date. If `-Inf` (default) then the minimum date from `object$LastModified`
#' @param to a character string of the form "YYYY-MM-DD" specifying the to date. If `Inf` (default) then the maximum date from `object$LastModified`
between.GCOD_df <- function(object = NULL, from = -Inf, to = +Inf) {
    stopifnot("object is not of class gc_opendata_athletes" = inherits(object, "gc_opendata_athletes"))
    if (is.null(object)) {
        object <- get_athletes()
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
    class(out) <- c("GCOD_df", class(out))
    attr(out, "details") <- details
    out
}
