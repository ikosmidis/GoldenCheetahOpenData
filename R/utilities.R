#' Return athlete IDs that have been modified between two dates
#'
#' @param object an `object` of class `gc_opendata_athletes`. If `NULL` (default) then the output of `get_athletes(TRUE)` is used
#' @param from a character string of the form "YYYY-MM-DD" specifying the from date. If `-Inf` (default) then the minimum date from `object$LastModified`
#' @param to a character string of the form "YYYY-MM-DD" specifying the to date. If `Inf` (default) then the maximum date from `object$LastModified`
between.gc_opendata_athletes <- function(object = NULL, from = -Inf, to = +Inf) {
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
