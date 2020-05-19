#' Retrieve information for the available athleted IDs in the GoldenCheetah OpenData project.
#'
#' @param n_ids integer indicating the maximum number of athlete IDs to return. Default is `Inf`, which will return all available athelte IDs.
#' @param mirror either `"S3"` or `"OSF"`, indicating the GoldeCheetah OpenData mirror to use. Default and recommended is "S3". See Details.
#' @param prefix character string that limits the response to athlete IDs that begin with it. Default is `NULL`, which does not limit responses.
#' @param ... further options to be passed to `[aws.s3::get_bucket_df()]`.
#' @details
#'
#' @return
#' A [`data.frame`] inheriting from class `GCOD_df` with variables
#' "key", "last_modified", "e_tag", "size", "owner_id",
#' "owner_display_name", "storage_class", "bucket", and "athlete_id".
#'
#' @seealso [`print.GCOD_df()`] [`min_size.GCOD_df()`] [`max_size.GCOD_df()`] [`total_size.GCOD_df()`] [`mean_size.GCOD_df()`] [`n_ids.GCOD_df()`]
#'
#' @references
#' Liversedge, M. (2020). GoldenCheetah OpenData Project. OSF. \url{https://doi.org/10.17605/OSF.IO/6HFPZ}
#'
#' @examples
#'
#' \donttest{
#' # Get details for all available athlete IDs
#' ids <- get_athlete_ids()
#' # Print basic info for the IDs retrieved
#' print(ids, txtplot = TRUE)
#' # Extract basic info for the IDs retrieved
#' n_ids(ids)
#' format(min_size(ids), unit = "auto")
#' format(max_size(ids), unit = "auto")
#' format(mean_size(ids), unit = "auto")
#' format(total_size(ids), unit = "auto")
#'
#' # `ids` inherits from `data.frame` so we can easily find which IDs
#' # have the minimum/maximum sizes (etc)
#' ids[which.min(ids$size), "athlete_id"]
#' ids[which.max(ids$size), "athlete_id"]
#'
#' }
#'
#' @export
get_athlete_ids <- function(n_ids = Inf,
                            mirror = "S3",
                            prefix = NULL,
                            ...) {
    mirror <- match.arg(mirror, c("OSF", "S3"))
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        athlete_prefix <- paste0("data/", prefix)
        out <- get_bucket(bucket = gc_bucket,
                          prefix = athlete_prefix,
                          max = n_ids + 1,
                          ...)
        out <- as.data.frame(out, stringsAsFactors = FALSE)
        ## Get athlete id's
        athlete <- gsub(".zip|data/", "", out[, 1])
        base_dir_index <- match("", athlete, nomatch = 0)
        out$athlete_id <- athlete
        ## Assuming tz in amazon S3 is UTC
        out$LastModified <- as.POSIXct(out$LastModified, tz = "UTC",
                                       format = "%Y-%m-%dT%H:%M:%S.000Z")
        if (base_dir_index) {
            out <- out[-base_dir_index, ]
        }
        names(out) <- c("key", "last_modified", "e_tag", "size", "owner_id", "owner_display_name", "storage_class", "bucket", "athlete_id")
        out$size <- as.numeric(out$size)
        if (isTRUE(nrow(out))) {
            rownames(out) <- seq.int(nrow(out))
        }
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented yet")
    }
    class(out) <- c("GCOD_df", class(out))
    out
}

#' Print method for objects of class `GCOD_df`, as produced by [`get_athlete_ids()`].
#'
#' @param object an object of class `GCOD_df`.
#' @param txtplot logical indicating whether or not a text barplot should be printed of the percent of athlete ID records modified per year quarter. Default is `FALSE`.
#' @param ... currently not used.
 #'
#' @export
print.GCOD_df <- function(object, txtplot = FALSE, ...) {
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
        class(min_size) <- class(max_size) <- class(total_size) <- "object_size"
        date_range <- range(object$last_modified)
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format(min_size, units = "auto"), "|",
            "max =", format(max_size, units = "auto"), "|",
            "total =", format(total_size, units = "auto"), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        ## txtbarchart(as.factor(format(object$last_modified, format = "%Y-%m")), ylab = "Athletes %")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(object$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}


