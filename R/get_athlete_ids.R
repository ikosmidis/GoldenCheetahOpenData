#' List available athletes
#'
#' @param n_athletes integer indicating the maximum number of athlete IDs to return. Default is `Inf`, which will return all available athelte IDs.
#' @param mirror either `"S3"` or `"OSF"`, indicating the GoldeCheetah OpenData mirror to use. Default and recommended is "S3". See Details.
#' @param prefix character string that limits the response to athlete IDs that begin with it. Default is `NULL`, which does not limit responses.
#' @param ... further options to be passed to `[aws.s3::get_bucket_df()]`.
#' @details
#'
#' The `data.frame` that is returned if `details = TRUE` has
#' "Key", "LastModified", "ETag", "Size", "Size", "Owner_ID",
#' "Owner_DisplayName", "StorageClass", "Bucket", "athlete_id"
#'
#' IK, 20200517: ADD DETAILS ON MIRROR
#'
#' IK, 20200517: ADD EXAMPLES
#' @export
get_athlete_ids <- function(n_athletes = Inf,
                            mirror = "S3",
                            prefix = NULL,
                            ...) {
    mirror <- match.arg(mirror, c("OSF", "S3"))
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        athlete_prefix <- paste0("data/", prefix)
        out <- get_bucket(bucket = gc_bucket,
                          prefix = athlete_prefix,
                          max = n_athletes + 1,
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

#' Print method for objects of class `GCOD_df`
#'
#' @param object an object of class `GCOD_df`.
#' @param txtplot logical indicating whether or not a text barplot should be printed of the percent of athlete ID records modified per year quarter. Default is `FALSE`.
#' @param ... currently not used.
 #'
#' @export
print.GCOD_df <- function(object, txtplot = FALSE, ...) {
    n_athletes <- nrow(object)
    if (isTRUE(n_athletes == 0)) {
        cat("Number of athlete IDs:", 0, "\n")
    }
    else {
        sizes <- object$size
        total_size <- sum(sizes)
        min_size <- min(sizes)
        max_size <- max(sizes)
        class(min_size) <- class(max_size) <- class(total_size) <- "object_size"
        date_range <- range(object$last_modified)
        cat("Number of athlete IDs:", n_athletes, "\n")
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
