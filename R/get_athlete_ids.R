#' List available athletes
#'
#' @param n_athletes integer indicating the maximum number of athlete IDs to return. Default is `Inf`, which will return all available athelte IDs.
#' @param mirror either `"S3"` or `"OSF"`, indicating the GoldeCheetah OpenData mirror to use. Default and recommended is "S3". See Details.
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
get_athlete_ids <- function(n_athletes = Inf,
                            mirror = "S3",
                            ...) {
    mirror <- match.arg(mirror, c("OSF", "S3"))
    if (isTRUE(mirror == "S3")) {
        gc_bucket <- 'goldencheetah-opendata'
        athlete_prefix <- "data/"
        out <- get_bucket_df(bucket = gc_bucket,
                             prefix = athlete_prefix,
                             max = n_athletes + 1,
                             ...)
        ## Get athlete id's
        athlete <- gsub(".zip|data/", "", out[, 1])
        base_dir_index <- match("", athlete, nomatch = 0)
        out$athlete_id <- athlete
        ## Assuming tz in amazon S3 is UTC
        out$LastModified <- as.POSIXct(out$LastModified, tz = "UTC",
                                               format = "%Y-%m-%dT%H:%M:%S.000Z")
        out <- out[-base_dir_index, ]
        names(out) <- c("path", "last_modified", "e_tag", "size", "owner_id", "owner_display_name", "storage_class", "bucket", "athlete_id")
        rownames(out) <- seq.int(nrow(out))
    }
    if (isTRUE(mirror == "OSF")) {
        stop("OSF is not implemented yet")
    }
    class(out) <- c("GCOD_df", class(out))
    out
}

