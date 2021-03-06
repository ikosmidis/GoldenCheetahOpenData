#' Get information for athlete IDs that are available in the GoldenCheetah OpenData project and organize it in a `gcod_db` object.
#'
#' @param n_ids integer indicating the maximum number of athlete IDs
#'     to return. Default is `Inf`, which will return all available
#'     athlete IDs.
#' @param mirror either `"S3"` or `"OSF"`, indicating which
#'     GoldenCheetah OpenData mirror should be used. Default and
#'     recommended is "S3". See Details.
#' @param prefix character string that limits the response to athlete
#'     IDs that begin with it. Default is `NULL`, which does not limit
#'     responses.
#' @param ... further options to be passed to
#'     `[aws.s3::get_bucket_df()]`.
#' @details
#'
#' @return
#'
#' A [`list`] also inheriting from class `gcod_db` with components
#' `remote_db` (the "remote perspective"; of class `gcod_remote_db`)
#' and `local_db` ("the local perspective"; of class
#' `gcod_local_db`). `remote_db` and `local_db` are [`data.frame`]s,
#' also inheriting from classes `gcod_remote_db` and `gcod_local_db`,
#' respectively. `local_db` has variables "path", "last_modified",
#' "size", "extracted", "downloaded", "athlete_id"m and `remote_db`
#' has variables "key", "last_modified", "e_tag", "size", "owner_id",
#' "owner_display_name", "storage_class", "bucket", and "athlete_id".
#'
#' @details
#'
#' `mirror = OSF` currently returns an error and will be supported in future versions.
#'
#' @seealso [`print.gcod_db()`] [`min_size.gcod_db()`]
#'     [`max_size.gcod_db()`] [`total_size.gcod_db()`]
#'     [`mean_size.gcod_db()`] [`n_ids.gcod_db()`]
#'     [`remote_perspective.gcod_db()`]
#'     [`local_perspective.gcod_db()`]
#'
#' @aliases
#' `gcod_db`
#'
#' @references
#' Liversedge, M. (2020). GoldenCheetah OpenData Project. OSF. \url{https://doi.org/10.17605/OSF.IO/6HFPZ}
#'
#' @examples
#'
#' \donttest{
#' ## Get details for all available athlete IDs
#' ids <- get_athlete_ids()
#' ## Print basic info for the IDs retrieved
#' print(ids, txtplot = TRUE)
#'
#' ## Extract basic info for the IDs retrieved
#' n_ids(ids, perspective = "remote")
#' n_ids(ids, perspective = "local")
#' format(min_size(ids, perspective = "local"), unit = "auto")
#' format(min_size(ids, perspective = "remote"), unit = "auto")
#' format(max_size(ids), unit = "auto")
#' format(mean_size(ids), unit = "auto")
#' format(total_size(ids), unit = "auto")
#'
#' ## remote_perspective(ids) and local_perspective(ids) inherit from
#' ## data.frame` so we can easily do stuff like
#'
#' ## find which IDs have the minimum/maximum sizes
#' remote_ids <- remote_perspective(ids)
#' remote_ids[which.min(remote_ids$size), "athlete_id"]
#' ## find all IDs with the string "007"
#' subset(athlete_id(ids), grepl("007", athlete_id(ids)))
#' ## etc
#' }
#'
#' @export
get_athlete_ids <- function(n_ids = Inf,
                            mirror = "S3",
                            prefix = NULL,
                            ...) {
  mirror <- match.arg(mirror, c("OSF", "S3"))
  if (isTRUE(mirror == "S3")) {
    gc_bucket <- "goldencheetah-opendata"
    athlete_prefix <- paste0("data/", prefix)
    remote_db <- get_bucket(
      bucket = gc_bucket,
      prefix = athlete_prefix,
      max = n_ids + 1,
      ...
    )
    remote_db <- as.data.frame(remote_db, stringsAsFactors = FALSE)
    ## Get athlete id's
    athlete <- gsub(".zip|data/", "", remote_db[, 1])
    base_dir_index <- match("", athlete, nomatch = 0)
    remote_db$athlete_id <- athlete
    ## Assuming tz in amazon S3 is UTC
    remote_db$LastModified <- as.POSIXct(remote_db$LastModified,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%S.000Z"
    )
    if (base_dir_index) {
      remote_db <- remote_db[-base_dir_index, ]
    }
    names(remote_db) <- c("key", "last_modified", "e_tag", "size", "owner_id", "owner_display_name", "storage_class", "bucket", "athlete_id")
    remote_db$size <- as.numeric(remote_db$size)
    if (isTRUE(nrow(remote_db))) {
      rownames(remote_db) <- seq.int(nrow(remote_db))
    }
  }
  if (isTRUE(mirror == "OSF")) {
    stop("OSF is not implemented in the curretn version of GoldenCheetahOpenData")
  }
  ## Dummy data to get the classes right
  local_db <- data.frame(
    path = "a",
    last_modified = as.POSIXct("2000-01-01"),
    size = 1,
    extracted = FALSE,
    downloaded = FALSE,
    athlete_id = "a",
    stringsAsFactors = FALSE
  )
  ## Local is always empty when `get_athelte_ids` is called
  make_gcod_db(remote_db, local_db[-1, ], mirror = mirror)
}
