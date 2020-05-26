#' Subset the local or remote perspective of a `gcod_db` object
#'
#' @param x an object of class `gcod_db`
#' @param subset logical expression indicating records to keep in either `remote_perspective(x)` or `local_perspective(x)` according to the value of `perspective`;  missing values are taken as `FALSE`.
#' @param perspective either `"remote"` (default) or `"local"`, for the perspective to subset.
#' @param ... currently not used.
#'
#' @details
#' See [`get_athlete_ids()`] for the variables each perspective holds.
#'
#' @return
#' An object of class `gcod_db` with the selected athlete IDs according to `subset` in its remote or local perspective.
#'
#' @export
#' @examples
#' \donttest{
#' ## Get all available athelte IDs
#' db <- get_athlete_ids()
#' ## Return athletes from the remote perspective of db with "b7-9" in their IDs
#' db79 <- subset(db, subset = grepl("b7-9", athlete_id(db)), perspective = "remote")
#' athlete_id(db79, perspective = "remote")
#' }
subset.gcod_db <- function(x, subset, perspective = "remote", ...) {

    switch(perspective,
        "remote" = within(x, {
            remote_db <- subset(remote_perspective(x), subset = subset)
        }),
        "local" = within(x, {
            local_db <- subset(local_perspective(x), subset = subset)
        }),
        stop("`perspective` should be one of 'remote', 'local'"))
}

#' Checks whether the workout archives referenced in the remote
#' perspective of a `gcod_db` exist in `local_dir'
#'
#' @param object an object of class `gcod_db`.
#' @param local_dir the directory to check for the workout archives
#'     for `athlete_id(object, perspective = "remote")`.
#' @param ... currently not used.
#'
#' @return
#' A vector of the same length as `n_ids(object, perspective = "remote")`.
#'
#' @examples
#' \donttest{
#' ids007 <- get_athlete_ids(prefix = "007")
#' ## Download the workouts for the first athlete in tempdir()
#' id <- subset(ids007, athlete_id(ids007) == athlete_id(ids007)[2])
#' id <- download_workouts(id)
#' ## Only the workout archive for athlete_id(ids007)[2] from the ones
#' #in the remote perspective of ids007 exist in tempdir()
#' exist_in(ids007, tempdir())
#' }
#' @export
exist_in.gcod_db <- function(object, local_dir, ...) {
    ## Find out what is in local_dir
    remote_files <- gsub("data/", "", remote_perspective(object)$key)
    local_files <- file.exists(paste0(local_dir, "/", remote_files))
    names(local_files) <- remote_files
    local_files
}

#' Attempts to rebuild a `gcod_db` from the contents of a local directory
#'
#' @param object a character string giving the path to the directory to use for extracting athlete IDs.
#' @param mirror either `"S3"` or `"OSF"`, indicating which GoldenCheetah OpenData mirror should be used. Default and recommended is "S3". See Details.
#' @param ... currently not used.
#'
#' @details
#' `mirror = OSF` currently returns an error and will be supported in future versions.
#'
#' @return
#' An object of class `gcod_db`.
#'
#' @examples
#' \donttest{
#' ids007 <- get_athlete_ids(prefix = "007")
#' ## Download the workouts in tempdir()
#' ids007 <- download_workouts(ids007)
#' ## Test that the rebuild `gcod_db` object is identical to ids007
#' identical(ids007, rebuild_gcod_db(tempdir()))
#' }
#' @export
rebuild_gcod_db.character <- function(object, mirror = "S3", ...) {
    if (!dir.exists(object)) {
        stop(paste(object, "is either not a directory or does not exist."))
    }
    zip_paths <- dir(object, pattern = ".zip", full.names = TRUE)
    if (length(zip_paths) == 0) {
        stop(paste("No zip files found in", object))
    }
    ## Here we keep the extension to avoid partial matching of non-id
    ## filenames with prefix
    prefix <- basename(zip_paths)
    ids <- gsub(".zip", "", prefix)
    ## remote_db
    ## Assuming that we cannot get two files with the same name on the
    ## same dir, so no duplicates can result in the remotes from c
    gcod_dbs <- lapply(prefix, function(x) get_athlete_ids(prefix = x, mirror = mirror))
    inds <- sapply(gcod_dbs, n_ids) > 0
    remote_db <- remote_perspective(do.call(function(...) {
        c.gcod_db(..., perspective = "remote")
    }, gcod_dbs))
    ## local_db
    ## Keep only files for which get_athlete_ids returns something
    zip_paths <- zip_paths[inds]
    ids <- ids[inds]
    finfo <- file.info(zip_paths)
    ## Check whether the athelte_ids have been extracted
    json_file <- if (length(ids)) paste0(ids, "/{", ids, "}.json") else character(0)
    extracted <- file.exists(file.path(object, json_file))
    downloaded <- rep(TRUE, length(ids))
    local_db <- data.frame(path = zip_paths,
                           last_modified = finfo$mtime,
                           size = finfo$size,
                           extracted = extracted,
                           downloaded = downloaded,
                           athlete_id = ids,
                           stringsAsFactors = FALSE)
    make_gcod_db(remote_db, local_db, mirror = mirror)
}

#' Concatenate `gcod_db` objects
#'
#' @param ... objects to be concatenated.
#' @param perspective either `"remote"` (default) or `"local"` or `"both"`, for the perspective to use for the extractor function.
#' @details
#'
#' If `perspective = "remote"`, then the remote perspectives of `...`
#' are concatenated and the local perspective the first object in
#' `...` is used. If `perspective = "local"`, then the local
#' perspectives of `...` are concatenated and the remote perspective
#' from first object in `...` is used. If `perspective = "both"` then
#' both the local and remote perspectives are concatenated to form the
#' local and the remote perspective of the returned object.
#'
#' Records with duplicated athlete IDs in the objects being
#' concatenated are replaced, in the concatenated object, by a single
#' record with the most recently modified entry.
#'
#' The mirror is inherited from the first object supplied for
#' concatenation.
#'
#' @return
#' An object of class `gcod_db`
#'
#' @export
c.gcod_db <- function(..., perspective = "remote") {
    perspective <- match.arg(perspective, c("remote", "local", "both"))
    input <- list(...)
    input <- input[!unlist(lapply(input, is.null))]
    ninput <- length(input)
    mirror <- attr(remote_perspective(input[[1]]), "mirror")
    rm_duplicates <- function(db) {
        ## Keeps the most recent file for each athlete id
        db <- db[order(db$last_modified, decreasing = TRUE), ]
        db <- db[!duplicated(db$athlete_id), ]
        db
    }
    if (isTRUE(perspective == "remote")) {
        local_db <- input[[1]]$local_db
        remote_db <- do.call("rbind", lapply(input, remote_perspective))
        remote_db <- rm_duplicates(remote_db)
    }
    if (isTRUE(perspective == "local")) {
        local_db <- do.call("rbind", lapply(input, local_perspective))
        local_db <- rm_duplicates(local_db)
        remote_db <- input[[1]]$remote_db
    }
    if (isTRUE(perspective == "both")) {
        local_db <- do.call("rbind", lapply(input, local_perspective))
        local_db <- rm_duplicates(local_db)
        remote_db <- do.call("rbind", lapply(input, remote_perspective))
        remote_db <- rm_duplicates(remote_db)
    }
    make_gcod_db(remote_db, local_db, mirror)
}

#' @rdname clean_db
#' @export
clean_db.character <- function(object, confirm = TRUE, verbose = TRUE) {
    if (!dir.exists(object)) {
        stop(paste(object, "is either not a directory or does not exist."))
    }
    sub_dirs <- list.dirs(object, full.names = TRUE, recursive = FALSE)
    ids <- basename(sub_dirs)
    do_clean_db(sub_dirs, ids, confirm, verbose)
}

#' @rdname clean_db
#' @export
clean_db.gcod_db <- function(object, confirm = TRUE, verbose = TRUE) {
    sub_dirs <- local_path(object)
    sub_dirs <- gsub(".zip", "", sub_dirs)
    ids <- basename(sub_dirs)
    do_clean_db(sub_dirs, ids, confirm, verbose)
    object$local_db$extracted <- FALSE
    object
}
