#' Subset the local or remote perspective of a `gcod_db` object
#'
#' @param x an object of class `gcod_db`
#' @param subset logical expression indicating records to keep in either `remote(x)` or `local(x)` according to the value of `perspective`;  missing values are taken as `FALSE`.
#' @param perspective either `"remote"` (default) or `"local"`, for the perspective to subset.
#' @param ... currently not used.
#'
#' @details
#' See [`get_athlete_ids()`] for the variables each perspective holds.
#'
#' @export
#' @examples
#' \donttest{
#' db <- get_athlete_ids()
#' ## Return athletes from the remote perspective of db with "b7-9" in
#' ## their IDs
#' db79 <- subset(db, subset = grepl("b7-9", remote(db)$athlete_id), perspective = "remote")
#' athlete_id(db79, perspective = "remote")
#' }
subset.gcod_db <- function(x, subset, perspective = "remote") {

    switch(perspective,
        "remote" = within(x, {
            remote_db <- subset(remote(x), subset = subset)
        }),
        "local" = within(x, {
            local_db <- subset(local(x), subset = subset)
        }),
        stop("`perspective` should be one of 'remote', 'local'"))
}

#' Checks whether the files in the remote paths of a `gcod_db` exist in `local_dir'
#' @param object asd
#' @param local_dir the directory to check zip files for the selected athlete IDs.
#'
#' @export
exist_in.gcod_db <- function(object, local_dir) {
    ## Find out what is in local_dir
    local_ids <- athlete_id(object, perspective = "local")
    remote_ids <- athlete_id(object, perspective = "remote")
    all(remote_ids %in% local_ids)
}

#' Attempts to rebuild a `gcod_db` from the contents of a local directory
#'
#' @param local_dir a character string giving the path to the directory to use for extracting athlete IDs.
#' @export
rebuild_gcod_db.character <- function(object, mirror = "S3") {
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
    remote_db <- remote(do.call(function(...) {
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

#' Concatanate gcod_db objects
#'
#' @details
#'
#' If `perspective = "remote"`, then `...$remote` are concatanated and
#' `...$local_db` from the first object is used. If `perspective = "local"`,
#' then `...$local` are concatanated and `...$remote_db` from the
#' first object is used. If `perspective = "both"` then both  from `...$remote` and
#' `...$local_db` are concatanated.
#'
#' Records with duplicated athlete IDs in the objects being
#' concatenated are replaced, in the concatanated object, by a single
#' record with the most recently modified entry.
#'
#' The mirror is inheritted from the first object supplied for
#' concatenation.
#'
#' @export
c.gcod_db <- function(..., perspective = "remote") {
    perspective <- match.arg(perspective, c("remote", "local", "both"))
    input <- list(...)
    input <- input[!unlist(lapply(input, is.null))]
    ninput <- length(input)
    mirror <- attr(remote(input[[1]]), "mirror")
    rm_duplicates <- function(db) {
        ## Keeps the most recent file for each athlete id
        db <- db[order(db$last_modified, decreasing = TRUE), ]
        db <- db[!duplicated(db$athlete_id), ]
        db
    }
    if (isTRUE(perspective == "remote")) {
        local_db <- input[[1]]$local_db
        remote_db <- do.call("rbind", lapply(input, remote))
        remote_db <- rm_duplicates(remote_db)
    }
    if (isTRUE(perspective == "local")) {
        local_db <- do.call("rbind", lapply(input, local))
        local_db <- rm_duplicates(local_db)
        remote_db <- input[[1]]$remote_db
    }
    if (isTRUE(perspective == "both")) {
        local_db <- do.call("rbind", lapply(input, local))
        local_db <- rm_duplicates(local_db)
        remote_db <- do.call("rbind", lapply(input, remote))
        remote_db <- rm_duplicates(remote_db)
    }
    make_gcod_db(remote_db, local_db, mirror)
}
