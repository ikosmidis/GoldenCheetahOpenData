
#' Checks whether the files in the remote paths of a `gcod_db` exist in `local_dir'
#' @param object asd
#' @param local_dir the directory to check zip files for the selected athlete IDs.
#'
exist_in.gcod_db <- function(object, local_dir) {
    ## Find out what is in local_dir
    local_ids <- athlete_id(object, db = "local")
    remote_ids <- athlete_id(object, db = "remote")
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
    remote_db <- remote(do.call(function(...) c.gcod_db(..., db = "remote"),
                                gcod_dbs))
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
    make_gcod_db(remote_db, local_db)
}

#' Concatanate gcod_db objects
#'
#' @details
#'
#' If `db = "remote"`, then `...$remote` are concatanated and
#' `...$local_db` from the first object is used. If `db = "local"`,
#' then `...$local` are concatanated and `...$remote_db` from the
#' first object is used. If `db = "both"` then both  from `...$remote` and
#' `...$local_db` are concatanated.
#'
#' Records with duplicated athlete IDs in the objects being
#' concatenated are replaced, in the concatanated object, by a single
#' record with the most recently modified entry.
#' @export
c.gcod_db <- function(..., db = "remote") {
    db <- match.arg(db, c("remote", "local", "both"))
    input <- list(...)
    input <- input[!unlist(lapply(input, is.null))]
    ninput <- length(input)
    rm_duplicates <- function(db) {
        ## Keeps the most recent file for each athlete id
        db <- db[order(db$last_modified, decreasing = TRUE), ]
        db <- db[!duplicated(db$athlete_id), ]
        db
    }
    if (isTRUE(db == "remote")) {
        local_db <- input[[1]]$local_db
        remote_db <- do.call("rbind", lapply(input, remote))
        remote_db <- rm_duplicates(remote_db)
    }
    if (isTRUE(db == "local")) {
        local_db <- do.call("rbind", lapply(input, local))
        local_db <- rm_duplicates(local_db)
        remote_db <- input[[1]]$remote_db
    }
    if (isTRUE(db == "both")) {
        local_db <- do.call("rbind", lapply(input, local))
        local_db <- rm_duplicates(local_db)
        remote_db <- do.call("rbind", lapply(input, remote))
        remote_db <- rm_duplicates(remote_db)
    }
    make_gcod_db(remote_db, local_db)
}
