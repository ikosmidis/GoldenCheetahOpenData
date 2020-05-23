#' Print methods for objects of class `gcod_db`, as produced by [`get_athlete_ids()`].
#'
#' @name print.gcod_db
#' @param object an object of class `gcod_db`, `gcod_db` or `gcod_db`
#' @param txtplot logical indicating whether or not text barplots should be printed for the remote and local databases showing the percentage of athlete ID records modified per year quarter. Default is `FALSE`.
#' @param ... currently not used.
#'
#' @aliases print.gcod_remote_db print.gcod_local_db
#' @export
print.gcod_db <- function(object, txtplot = FALSE, ...) {
    cat("Remote db\n")
    print.gcod_remote_db(remote(object), txtplot, ...)
    cat("\n")
    cat("Local db\n")
    print.gcod_local_db(local(object), txtplot, ...)
}

#' @rdname print.gcod_db
#' @export
print.gcod_remote_db <- function(object, txtplot = FALSE, ...) {
    object <- object
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
        date_range <- range(object$last_modified)
        cat("Mirror:", attr(object, "mirror"), "\n")
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(object$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' @rdname print.gcod_db
#' @export
print.gcod_local_db <- function(object, txtplot = FALSE, ...) {
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
        date_range <- range(object$last_modified)
        cat("Number of athlete IDs:", n_ids, "\n")
        cat("File sizes:",
            "min =", format_object_size(min_size), "|",
            "max =", format_object_size(max_size), "|",
            "total =", format_object_size(total_size), "\n")
        cat("Last modified: between", paste(format(date_range), collapse = " and "), "\n")
        if (isTRUE(txtplot)) {
            cat("Athlete ID records modified per year quarter:\n")
            tabs <- cut(object$last_modified, breaks = "quarter", ordered_result = TRUE)
            txtplot::txtbarchart(tabs, ylab = "Athletes %",
                                 width = round(options()$width), pch = "=")
        }
    }
}

#' Extract information from an object of class `gcod_db`, as produced by [`get_athlete_ids()`].
#' @name gcod_db_extractors
#' @param object an object of class `gcod_db`, as produced by [`get_athlete_ids()`].
#' @param db Either `"remote"` (default) or `"local"`, for the database to use for the extractor function.
#'
#' @details
#' File sizes are reported in bytes but inherit from class `object_size`. So, `format.object_size()` can be used for pretty units, etc.
#'
#' `local_path()` extracts the local paths from the `gcod_db` file. `remote(object)` and `local(object)` are equivalent to `object$remote_db` and `object$local_db`, respectively.
#'
#' @seealso [`get_athlete_ids()`]
#' @aliases min_size max_size mean_size total_size n_ids
NULL

#' @rdname gcod_db_extractors
#' @export
n_ids.gcod_db <- function(object, db = "remote") {
    switch(db,
           "remote" = nrow(remote(object)),
           "local" = nrow(local(object)),
           "`db` should be one of 'remote', 'local'")
}

#' @rdname gcod_db_extractors
#' @export
min_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, min(remote(object)$size), 0),
                  "local" = ifelse(n > 0, min(local(object)$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
max_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, max(remote(object)$size), 0),
                  "local" = ifelse(n > 0, max(local(object)$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
total_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, sum(remote(object)$size), 0),
                  "local" = ifelse(n > 0, sum(local(object)$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}


#' @rdname gcod_db_extractors
#' @export
total_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, sum(remote(object)$size), 0),
                  "local" = ifelse(n > 0, sum(local(object)$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}


#' @rdname gcod_db_extractors
#' @export
mean_size.gcod_db <- function(object, db = "remote") {
    n <- n_ids(object, db = db)
    out <- switch(db,
                  "remote" = ifelse(n > 0, mean(remote(object)$size), 0),
                  "local" = ifelse(n > 0, mean(local(object)$size), 0),
                  "`db` should be one of 'remote', 'local'")
    to_object_size(out)
}

#' @rdname gcod_db_extractors
#' @export
local_path.gcod_db <- function(object, ...) {
    object$local_db$path
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
athlete_id.gcod_db <- function(object, db = "remote") {
    switch(db,
           "remote" = remote(object)$athlete_id,
           "local" = local(object)$athlete_id,
           "`db` should be one of 'remote', 'local'")
}



#' Checks whether the files in the remote paths of a `gcod_db` exist in `local_dir'
#' @param object asd
#' @param local_dir the directory to check zip files for the selected athlete IDs.
#'
#' @details
#' Will update local_db accordingly
#' Ignores the contents of local_db, and rebuilds it according to the contents of local_dir
check_local_dir.gcod_db <- function(object, local_dir) {
    ## Find out what is in local_dir
    zip_paths <- dir(local_dir, pattern = ".zip", full.names = TRUE)
    local_ids <- athlete_id(object, db = "local")
    remote_ids <- athlete_id(object, db = "remote")
    match(local_ids, remote_ids, nomatch = 0)
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
    json_file <- paste0(ids, "/{", ids, "}.json")
    extracted <- file.exists(file.path(object, json_file))
    local_db <- data.frame(path = zip_paths,
                           last_modified = finfo$mtime,
                           size = finfo$size,
                           extracted = extracted,
                           downloaded = TRUE,
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
