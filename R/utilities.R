make_gcod_db <- function(remote_db, local_db) {
    if (!inherits(local_db, "gcod_local_db")) {
        class(local_db) <- c("gcod_local_db", class(local_db))
    }
    if (!inherits(remote_db, "gcod_remote_db")) {
        class(remote_db) <- c("gcod_remote_db", class(remote_db))
    }
    rownames(remote_db) <- rownames(local_db) <- NULL
    out <- list(remote_db = remote_db, local_db = local_db)
    class(out) <- c("gcod_db", class(out))
    out
}

to_object_size <- function(x) {
    class(x) <- "object_size"
    x
}

format_object_size <- function(x, unit = "auto") {
    format(to_object_size(x), unit = unit)
}

