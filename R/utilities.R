do_clean_db <- function(sub_dirs, ids, confirm, verbose) {
  if (length(ids) == 0) {
    stop(paste("No workout directories found"))
  }
  json_file <- if (length(ids)) file.path(sub_dirs, paste0("/{", ids, "}.json")) else character(0)
  extracted <- file.exists(json_file)
  if (all(!extracted)) {
    stop(paste("No workout directories found"))
  }
  if (isTRUE(confirm)) {
    out <- utils::askYesNo(paste0("Attempting to delete\n", paste(sub_dirs, collapse = "\n"), "\nProcced?"))
    if (!isTRUE(out)) {
      return(NULL)
    }
  }
  if (verbose) {
    message("Deleting extracted directories...", appendLF = FALSE)
  }
  unlink(sub_dirs[extracted], recursive = TRUE)
  if (verbose) {
    message("Done.", appendLF = TRUE)
  }
}


make_gcod_db <- function(remote_db, local_db, mirror) {
  if (!inherits(local_db, "gcod_local_db")) {
    class(local_db) <- c("gcod_local_db", class(local_db))
  }
  if (!inherits(remote_db, "gcod_remote_db")) {
    class(remote_db) <- c("gcod_remote_db", class(remote_db))
  }
  rownames(remote_db) <- rownames(local_db) <- NULL
  out <- list(remote_db = remote_db, local_db = local_db)
  class(out) <- c("gcod_db", class(out))
  attr(out$remote_db, "mirror") <- mirror
  out
}

to_object_size <- function(x) {
  class(x) <- "object_size"
  x
}

format_object_size <- function(x, unit = "auto") {
  format(to_object_size(x), unit = unit)
}
