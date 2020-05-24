#' GoldenCheetahOpenData: R API to the GoldenCheetah OpenData Project
#'
#' @docType package
#' @name GoldenCheetahOpenData
#' @import aws.s3
#' @import osfr
#' @import trackeR
#' @importFrom jsonlite read_json
#' @importFrom readr read_csv
NULL

#' @export
extract_workouts <- function(object, ...) {
    UseMethod("extract_workouts")
}

#' @export
read_workouts <- function(object, ...) {
    UseMethod("read_workouts")
}


#' @export
n_ids <- function(object, ...) {
    UseMethod("n_ids")
}


#' @export
min_size <- function(object, ...) {
    UseMethod("min_size")
}


#' @export
max_size <- function(object, ...) {
    UseMethod("max_size")
}

#' @export
total_size <- function(object, ...) {
    UseMethod("total_size")
}

#' @export
mean_size <- function(object, ...) {
    UseMethod("mean_size")
}

#' @export
local_path <- function(object, ...) {
    UseMethod("local_path")
}


#' @export
remote <- function(object, ...) {
    UseMethod("remote")
}

#' @export
local <- function(object, ...) {
    UseMethod("local")
}

#' @export
athlete_id <- function(object, ...) {
    UseMethod("athlete_id")
}

#' @export
rebuild_gcod_db <- function(object, ...) {
    UseMethod("rebuild_gcod_db")
}

#' Clean up a directory from extracted subdirectories
#'
#' @param object either a character string giving the path to the directory to clean or an object of class `gcod_db`.
#' @param confirm logical determining whether the user should be asked whether they should continue with the deletion or not. Default is `TRUE`.
#' @param verbose
#'
#' @details
#' If `object` is an object of class `gcod_db`, then `clean_db`, looks for sub-directories with the same name as the zip files `local_path(object)`.
#'
#' @return
#'
#' If `object` is a character string, then nothing is returned. If `object` is a `gcod_db` object, then the object is returned, with all elements of `local(object)$extracted` being `FALSE`.
#'
#' @aliases clean_db.character clean_db.gcod_db
#' @export
clean_db <- function(object, confirm = TRUE, verbose = TRUE) {
    UseMethod("clean_db")
}
