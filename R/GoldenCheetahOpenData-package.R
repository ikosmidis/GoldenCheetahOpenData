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
