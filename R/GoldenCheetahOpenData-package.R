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
process_workouts <- function(object, ...) {
    UseMethod("process_workouts")
}


#' @export
n_ids <- function(object) {
    UseMethod("n_ids")
}


#' @export
min_size <- function(object) {
    UseMethod("min_size")
}


#' @export
max_size <- function(object) {
    UseMethod("max_size")
}

#' @export
total_size <- function(object) {
    UseMethod("total_size")
}

#' @export
mean_size <- function(object) {
    UseMethod("mean_size")
}
