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
download_workouts <- function(object, ...) {
    UseMethod("download_workouts")
}
