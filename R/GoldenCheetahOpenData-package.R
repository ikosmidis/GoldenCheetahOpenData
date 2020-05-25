#' GoldenCheetahOpenData: R API to the GoldenCheetah OpenData Project
#' and Management of Local Workout Databases
#'
#' GoldenCheetahOpenData provides an R API to the GoldenCheetah
#' OpenData Project <doi: 10.17605/OSF.IO/6HFPZ>. Methods are provided
#' to download workout data from the AWS S3 mirror of the
#' GoldenCheetah OpenData Project and to wrangle the workout data and
#' organize them into `trackeRdata` objects for further processing
#' using the infrastructure of the `trackeR` R package.
#'
#' @references
#'
#' Liversedge, M. (2020). GoldenCheetah OpenData Project. OSF. \url{https://doi.org/10.17605/OSF.IO/6HFPZ}
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in R. *Journal
#' of Statistical Software*, **82**(7),
#' 1--29. [doi:10.18637/jss.v082.i07](https://doi.org/10.18637/jss.v082.i07)
#'
#' @docType package
#' @name GoldenCheetahOpenData
#' @import trackeR
#' @import aws.s3
#' @importFrom jsonlite read_json
#' @importFrom txtplot txtbarchart
#' @importFrom readr read_csv
NULL

#' @rdname extract_workouts.gcod_db
#' @export
extract_workouts <- function(object, ...) {
    UseMethod("extract_workouts")
}

#' @rdname gcod_db_extractors
#' @export
read_workouts <- function(object, ...) {
    UseMethod("read_workouts")
}

#' @rdname gcod_db_extractors
#' @export
n_ids <- function(object, ...) {
    UseMethod("n_ids")
}

#' @rdname gcod_db_extractors
#' @export
min_size <- function(object, ...) {
    UseMethod("min_size")
}

#' @rdname gcod_db_extractors
#' @export
max_size <- function(object, ...) {
    UseMethod("max_size")
}

#' @rdname gcod_db_extractors
#' @export
total_size <- function(object, ...) {
    UseMethod("total_size")
}

#' @rdname gcod_db_extractors
#' @export
mean_size <- function(object, ...) {
    UseMethod("mean_size")
}

#' @rdname gcod_db_extractors
#' @export
local_path <- function(object, ...) {
    UseMethod("local_path")
}

#' @rdname gcod_db_extractors
#' @export
remote <- function(object, ...) {
    UseMethod("remote")
}

#' @rdname gcod_db_extractors
#' @export
local <- function(object, ...) {
    UseMethod("local")
}

#' @rdname gcod_db_extractors
#' @export
athlete_id <- function(object, ...) {
    UseMethod("athlete_id")
}

#' @rdname gcod_db_extractors
#' @export
rebuild_gcod_db <- function(object, ...) {
    UseMethod("rebuild_gcod_db")
}

#' Clean up a directory from extracted workout sub-directories.
#'
#' @param object either a character string giving the path to the directory to look for workout sub-directories or an object of class `gcod_db`.
#' @param confirm logical determining whether the user should be prompted to confirm whether they should continue with the deletion. Default is `TRUE`.
#' @param verbose logical determining whether progress information should be printed. Default is `TRUE`.
#'
#' @details
#' If `object` is an object of class `gcod_db`, then `clean_db`, looks for sub-directories with the same name as the zip files `local_path(object)`.
#'
#' @return
#'
#' If `object` is a character string, then nothing is returned. If `object` is a `gcod_db` object, then the object is returned, with all elements of `local(object)$extracted` set to `FALSE`.
#'
#' @aliases clean_db.character clean_db.gcod_db
#' @export
clean_db <- function(object, confirm = TRUE, verbose = TRUE) {
    UseMethod("clean_db")
}

#' @rdname exist_in.gcod_db
#' @export
exist_in <- function(object, ...) {
    UseMethod("exist_in")
}
