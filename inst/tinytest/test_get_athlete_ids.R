## AWS S3

## List the 0 athlete files
s3_ids0 <- get_athlete_ids(n_ids = 0, mirror = "S3")

## List the 11 first athlete files from S3 in alphabetical order
s3_ids <- get_athlete_ids(n_ids = 11, mirror = "S3")

## Get data for specific athletes
s3_ids1 <- get_athlete_ids(n_ids = 11, mirror = "S3", prefix = "000")

## Check the dimensions
expect_identical(dim(s3_ids0$remote_db), as.integer(c(0, 9)))
expect_identical(dim(s3_ids0$local_db), as.integer(c(0, 6)))
expect_identical(dim(s3_ids$remote_db), as.integer(c(11, 9)))
expect_identical(dim(s3_ids$local_db), as.integer(c(0, 6)))

## Check that we get the right names
remote_names <- c("key", "last_modified", "e_tag", "size", "owner_id", "owner_display_name", "storage_class", "bucket", "athlete_id")
local_names <- c("path", "last_modified", "size", "extracted", "downloaded", "athlete_id")
expect_identical(remote_names, names(s3_ids$remote_db))
expect_identical(local_names, names(s3_ids$local_db))

## Check that s3_ids inherits from the right classes
expect_true(inherits(s3_ids$remote_db, "data.frame"))
expect_true(inherits(s3_ids$local_db, "data.frame"))
expect_true(inherits(s3_ids$remote_db, "gcod_remote_db"))
expect_true(inherits(s3_ids$local_db, "gcod_local_db"))
expect_true(inherits(s3_ids, "gcod_db"))

## Check that dates are POSIXct
expect_true(inherits(s3_ids$remote_db$last_modified, "POSIXct"))
expect_true(inherits(s3_ids$local_db$last_modified, "POSIXct"))

## Check that there are no factors in the data frame
expect_false(any(sapply(s3_ids$remote_db, is.factor)))
expect_false(any(sapply(s3_ids$local_db, is.factor)))

## Expect match error if mirror != "OSF" or "S3"
expect_error(get_athlete_ids(mirror = "queens_magic_mirror"), pattern = "'arg' should be one of")

## expect pattern "data/" and ".zip" in key but not in athlete_id
expect_true(all(grepl("data/", s3_ids$remote_db$key)))
expect_true(all(grepl(".zip", s3_ids$remote_db$key)))
expect_false(any(grepl("data/", s3_ids$remote_db$athlete_id)))

## Check mirror
expect_identical(attr(s3_ids$remote_db, "mirror"), "S3")

## Expect athlete_ids starting with "000"
expect_true(all(substr(s3_ids1$remote_db$athlete_id, 1, 3) == "000"))

## OSF

## Expect error if mirror = "OSF" (to be fixed)
expect_error(get_athlete_ids(mirror = "OSF"), pattern = "is not implemented")

