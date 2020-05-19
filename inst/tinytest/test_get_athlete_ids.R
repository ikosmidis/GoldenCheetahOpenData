## Get athlete ids from AWS S3

## List the 0 athlete files
s3_ids0 <- get_athlete_ids(details = TRUE, mirror = "S3", n_ids = 0)

## List the 11 first athlete files from S3 in alphabetical order
s3_ids <- get_athlete_ids(details = TRUE, mirror = "S3", n_ids = 11)

## Get data for a specific athlete
s3_ids1 <- get_athlete_ids(details = TRUE, mirror = "S3", prefix = "000", n_ids = 11)

## Check the dimensions
expect_identical(dim(s3_ids0), as.integer(c(0, 9)))
expect_identical(dim(s3_ids), as.integer(c(11, 9)))

## Check that we get the right names
expected_names <- c("key", "last_modified", "e_tag", "size", "owner_id", "owner_display_name", "storage_class", "bucket", "athlete_id")
expect_identical(expected_names, names(s3_ids))

## Check that s3_ids inherits from a data frame and GCOD_df
expect_true(inherits(s3_ids, "data.frame"))
expect_true(inherits(s3_ids, "GCOD_df"))

## Check that dates are POSIXct
expect_true(inherits(s3_ids$last_modified, "POSIXct"))

## Check that there are no factors in the data frame
expect_false(any(sapply(s3_ids, is.factor)))

## Expect match error if mirror != "OSF" or "S3"
expect_error(get_athlete_ids(mirror = "queens_magic_mirror"), pattern = "'arg' should be one of")

## expect pattern "data/" and ".zip" in key but not in athlete_id
expect_true(all(grepl("data/", s3_ids$key)))
expect_true(all(grepl(".zip", s3_ids$key)))
expect_false(any(grepl("data/", s3_ids$athlete_id)))


## Get athlete ids from OSF

## Expect error if mirror = "OSF" (to be fixed)
expect_error(get_athlete_ids(mirror = "OSF"), pattern = "is not implemented")

