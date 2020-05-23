## Download workouts from S3

## Error if a vector of character strings
expect_error(download_workouts(c("000", "001"), local_dir = tempdir(), verbose = TRUE),
             pattern = "Vectors of character strings are not supported")

## Error if dir does not exists
set.seed(123)
randomstring <- paste(sample(c(letters, 0:9, "-", "+", "_"), 100, replace = TRUE), collapse = "")
expect_error(download_workouts("000", local_dir = randomstring),
             pattern = "does not exist")

## Download workouts using the first few letters of athlete IDs
expect_message(download_workouts("000d", local_dir = tempdir(), verbose = TRUE),
               pattern = "Downloading")

## Check no athlete ids to download
expect_error(download_workouts("abcdefg", local_dir = tempdir(), confirm = TRUE),
             pattern = "no athlete IDs to download")

## Check that supplying the first few letters of the ID works
out <- download_workouts("002", local_dir = tempdir())
expect_true(grepl("0027fd73-2e77", out$local_db$path))
expect_false(out$local_db$extracted)
expect_identical(attr(out$remote_db, "mirror"), "S3")
expect_true(inherits(out, "gcod_db"))
expect_true(all(file.exists(out$local_db$path)))


## Check that overwrite works
expect_message(out <- download_workouts("0027fd", local_dir = tempdir(),
                                        verbose = TRUE,
                                        extract = TRUE, overwrite = FALSE),
               pattern = "Skipping")

## Check that extraction works
out <- download_workouts("002", local_dir = tempdir(), extract = TRUE, overwrite = TRUE)
expect_true(out$local_db$extracted)
expect_true(file.exists(out$local_db$path))
expect_true(dir.exists(dirname(out$local_db$path)))

## Clean up
system(paste("rm", file.path(tempdir(), "*.zip")))
unlink(gsub(".zip", "", out$local_db$path), recursive = TRUE)




