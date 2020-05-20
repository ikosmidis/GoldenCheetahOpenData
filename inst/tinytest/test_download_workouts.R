## Download workouts from S3

## Error if a vector of character strings
expect_error(download_workouts(c("000", "001"), dir = tempdir(), verbose = TRUE),
             pattern = "Vectors of character strings are not supported")

## Error if dir does not exists
set.seed(123)
randomstring <- paste(sample(c(letters, 0:9, "-", "+", "_"), 100, replace = TRUE), collapse = "")
expect_error(download_workouts("000", dir = randomstring),
             pattern = "does not exist")

## Download workouts using the first few letters of athlete IDs
expect_message(download_workouts("000d", dir = tempdir(), verbose = TRUE),
               pattern = "Downloading")

## Check no athlete ids to download
expect_error(download_workouts("abcdefg", dir = tempdir(), confirm = TRUE),
             pattern = "no athlete IDs to download")

## Check that supplying the first few letters of the ID works
out <- download_workouts("002", dir = tempdir())
expect_true(grepl("0027fd73-2e77", out$path))
expect_false(out$extracted)
expect_identical(out$mirror, "S3")
expect_true(inherits(out, "GCOD_files"))
expect_true(file.exists(out$path))

## Check that overwrite works
expect_error(out <- download_workouts("002", dir = tempdir(),
                                      extract = TRUE, overwrite = FALSE))

## Check that extraction works
out <- download_workouts("002", dir = tempdir(), extract = TRUE, overwrite = TRUE)
expect_true(out$extracted)
expect_true(file.exists(out$path))
expect_true(dir.exists(dirname(out$path)))

## Clean up
expect_true(all(file.remove(out$path)))
unlink(gsub(".zip", "", out$path), recursive = TRUE)




