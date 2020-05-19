## Download workouts from S3

## Error if a vector of character strings
expect_error(download_workouts(c("000", "001"), dir = tempdir(), verbose = TRUE),
             pattern = "length\\(athlete_id\\) should be")

## Error if dir does not exists
set.seed(123)
randomstring <- paste(sample(c(letters, 0:9, "-", "+", "_"), 100, replace = TRUE), collapse = "")
expect_error(download_workouts("000", dir = randomstring),
             pattern = "is not a valid path")

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

## Check extraction
expect_message(extract_workouts.GCOD_files(out, verbose = TRUE),
               pattern = "Extracting")

## Check that supplying the first few letters of the ID works
out <- download_workouts("002", dir = tempdir(), extract = TRUE)
expect_true(out$extracted)
expect_true(file.exists(out$path))
expect_true(file.exists(gsub(".zip", "", out$path)))

## Check
