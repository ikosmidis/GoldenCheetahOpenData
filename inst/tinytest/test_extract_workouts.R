## Download workouts from S3

out <- download_workouts("007", dir = tempdir(), overwrite = TRUE)

## Test that verbose does what it is supposed to
## Exists directly after the message is produced so no files are extracted
expect_message(extract_workouts(out, verbose = TRUE, clean_up = FALSE, overwrite = TRUE),
               pattern = "Extracting")

 expect_message(extract_workouts(out, verbose = TRUE, clean_up = FALSE, overwrite = FALSE),
                pattern = "Exists")

## Test that we fail if zip cannot be extracted
tmp <- file.path(tempdir(), "gibrish.zip")
cat("gibrish", file = tmp)
loc <- rebuild_gcod_db(tempdir())
## Hacking the gcod_db object
loc$local_db <- rbind(loc$local_db[1, ], loc$local_db)
loc$local_db$path[1] <- tmp
## If overwrite = TRUE then a failed message
expect_message(extract_workouts(loc, verbose = TRUE, clean_up = FALSE, overwrite = TRUE),
               pattern = "Failed")
## If overwrite = FALSE then just exists messages
expect_message(extract_workouts(loc, verbose = TRUE, clean_up = FALSE, overwrite = FALSE),
                pattern = "Exists")
file.remove(tmp)


out <- extract_workouts(out, verbose = FALSE, clean_up = FALSE, overwrite = FALSE)

## Extract the files
out1 <- extract_workouts(out)

## Test that the sub-directories exist
expect_true(any(grepl("007", list.dirs(tempdir(), recursive = FALSE))))

## Test that extracted is TRUE
expect_true(all(out1$local_db$extracted))

## Clean up
expect_true(all(file.remove(local_perspective(out)$path)))
unlink(gsub(".zip", "", out$local_db$path), recursive = TRUE)
