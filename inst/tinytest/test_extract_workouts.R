## Download workouts from S3

out <- download_workouts("007", dir = tempdir(), overwrite = TRUE)

## Test that verbose does what it is supposed to
## Exists directly after the message is produced so no files are extracted
expect_message(extract_workouts(out, verbose = TRUE, clean_up = FALSE, overwrite = TRUE),
               "Extracting")

expect_message(extract_workouts(out, verbose = TRUE, clean_up = FALSE, overwrite = FALSE),
               "*Exists")

## Extract the files
out1 <- extract_workouts(out)

## Test that there are complaints if the directories exist and overwrite = FALSE
expect_error(extract_workouts(out, verbose = FALSE, clean_up = FALSE, overwrite = FALSE),
             "overwrite = TRUE")

## Test that the sub-directories exist
expect_true(all(grepl("007", list.dirs(tempdir(), recursive = FALSE))))

## Test that extracted is TRUE
expect_true(all(out1$local_db$extracted))

## Clean up
expect_true(all(file.remove(local(out)$path)))
unlink(gsub(".zip", "", out$local_db$path), recursive = TRUE)
