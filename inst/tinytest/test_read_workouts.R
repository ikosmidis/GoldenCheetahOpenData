ids <- get_athlete_ids(prefix = "00")
out <- download_workouts(ids, "002|000e", dir = tempdir(), overwrite = TRUE)
out <- extract_workouts(out)

res <- read_workouts(out, verbose = FALSE)



## Clean up
file.remove(out$path)
unlink(gsub(".zip", "", out$path), recursive = TRUE)
