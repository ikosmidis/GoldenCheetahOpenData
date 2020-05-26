## AWS S3

## List the athlete files starting with "007"
w1 <- get_athlete_ids(prefix = "0075")
w2 <- get_athlete_ids(prefix = "dbced")

## Test db extractors
expect_identical(remote_perspective(w1), w1$remote)
expect_identical(local_perspective(w1), w1$local)

## Test classes
expect_identical(class(local_perspective(w1)), c("gcod_local_db", "data.frame"))
expect_identical(class(remote_perspective(w1)), c("gcod_remote_db", "data.frame"))
expect_identical(class(w1), c("gcod_db", "list"))


## Test c
re <- c(w1, w2, perspective = "remote")
lo <- c(w1, w2, perspective = "local")
bo <- c(w1, w2, perspective = "both")

expect_true(nrow(remote_perspective(re)) == 2)
expect_true(nrow(local_perspective(re)) == 0)

expect_true(nrow(remote_perspective(lo)) == 1) ## Only the remote from w1 is c'd
expect_true(nrow(local_perspective(lo)) == 0)

expect_true(nrow(remote_perspective(bo)) == 2) ## Both remotes
expect_true(nrow(local_perspective(bo)) == 0)

w10 <- w1
w10$remote_db$last_modified[1] <- w10$remote_db$last_modified[1] - 10000
w20 <- w2
w20$remote_db$last_modified[1] <- w20$remote_db$last_modified[1] - 10000
du <- c(w1, w1, w10, w2, w20, perspective = "both")

expect_true(nrow(remote_perspective(du)) == 2) ## Both remotes
expect_identical(du, c(w1, w2))

## Download them in temp dir
ids <- download_workouts(bo, local_dir = tempdir(), overwrite = FALSE)
## Test duplicates control
expect_identical(c(ids, ids, ids), ids)

## Sizes
expect_true(min_size(ids, perspective = "remote") == min(ids$remote_db$size))
expect_true(max_size(ids, perspective = "remote") == max(ids$remote_db$size))
expect_true(mean_size(ids, perspective = "remote") == mean(ids$remote_db$size))
expect_true(total_size(ids, perspective = "remote") == sum(ids$remote_db$size))
expect_true(min_size(ids, perspective = "local") == min(ids$local_db$size))
expect_true(max_size(ids, perspective = "local") == max(ids$local_db$size))
expect_true(mean_size(ids, perspective = "local") == mean(ids$local_db$size))
expect_true(total_size(ids, perspective = "local") == sum(ids$local_db$size))

## n_ids
expect_true(n_ids(ids, perspective = "remote") == nrow(ids$remote))
expect_true(n_ids(ids, perspective = "local") == nrow(ids$local))

## local_path
expect_identical(local_path(ids), ids$local$path)

## Rebuild
rids <- rebuild_db(tempdir())
rids$local_db <- rids$local_db[order(rids$local_db$athlete_id), ]
ids$local_db <- ids$local_db[order(ids$local_db$athlete_id), ]
ids$local_db$downloaded <- TRUE
rownames(ids$local_db) <- rownames(rids$local_db) <- NULL
expect_identical(remote_perspective(rids), remote_perspective(ids))
expect_identical(local_perspective(rids), local_perspective(ids))


system(paste("rm", file.path(tempdir(), "*.zip")))
