## AWS S3

## List the athlete files starting with "007"
w1 <- get_athlete_ids(prefix = "0075")
w2 <- get_athlete_ids(prefix = "dbced")

## Test db extractors
expect_identical(remote(w1), w1$remote)
expect_identical(local(w1), w1$local)

## Test classes
expect_identical(class(local(w1)), c("gcod_local_db", "data.frame"))
expect_identical(class(remote(w1)), c("gcod_remote_db", "data.frame"))
expect_identical(class(w1), c("gcod_db", "list"))


## Test c
re <- c(w1, w2, perspective = "remote")
lo <- c(w1, w2, perspective = "local")
bo <- c(w1, w2, perspective = "both")

expect_true(nrow(remote(re)) == 2)
expect_true(nrow(local(re)) == 0)

expect_true(nrow(remote(lo)) == 1) ## Only the remote from w1 is c'd
expect_true(nrow(local(lo)) == 0)

expect_true(nrow(remote(bo)) == 2) ## Both remotes
expect_true(nrow(local(bo)) == 0)

w10 <- w1
w10$remote_db$last_modified[1] <- w10$remote_db$last_modified[1] - 10000
w20 <- w2
w20$remote_db$last_modified[1] <- w20$remote_db$last_modified[1] - 10000
du <- c(w1, w1, w10, w2, w20, perspective = "both")

expect_true(nrow(remote(du)) == 2) ## Both remotes
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
rids <- rebuild_gcod_db(tempdir())
rids$local_db <- rids$local_db[order(rids$local_db$athlete_id), ]
ids$local_db <- ids$local_db[order(ids$local_db$athlete_id), ]
ids$local_db$downloaded <- TRUE
rownames(ids$local_db) <- rownames(rids$local_db) <- NULL
expect_identical(remote(rids), remote(ids))
expect_identical(local(rids), local(ids))


system(paste("rm", file.path(tempdir(), "*.zip")))
