to_object_size <- function(x) {
    class(x) <- "object_size"
    x
}

format_object_size <- function(x, unit = "auto") {
    format(to_object_size(x), unit = unit)
}
