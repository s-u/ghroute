GHRoutes <- function(ok, router)
    structure(list(
        success = ok,
        paths = .jcall(router, "[Lcom/graphhopper/ResponsePath;", "getPaths"),
        responses = .jcall(router, "[Lcom/graphhopper/GHResponse;", "getResponses"),
        points = matrix(.jcall(router, "[D", "getAllPoints"),, 3),
        router = router),
        class="GHRoutes")

print.GHRoutes <- function(x, ..., max=5) {
    n <- length(x$success)
    cat("GHRoutes: ", n, " routing requests, ", sum(x$success), " successful ", length(x$success) - sum(x$success), " failed.\n", sep='')
    has.more <- FALSE
    if (n > max) {
        x <- max
        has.more <- TRUE
    }
    if (n > 0)
        for (i in 1:n)
            print(x[[i]], short=TRUE)
    if (has.more)
        cat("...\n")
    invisible(x)
}

length.GHRoutes <- function(x) length(x$success)

is.na.GHRoutes <- function(x) is.na(x$success) | (x$success == FALSE)

`[[.GHRoutes` <- function(x, i) {
    ## this is not quite the usual behavior, but it is like using [[-1]] which does lead to an error
    if (i < 1L || i > length(x$success)) stop("Attempt to access non-existent route at index ", i)
    if (length(x$paths)) {
        path <- x$paths[[i]]
        GHPath(path)
    } else if (length(x$responses)) {
        resp <- x$responses[[i]]
        lapply(.jcall(gh$tools, "[Lcom/graphhopper/ResponsePath;", "getAllPaths", resp), GHPath)
    } else NULL
}

`[.GHRoutes` <- function(x, i, j, ...) {
    ix <- seq_along(x$success)[i]
    if (any(is.na(ix))) stop("Indexing routes out of bounds is not allowed")
    cs <- function(o) if (length(o)) o[ix] else o
    ## re-index 
    reix <- function(m) { m[,3] <- match(m[,3], unique(m[,3])); m }
    structure(list(
        success=cs(x$success),
        paths=cs(x$paths),
        responses=cs(x$responses),
        points=reix(x$points[x$points[,3] %in% ix,]),
        router=x$router),
        class="GHRoutes")
}

`c.GHRoutes` <- function(...) stop("Sorry, you cannot combine routes")
`[<-.GHRoutes` <- function(...) stop("Sorry, route lists are not mutable")
`[[<-.GHRoutes` <- function(...) stop("Sorry, route lists are not mutable")

