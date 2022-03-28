GHPath <- function(path)
    structure(list(path=path), class="GHPath")

length.GHPath <- function(x) if (is.jnull(x$path)) 0L else x$path$getPoints()$size()

print.GHPath <- function(x, ..., short=FALSE) {
    cat("GHPath of ", length(x), " points", if (x$path$hasErrors()) " (with errors!)" else "", "\n", sep='')
    if (short) return(x)
    pt <- ll(x)
    head(pt, 4)
    if (nrow(pt) > 4) cat("[...]\n")
    invisible(x)
}

ll <- function(x, ...) UseMethod("ll")

ll.GHPath <- function(x, ...)
    matrix(.jcall(gh$tools, "[D", "pointList2ll", x$path$getPoints(), class.loader=.rJava.class.loader),,2,
           dimnames=list(NULL, c("lat", "lon")))

as.matrix.GHPath <- function(x, ...) ll(x, ...)

`[.GHPath` <- function(x, ...) {
    pt <- ll(x)
    pt[...]
}

`[<-.GHPath` <- function(...) stop("Sorry, paths are not mutable")
