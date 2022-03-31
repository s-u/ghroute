GHPath <- function(path)
    structure(list(path=path), class="GHPath")

length.GHPath <- function(x) if (is.jnull(x$path)) 0L else x$path$getPoints()$size()

print.GHPath <- function(x, ..., short=FALSE) {
    if (is.jnull(x$path)) {
        cat("Empty GHPath\n")
        return(invisible(x))
    }
    cat("GHPath of ", length(x), " points", if (x$path$hasErrors()) " (with errors!)" else "", "\n", sep='')
    if (short) return(invisible(x))
    pt <- ll(x)
    print(head(pt, 4))
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

`[<-.GHPath` <- function(..., value) stop("Sorry, paths are not mutable")
