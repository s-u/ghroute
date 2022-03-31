gh <- new.env(TRUE, emptyenv())

.onLoad <- function(libname, pkgname) {
	gh$lib <- libname
	gh$pkg <- pkgname
	gh$init <- FALSE
	.jpackage(gh$pkg, lib.loc=gh$lib, own.loader=TRUE)
	gh$tools <- .jnew("nz.urbanek.RGH.GHTools", class.loader=.rJava.class.loader)
}

gh.profile <- function(name, vehicle=name, weighting="fastest", turn.costs=FALSE) {
	name <- as.character(name)
	if (length(name) != 1) stop("invalid profile name")
	if (length(vehicle) != 1) stop("invalid vehicle")
	car <- .jnew("com.graphhopper.config.Profile", name, class.loader=.rJava.class.loader)
	invisible(car$setVehicle(vehicle)$setWeighting(weighting)$setTurnCosts(turn.costs))
}

.default <- function() if (is.null(gh$router)) stop("No default router is present") else gh$router

router <- function(osm.file, path="graphhopper-cache", profiles="car", open=TRUE, make.default=TRUE) {
	osm.file <- path.expand(osm.file)
	path <- path.expand(path)
	if (!file.exists(osm.file)) stop("osm.file does not exist")
	if (is.character(profiles))
		profiles <- lapply(profiles, gh.profile)
	h <- .jnew("com.graphhopper.GraphHopper", class.loader=.rJava.class.loader)
	h$setOSMFile(osm.file)
	h$setGraphHopperLocation(path)
	## we cannot use .jarray(profiles, "com.graphhopper.config.Profile"), because .jarray is missing class.loader argument
	.jcall(h, "Lcom/graphhopper/GraphHopper;", "setProfiles", .jcast(.jarray(profiles), "[Lcom/graphhopper/config/Profile;"))
	## prepare Contraction Hierarchy profiles
	ch.names <- lapply(profiles, function(o) o$getName())
	chp <- .jarray(lapply(ch.names, function(name) .jnew("com.graphhopper.config.CHProfile", name, class.loader=.rJava.class.loader)))
	.jcall(h$getCHPreparationHandler(), "Lcom/graphhopper/routing/ch/CHPreparationHandler;", "setCHProfiles", .jcast(chp, "[Lcom/graphhopper/config/CHProfile;"))
	if (open) h$importOrLoad() else h$importAndClose()
        rt <- .jnew("nz.urbanek.RGH.GHRouter", h, class.loader=.rJava.class.loader)
	if (make.default) {
		gh$router <- rt
		gh$default.profile <- ch.names[[1]]
	}
	rt
}

route <- function(x, ...)
    UseMethod("route")

route.matrix <- function(x, profile, times, alt=FALSE, output=c("matrix","sf","gh"), silent=FALSE,
	                 threads=1L, router=.default(), ...) {
    output <- match.arg(output)
    switch (output,
            matrix=,
                sf=if (alt) {
                    warning("only gh output supports alternative routes, returning best route only")
                    alt <- FALSE
                })
    if (missing(profile)) profile <- gh$default.profile
    if (ncol(x) == 2) { ## one route, many points
        if (nrow(x) < 2)
            stop("Need at least two waypoints")
        if (nrow(x) == 2)
            x <- matrix(t(x), 1)
        else
            stop("Multi-point routes are not implemented yet")
    }
    if (ncol(x) != 4)
        stop("Input matrix must have four columns: lat1, lon1, lat2 and lon2")
    storage.mode(x) <- "double"
    succ <- if (missing(threads) || threads < 2L)
       .jcall(router, "[Z", "routeMatrix", x, profile, if (isTRUE(alt)) FALSE else TRUE)
    else
       .jcall(router, "[Z", "routeMatrixParallel", x, profile, if (isTRUE(alt)) FALSE else TRUE, as.integer(threads)[1])
    
    if (!silent && !all(succ)) warning("Warning, ", sum(!succ), " routes were not successful")
    if (output == "gh")
        return(GHRoutes(succ, router))

    res <- if (nrow(x) == 1) {
        rt <- matrix(.jcall(router, "[D", "getPoints", 0L),, 2,
                     dimnames=list(NULL, c("lat", "lon")))
        if (output == "sf")
            sf::st_sfc(list(sf::st_linestring(rt[,2:1])), crs=4326)
        else
            rt
    } else {
        rts <- matrix(.jcall(router, "[D", "getAllPoints"),, 3,
                      dimnames=list(NULL, c("lat", "lon", "index")))
        if (output == "sf") {
            ## to handle missing/failed paths correctly we
            ## create a full list and then assign the individual
            ## elements; Note that sf_sfc is *really* slow
            ## and there is nothing we can do about it...
            l <- vector("list", nrow(x))
            if (nrow(rts) > 0L) {
                rl <- rle(rts[,3])
                cs <- cumsum(rl$lengths)
                cs1 <- c(0L, cs) + 1L
                for (i in seq_along(rl$lengths))
                    l[[rl$value[i]]] <- sf::st_linestring(rts[cs1[i]:cs[i], 2:1])
            }
            sf::st_sfc(l)
        } else
            rts
    }
    ## create a data frame with extra info for sf output
    if (inherits(res, "sfc")) {
        df <- data.frame(time=.jcall(router, "[D", "getTimes"),
                         dist=.jcall(router, "[D", "getDistances"))
        sf::st_geometry(df) <- res
        df
    } else res
}

rts2list <- function(rts, nrow=max(index), index=rts[,3]) {
    l <- vector("list", nrow)
    if (nrow > 0L) {
        rl <- rle(index)
        cs <- cumsum(rl$lengths)
        cs1 <- c(0L, cs) + 1L
        for (i in seq_along(rl$lengths))
            l[[rl$value[i]]] <- rts[cs1[i]:cs[i],]
    }
    l
}

route.default <- function(x, start.lon, end.lat, end.lon, ...) {
    if (length(x) > 1) stop("Use matrix form to compute multiple routes")
    route.matrix(matrix(c(x, start.lon, end.lat, end.lon), 1), ...)
}

gh.translation <- function(locale) {
	if (is.null(tmap <- gh$translation.map)) {
		tmap <- gh$translation.map <- .jnew("com.graphhopper.util.TranslationMap", class.loader=.rJava.class.loader)
		tmap$doImport()
	}
	tr <- tmap$get(locale)
	if (is.jnull(tr))
		stop("Translation not found for the given locale.")
        tr
}
