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

route.matrix <- function(x, profile, alt=FALSE, simple=TRUE, silent=FALSE, router=.default()) {
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
    err <- .jcall(router, "[Z", "routeMatrix", x, profile, if (isTRUE(alt)) FALSE else TRUE)
    if (!silent && any(err)) warning("Warning, ", sum(err), " routes were not successful")
    if (simple) {
        if (alt)
            warning("alternative routes are discarded from the simple output")
        if (nrow(x) == 1)
            matrix(.jcall(router, "[D", "getPoints", 0L),, 2)
        else
            matrix(.jcall(router, "[D", "getAllPoints"),, 3)
    } else
        GHRoutes(!err, router)
}

route.default <- function(start.lat, start.lon, end.lat, end.lon, profile, router=.default()) {
    if (missing(profile)) profile <- gh$default.profile
    if (length(start.lat) > 1) stop("Use matrix form to compute multiple routes")
    route.matrix(matrix(c(start.lat, start.lon, end.lat, end.lon), 1),
                 simple=FALSE, alt=TRUE)
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
