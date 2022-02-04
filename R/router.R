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
	if (make.default) {
		gh$router <- h
		gh$default.profile <- ch.names[[1]]
	}
	h
}

route <- function(start.lat, start.lon, end.lat, end.lon, profile, router=.default()) {
	if (missing(profile)) profile <- gh$default.profile
	if (length(start.lat) > 1) stop("I can route only one trip at a time")
	req <- .jnew("com.graphhopper.GHRequest", start.lat, start.lon, end.lat, end.lon, class.loader=.rJava.class.loader)
	req$setProfile(profile)
	rsp <- router$route(req)
	if (rsp$hasErrors())
		stop(errorCondition("GraphHopper routing restulted in errors", errors=rsp$getErrors(), response=rsp, class=c("GHRoutingError", "error")))
	paths <- rsp$getAll()
	n <- paths$size()
	pl <- lapply(seq.int(n), function(i) {
		path <- paths$get(i - 1L)
		list(points=matrix(.jcall(gh$tools, "[D", "pointList2ll", path$getPoints(), class.loader=.rJava.class.loader),,2),
		     waypoints=matrix(.jcall(gh$tools, "[D", "pointList2ll", path$getWaypoints(), class.loader=.rJava.class.loader),,2),
		     distance=path$getDistance(),
		     ascend=path$getAscend(),
		     descend=path$getDescend(),
		     time=path$getTime()/1000,
		     weight=path$getRouteWeight(),
		     jobj=path)
		})
	pl
}

gh.translation <- function(locale) {
	if (is.null(tmap <- gh$translation.map)) {
		tmap <- gh$translation.map <- .jnew("com.graphhopper.util.TranslationMap", class.loader=ghroute:::.rJava.class.loader)
		tmap$doImport()
	}
	tr <- tmap$get(locale)
	if (is.jnull(tr))
		stop("Translation not found for the given locale.")
        tr
}
