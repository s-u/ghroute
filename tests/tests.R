library(ghroute)

if (!nzchar(Sys.getenv("RUN_TESTS"))) {
    cat("\nNOTE: tests are not run unless RUN_TESTS is set due to high memory,\n      time and space requirements for real-world routing.\n\n")
    base::quit("no")
}

pbf <- "osm/cyprus.pbf"
pbf.url <- "https://download.geofabrik.de/europe/cyprus-latest.osm.pbf"
ghcache <- "gh-cache-cyprus"

if (!file.exists(pbf)) {
    if (!dir.exists("osm"))
        dir.create("osm")
    cat("Downloading OSM map\n")
    download.file(pbf.url, pbf, mode="wb", quiet=TRUE)
}

if (dir.exists(ghcache)) {
    cat("NOTE: GraphHopper route cache", ghcache, "already exists.\n")
    if (nzchar(Sys.getenv("CLEAR_CACHE"))) {
        cat("Clearing cache.")
        unlink(ghcache, TRUE)
    }
}

cat("--- Creating Router\n")
router("osm/cyprus.pbf", ghcache)

cat("\n--- Routing\n")
## two-point version
rts = route(34.7592, 32.4123, 34.7996, 32.4517)

## matrix version
m = matrix(c(34.7592, 32.4123, 34.7996, 32.4517,
             34.7592, 32.4123, 34.6791, 33.044,
             34.9827, 33.7348, 35.1716, 33.3616),,4,TRUE)
## simple, best
rts = route(m)

rts2st <- function(rts)
    sf::st_as_sfc(lapply(split(rts, rts[,3]), function(o) sf::st_linestring(matrix(o,,3)[,2:1])), crs=4326)

have.sf <- requireNamespace("sf", quietly=TRUE)
if (have.sf) {
    st <- rts2st(rts)
    plot(st)
}

rt = route(m, simple=FALSE)
