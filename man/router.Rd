\name{router}
\alias{router}
\alias{route}
\alias{route.matrix}
\alias{route.default}
\alias{gh.profile}
\alias{gh.translation}
\title{
  Street Routing Functions
}
\description{
\code{router} initializes a GraphHopper router with a specific profile and data sources.

\code{route} computes a route using the specified router.

\code{gh.profile} defines a new routing profile
}
\usage{
router(osm.file, path = "graphhopper-cache", profiles = "car", open = TRUE,
       make.default = TRUE)

route(x, ...)
\method{route}{matrix}(x, profile, times, alt=FALSE, output=c("matrix","sf","gh"),
             silent=FALSE, threads=1L, router=.default(), ...)
\method{route}{default}(x, start.lon, end.lat, end.lon, ...)

gh.profile(name, vehicle = name, weighting = "fastest", turn.costs = FALSE)
gh.translation(locale)
}
\arguments{
  \item{osm.file}{string, path to the Open Street Map file used for routing}
  \item{path}{string, path to the GraphHopper cache. If it doesn't exist the
  cache will be created when first used}
  \item{profiles}{either a character vector or a list with one or more
  objects returned from \code{gh.profile()}. This determines the
  properties for the routing such as available means of transport, speeds,
  turn penalties etc.}
  \item{open}{logical, if \code{TRUE} then the returned object can be
  used for routing. If \code{FALSE} then the cache is created and
  saved for later use, but no router is created for the current
  session.}
  \item{make.default}{logical, if \code{TRUE} then the resulting
    router is used as the default router for subseqeunt operations}
  \item{x}{object specifying the waypoints for the routes. See details
    for the supported input specifications.}
%  \item{start.lat}{scalar numeric, latitude of the starting point}
  \item{start.lon}{scalar numeric, longitude of the starting point}
  \item{end.lat}{scalar numeric, latitude of the end point}
  \item{end.lon}{scalar numeric, longitude of the end point}
  \item{profile}{either a string or a \code{ghprofile} object. If not
    specified, defaults to the first profile of the router}
  \item{output}{desired output format: \code{"matrix"} for a matrix
    describing the paths, \code{"sf"} for a data frame with a
    \code{"sfc"} geometry column or \code{"gh"} GraphHoppe objects.}
  \item{times}{optional, vector of start times for time-dependent routing (such as public transport)}
  \item{silent}{logical, if \code{TRUE} then warnings (such as
    routing errors) are suppressed}
  \item{alt}{logical, if \code{TRUE} then multiple alternative routes
    are included in the result is available, otherwise only the best
    route is reported in each case. Note that only the \code{"gh"}
    format supports multiple routes per query.}
  \item{threads}{integer, number of parallel threads to use for the routing.
  For large matrices on private machines \code{parallel::detectCores()}
  or similar may make sense}
  \item{router}{object returned from the \code{router()} function}
  \item{name}{string, name of the profile}
  \item{vehicle}{string, mode of transport}
  \item{weighting}{string, weighting type}
  \item{turn.costs}{logical, whether to use turn costs}
  \item{locale}{string, name of the language locale}
  \item{...}{parameters passed to methods}
}
\details{
\code{router} must be called at least once to initialize the routing
parameters.

\code{gh.profile} defines a profile to be used in the router. Note
that a new cache must be built any time the profiles are changed.

\code{route} calculates one or more routes for each source/destination
pair. The matrix form requires a matrix with exactly 4 columns
specifying latitude and longitude of the start and end point
respectiely. Each row of the input matrix will have exactly one entry in
the result.

The scalar (default) version is just a wrapper for
\code{matrix(c(start.lat, start.lon, end.lat, end.lon), 1)}.
Note that due to R method argument consistency requirements the first
argument of the default method is actually called \code{x} even though
it would better be named \code{start.lat}.

\code{gh.translation} loads translation from GraphHopper for
a given language locale and return the corresponding Java object.
\code{"en"} is always guaranteed to exist, others depend on the
GraphHopper installation.
}
\value{
\code{router} returns a router object.

\code{route}: the value depends on the \code{"output"} argument.

The \code{"matrix"} output is a matrix with columns \code{"lat"},
\code{"lon"} and \code{"index"} where \code{"index"} is the row number
in the input. Note that routing errors may occur (e.g. if the endpoint
is not near any roads) in which case the corresponding row may not
appear in the output. Use \code{output="gh"} for comprehensive error
reporting.

The \code{"sf"} output produces a data frame with a geometry column
representing the best path. It case of an error an empty line string is
used. Note that this output type requires the \code{sf} package.

The \code{"gh"} output produces an object of the class
\code{\link{GHRoutes}}
which holds the corresponding Java objects and thus allows for explicit
queries for different aspects of the routes including the support for
alternative routes.

\code{gh.translation} returns a Java object of the class
\code{com.graphhopper.util.TranslationMap}.

\code{gh.profile} returns a Java object of the class
\code{com.graphhopper.config.Profile}. Note that since it is a Java
object it can be modified by calling methods on it before passing
it to \code{router}.
}
%\references{
%}
\author{
Simon Urbanek
}
\note{
The functions above are a very thin API over GraphHopper Java
classes. It is possible to create more complex routing constraints by
using the Java classes directly. For example, the \code{gh.profile}
function returns an object of the class
\code{com.graphhopper.config.Profile} which can be further mutated
before passing it to the router. It corresponds directly to the
\code{profiles:} section of the YAML config files in the GraphHopper
documentation. By default all entries in \code{profiles}
are also automatically added to the contraction hierarchies
(see \code{profiles_ch:} section and
\code{com.graphhopper.config.CHProfile}).
}
%\seealso{
%}
\examples{
dst <- tempfile("osm", fileext=".pbf")
cache <- tempfile("router-cache")

## download OSM map of Cyprus
download.file("https://download.geofabrik.de/europe/cyprus-latest.osm.pbf", dst, mode="wb")

library(ghroute)
## create router for Cyprus with defaults car and fastest
router(dst, cache)

## compute single route between two points
rt = route(34.7592, 32.4123, 34.7996, 32.4517)

## by default the result is a lat/lon matrix
str(rt)

par(mar=rep(0,4))
plot(rt[,2], rt[,1], ty='l', asp=1/cos(32.4/180*pi))

## it is also possible to compute multiple different
## routes by supplying a start-end matrix
m = matrix(c(34.7592, 32.4123, 34.7996, 32.4517,
             34.7592, 32.4123, 34.6791, 33.044,
             34.9827, 33.7348, 35.1716, 33.3616),,4,TRUE)

## the result is a big matrix with "index" column
## refering to the input row
rts = route(m)

str(rts)

## it is possible to split the result into a list of
## matrices - each for one input row
res = rts2list(rts, nrow(m))
str(res)

plot(rts[,2], rts[,1], ty='n', asp=1/cos(32.4/180*pi))
for (o in res) lines(o[,2], o[,1])

## if you have sf package, you can get sfc instead:
have.sf <- requireNamespace("sf", quietly=TRUE)
if (have.sf) {
  rts = route(m, output="sf")
  print(rts)
  plot(rts$geometry)
}

## the maximum flexilibity is with "gh" output
rts = route(m, output="gh")

rts

rts[[1]]

## expert use - return turn-by-turn instructions by querying the Java object
## get the instructions object
ins <- rts[[1]]$path$getInstructions()
## use English locale
tr <- gh.translation("en")
## get the descriptions
sapply(seq.int(ins$size()),
       function(j) ins$get(j - 1L)$getTurnDescription(tr))

## clean up
unlink(c(dst, cache), TRUE, FALSE, FALSE)

}
\keyword{manip}
