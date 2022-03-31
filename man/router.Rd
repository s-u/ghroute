\name{router}
\alias{router}
\alias{route}
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
route.matrix(x, profile, times, alt=FALSE, output=c("matrix","sf","gh"),
             silent=FALSE, router=.default())
route.default(start.lat, start.lon, end.lat, end.lon, profile,
             output=c("matrix", "sf", "gh"), alt=FALSE, router=.default())

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
  \item{start.lat}{scalar numeric, latitude of the starting point}
  \item{start.lon}{scalar numeric, longitude of the starting point}
  \item{end.lat}{scalar numeric, latitude of the end point}
  \item{end.lon}{scalar numeric, longitude of the end point}
  \item{profile}{either a string or a \code{ghprofile} object. If not
    specified, defaults to the first profile of the router}
  \item{output}{desired output format: \code{"matrix"} for a matrix
    describing the paths, \code{"sf"} for a data frame with a
    \code{"sfc"} geometry column or \code{"gh"} GraphHoppe objects.}
  \item{alt}{logical, if \code{TRUE} then multiple alternative routes
    are included in the result is available, otherwise only the best
    route is reported in each case. Note that only the \code{"gh"}
    format supports multiple routes per query.}
  \item{router}{object returned from the \code{router()} function}
  \item{name}{string, name of the profile}
  \item{vehicle}{string, mode of transport}
  \item{weighting}{string, weighting type}
  \item{turn.costs}{logical, whether to use turn costs}
  \item{locale}{string, name of the language locale}
}
\details{
\code{router} must be called at least once to initialize the routing
parameters.

\code{gh.profile} defines a profile to be used in the router. Note
that a new cache must be built any time the profiles are changed.

\code{route} calculates one or more routes for each source/destination
pair. The matrix form requires a matrix with exactly 4 columns
specifying latitude and longitude for the start and end point
respectiely. Each row of the input matrix will have exatly one entry in
the result.

The scalar (default) version is just a wrapper for
\code{matrix(c(start.lat, start.lon, end.lat, end.lon), 1)}.

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

%on success returns a list of routes where the first one
%is considered the best. Each route is a list with the following
%components:
%\item{points}{numeric matrix with two columns: latitude and longitude,
%representing the points along the route}
%\item{waypoints}{numeric matrix with two columns: latitude and longitude,
%representing the start/end points snapped to the locations usable by
%the routing algorithm}
%\item{distance}{numeric, trip distance in meters}
%\item{ascend}{numeric, total elevation change up}
%\item{descend}{numeric, total elevation change down}
%\item{time}{numeric, duration of the trip in seconds}
%\item{weight}{numeric, weight of the route - depends on the routing
%settings, for fastest-route routing it is usually same as the time}
%\item{jobj}{experimental, the path Java object (of the class
%\code{com.graphhopper.ResponsePath}), mainly for debugging
%or accessing advanced features such as turn-by-turn directions, trip
%legs for public transport etc.}

%For further details see the GraphHopper documentation.

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
download.file("https://download.geofabrik.de/europe/cyprus-latest.osm.pbf", dst)

library(ghroute)
## create router for Cyprus with defaults car and fastest
router(dst, cache)

## compute routes between two points
rts = route(34.7592, 32.4123, 34.7996, 32.4517)
## pick the best one (=first)
rt = rts[[1]]

par(mar=rep(0,4))
plot(rt$points[,2], rt$points[,1], ty='l', asp=1/cos(32.4/180*pi))
points(rt$waypoints[,2:1], col=2, pch=4, cex=2)

legend("topleft",,c(
  sprintf("Distance: \%.2fkm", rt$distance / 1e3),
  sprintf("Time: \%d:\%02d", as.integer(rt$time / 60), as.integer(rt$time \%\% 60))))

## expert use - return turn-by-turn instructions by querying the Java object
## get the instructions object
ins <- rt$jobj$getInstructions()
## use English locale
tr <- gh.translation("en")
## get the descriptions
sapply(seq.int(ins$size()),
       function(j) ins$get(j - 1L)$getTurnDescription(tr))

## clean up
unlink(c(dst, cache), TRUE, FALSE, FALSE)

}
\keyword{manip}
