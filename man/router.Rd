\name{router}
\alias{router}
\alias{route}
\alias{gh.profile}
\title{
  Street Routing Functions
}
\description{
\code{router} initializes a GraphHopper router with a specific profile and data sources.

\code{route} computest a route using the specified router.

\code{gh.profile} defines a new routing profile
}
\usage{
router(osm.file, path = "graphhopper-cache", profiles = "car", open = TRUE, make.default = TRUE)
route(start.lat, start.lon, end.lat, end.lon, profile, router = .default()) 
gh.profile(name, vehicle = name, weighting = "fastest", turn.costs = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{osm.file}{path to the Open Street Map file used for routing}
  \item{path}{path to the GraphHopper cache. If it doesn't exist the
  cache will be created when first used}
  \item{profiles}{either a character vector or a list with one or more
  objects returned from \code{gh.profile()}. This determines the
  properties for the routing such as available means of transort, speeds,
  turn penalties etc.}
  \item{open}{logical, if \code{TRUE} then the returned object can be
  used for routing. If \code{FALSE} then the cache is created and
  saved for later use, but no router is created for the current
  session.}
  \item{make.default}{logical, if \code{TRUE} then the resulting
  router is used as the default router for subseqeunt operations}
  \item{start.lat}{scalar numeric, latitude of the starting point}
  \item{start.lon}{scalar numeric, longitude of the starting point}
  \item{end.lat}{scalar numeric, latitude of the end point}
  \item{end.lon}{scalar numeric, longitude of the end point}
  \item{profile}{either a string or a ghprofile object. If not
  specified, defaults to the first profile of the router}  
  \item{router}{object returned from the \code{router()} function}
  \item{name}{string, name of the profile}
  \item{vehicle}{string, mode of transport}
  \item{weighting}{string, weighting type}
  \item{turn.costs}{logical, whether to use turn costs}
}
\details{
\code{router} must be called at least once to initialize the routing
parameters.

\code{gh.profile} defines a profile to be used in the router. Note
that a new cache must be built any time the profiles are changed.

\code{route} calculates one or more routes from the starting point to
the end point.
}
\value{
\code{router} returns a router object.

\code{route} on success returns a list of routes where the first one
is considered the best. Each route is a list with the following
components:
\item{points}{numeric matrix with two columns: latitude and longitude,
representing the points along the route}
\item{waypoints}{numeric matrix with two columns: latitude and longitude,
representing the start/end points snapped to the locations usable by
the routing algorithm}
\item{distance}{numeric, trip distance in meters}
\item{ascend}{numeric, total elevation change up}
\item{descend}{numeric, total elevation change down}
\item{time}{numeric, duration of the trip in seconds}
\item{weight}{numeric, weight of the route - depends on the routing
settings, for fastest-route routing it is usually same as the time}
\item{jobj}{experimental, the path Java object, mainly for debugging
or accessing advanced features such as turn-by-turn directions, trip
legs for publis transport etc.}

For further details see the GraphHopper documentation.
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
%\examples{
%}
\keyword{manip}
