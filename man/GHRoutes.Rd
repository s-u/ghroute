\name{GHRoutes}
\alias{GHRoutes}
\title{GraphHopper Routes Class}
\description{
\code{GHRoutes} is a class of objects representing results from the
GraphHopper routing when \code{output="gh"} is used.

I behaves like a non-mutable list such that usual
operations such as subsetting, element extraction, iteration and
\code{length()} work as expected.
However, the object should be consiered non-mutable,
i.e., it is not possible to assign new values into an existing
object. Although subsetting is allowed, concatenation is not.

The object itself contains Java references
which enables low-level access to the underlying results using tha
GraphHopper Java API.

It is the only result type which supports representation of
alternative routes. If \code{alt=TRUE} is used then each (virtual)
element contains a list of result paths of which the first one is
considerend the best according to GraphHopper.

Note that this API is considered experimental and is subject to change.
}
\author{
Simon Urbanek
}
\keyword{manip}
