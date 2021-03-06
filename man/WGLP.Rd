% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WGLP.R
\name{WGLP}
\alias{WGLP}
\title{Williams Transformation}
\usage{
WGLP(s)
}
\arguments{
\item{s}{The run size of design B, where s is a prime.}
}
\value{
The return value is an LH(s,s-1) when s is a prime or LH(s, phi(s)) when s is not a prime.
}
\description{
\code{WGLP} is Williams transformations of linearly transformed good lattice points. This provides a choice of design B.
}
\details{
Under the L1-distance, Wang et al. (2018,Theorem 2) constructed an LH(s,s-1) when s is a prime, where LH(s,s-1) is a Latin hypercube of s runs for s-1 factors. If s is not
a prime,  the Williams transformation can generate designs with s runs and phi(s) factors,
where phi(s) is the Euler function, that is, the number of positive integers smaller than and coprime to s.
}
\examples{
# Note that WGLP(7) produces an equi-distant LH(7,6)
B <- WGLP(7)
B <- WGLP(13)
}
\references{
Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
}
