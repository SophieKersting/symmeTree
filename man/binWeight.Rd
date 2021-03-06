% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binWeight.R
\name{binWeight}
\alias{binWeight}
\alias{MinSNIorJOfn}
\alias{Number2Binary}
\title{Binary weight and other functions for SNI and J}
\usage{
binWeight(number)

MinSNIorJOfn(n)

Number2Binary(number, noBits)
}
\arguments{
\item{number}{integer >=0}

\item{n}{integer >=0}

\item{noBits}{desired length of binary expansion (default: 32 bits)}
}
\value{
the number of 1's in the binary expansion (numeric)

the minimal SNI and J value for trees with n leaves (numeric)

numeric vector containing the binary expansion
}
\description{
\code{binWeight} - Calculates the binary weight, i.e. the number of 1's in
the binary expansion, of an integer.

\code{MinSNIorJOfn} - Calculates the minimal value of the symmetry nodes
index and the Rogers J index for trees with n leaves.

\code{Number2Binary} - Determines the binary expansion of an integer.
}
\examples{
binWeight(55)
MinSNIorJOfn(55)
Number2Binary(55,ceiling(log2(55)))
}
\author{
Sophie Kersting
}
