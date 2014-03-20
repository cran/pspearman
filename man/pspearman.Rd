\name{pspearman}
\alias{pspearman}
\concept{Spearman's correlation coefficient}
\concept{Spearman's rho}

\title{Distribution function of Spearman's rho}

\description{
  This function provides three types of approximations of the distribution
  function of Spearman's rho. Besides the two approximations
  used in \code{cor.test(,method="spearman")}, which are AS89 and the t-distribution,
  this function allows to use precomputed null distribution for \code{n <= 22}.
  See \code{spearman.test} for the details of the algorithm used to compute this
  null distribution.
}

\usage{
    pspearman(s, n, lower.tail = TRUE,
        approximation = c("exact", "AS89", "t-distribution"))
}

\arguments{
  \item{s}{The observed value of S statistics \code{sum((rank(x) - rank(y))^2)}.}
  \item{n}{The number of observations.}
  \item{lower.tail}{If TRUE (the default), the probability of \code{S <= s}
  is computed. If FALSE, the probability of \code{S >= s} is computed.}
  \item{approximation}{Selection of the method of approximation of the distribution
  function.}
}

\value{
  Depending on \code{lower.tail}, either the probability of \code{S <= s}
  or of \code{S >= s} is computed, where S is the statistics
  \code{sum((rank(x) - rank(y))^2)}.
}

\details{
  See \code{spearman.test} for more detail.
}

\examples{
  pspearman(36, 10, approximation="exact") # [1] 0.005265377
  pspearman(36, 10, approximation="AS89")  # [1] 0.005825634
}

\keyword{ distribution }

