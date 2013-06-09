\name{fix.rate}
\alias{fix.rate}
\title{Find the fixed interest rates over given future period}
\usage{
  fix.rate(start.time = 0, period = 5, current.fix.rates,
    flex.rate)
}
\arguments{
  \item{start.time}{the time in years when the fixed rate
  mortgage will start}

  \item{period}{the fixed period for the mortgage}

  \item{current.fix.rates}{vector of length n giving the
  current fixed rate mortges for a fixed period of 1 to n
  years}

  \item{flex.rate}{object from \code{\link{flex.rate}}
  giving the flexible rates. The period of this must be at
  least as long as \code{start.time}}
}
\value{
  The fixed interest rate
}
\description{
  This function predicts the fixed interest rates for a
  future time, given the fixed rates now, and the flexibles
  rates in the future
}
\examples{
flexRate <- flex.rate(times = c(0, 10), rates = c(1, 5))
current.fix.rates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
fix.rate(start.time = 0, period = 5, current.fix.rates = current.fix.rates, flex.rate = flexRate)
}

