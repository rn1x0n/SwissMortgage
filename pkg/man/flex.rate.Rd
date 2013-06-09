\name{flex.rate}
\alias{flex.rate}
\title{Find the flexible interest rates over given future period}
\usage{
  flex.rate(times, rates, last.time = max(times))
}
\arguments{
  \item{times}{numeric vector of times in years (starts at
  time 0)}

  \item{rates}{numeric vector of rates for each time (first
  element is current rate)}

  \item{last.time}{extent the curve upto the this time}
}
\value{
  A data frame with elements \item{month}{time in months.
  Starts at month = 1, for the rate in the first month}
  \item{rate}{interest rate that month}
}
\description{
  This function defines the flexible interest rates for a
  given period.
}
\examples{
flex.rate(times = c(0, 3, 6, 10), rates = c(1, 2, 2.5, 5), last.time = 12)
}

