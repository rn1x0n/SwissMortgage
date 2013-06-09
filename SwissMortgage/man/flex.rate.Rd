\name{flex.rate}
\alias{flex.rate}
\title{Find the flexible interest rates over given future period}
\usage{
  flex.rate(current.rate = 1, final.rate = 5, period = 10,
    extend.period = 50)
}
\arguments{
  \item{current.rate}{current interest rate}

  \item{final.rate}{final interest rate}

  \item{period}{period in years over which the rate will
  change}

  \item{extend.period}{after the period has ended keep at
  the final rate for this long}
}
\value{
  A data frame with elements \item{month}{time in months}
  \item{rate}{interest rate that month}
}
\description{
  This function predicts the flexible interest rates for a
  given period.
}
\examples{
flex.rate(current.rate = 1, final.rate = 5, period = 10)
}

