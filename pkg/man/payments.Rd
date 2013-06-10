\name{payments}
\alias{payments}
\title{Find the payments over time for a set of mortgages}
\usage{
  payments(shinyPlan, currentFixRates, flexRate)
}
\arguments{
  \item{shinyPlan}{a shiny plan list}

  \item{currentFixRates}{vector of length n giving the
  current fixed rate mortges for a fixed period of 1 to n
  years}

  \item{flexRate}{object from \code{\link{flex.rate}}
  giving the flexible rates.}
}
\value{
  A long format data frame with one row per mortgage per
  month with elements \item{month}{time in months}
  \item{mortgage}{factor giving the name of the mortgage,
  taken from the names in \code{plan}}
  \item{interest}{interest that month}
  \item{amortization}{amortization repayment that month}
  \item{payment}{sum of interest and repayment for that
  month}
}
\description{
  This function finds the payment each month for a set of
  fixed or amortization mortgages. This is broken down by
  how much is a amortization repayment and how much is
  interest, for each mortgage. This is a wrapper function
  for \code{\link{shinyPlan2plan}}, and
  \code{\link{plan.pay}}.
}
\details{
  The set of mortages is defined by the list
  \code{shinyPlan}. Each element of ths list is a named
  list of lists defining a set of subsequent mortgages.
}
\examples{
shinyPlan <- list(
  "Amortization" = list(
    list(debt = 1000, fix.rate = TRUE,  period = 8, interest.only = FALSE,  amortization.period = 20)
  ),
  "Fix1" = list(
    list(debt = 1000, fix.rate = FALSE, period = 4, interest.only = TRUE, amortization.period = NULL),
    list(debt = NULL, fix.rate = TRUE,  period = 3, interest.only = TRUE, amortization.period = NULL)
  )
)
currentFixRates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
flexRate <- flex.rate(times = c(0, 10), rates = c(1, 5))
payments(shinyPlan = shinyPlan, currentFixRates = currentFixRates,  flexRate = flexRate)
}

