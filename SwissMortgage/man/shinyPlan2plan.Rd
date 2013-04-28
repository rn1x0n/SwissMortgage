\name{shinyPlan2plan}
\alias{shinyPlan2plan}
\title{Convert a shinyPlan list to a plan list}
\usage{
  shinyPlan2plan(shinyPlan, currentFixRates, flexRate,
    timeHorizon = NULL)
}
\arguments{
  \item{shinyPlan}{a shiny plan list}

  \item{currentFixRates}{vector of length n giving the
  current fixed rate mortges for a fixed period of 1 to n
  years}

  \item{flexRate}{object from \code{\link{flex.rate}}
  giving the flexible rates.}

  \item{timeHorizon}{period over which the final set of
  morgages perdict over. Default is NULL, in which case it
  is the longest single period of the shinyPlan set}
}
\value{
  a plan list. See \code{\link{plan.pay}}
}
\description{
  This function converts a shinyPlan list to a more general
  plan list which can be processed by
  \code{\link{plan.pay}}
}
\details{
  shinyPlan is a list of named lists with elements
  \itemize{ \item{debt}{The amount borrowed}
  \item{fix.rate}{TRUE for a fixed rate of interest, FALSE
  for an interest rate that can change over time. These are
  calculated from \code{current.fix.rate} and
  \code{flex.rate}} \item{period}{Period of mortgage in
  years} \item{interest.only}{If TRUE then only interest is
  paid on the debt, if FALSE then the debt is amortized}
  \item{amortization.period}{if \code{interest.only = TRUE}
  then this gives the amortization period in years}
  \item{renew}{The period to keep renewing the mortgage
  for. 0 means the mortgage is not renewed} }
}
\examples{
shinyPlan <- list(
   "Fix1" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 5),
   "Fix2" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 0),
   "Amm1" = list(debt = 200000, fix.rate = FALSE, period = 10, interest.only = FALSE, amortization.period = 20, renew = 0)
)
currentFixRates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
flexRate <- flex.rate()
shinyPlan2plan(shinyPlan = shinyPlan, currentFixRates = currentFixRates,  flexRate = flexRate, timeHorizon = 10)
}

