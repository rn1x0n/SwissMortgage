\name{shinyPlan2plan}
\alias{shinyPlan2plan}
\title{Convert a shinyPlan list to a plan list}
\usage{
  shinyPlan2plan(shinyPlan, currentFixRates, flexRate)
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
  a plan list. See \code{\link{plan.pay}}
}
\description{
  This function converts a shinyPlan list to a more general
  plan list which can be processed by
  \code{\link{plan.pay}}
}
\details{
  The set of mortages is defined by the list
  \code{ShinyPlan}. Each element of ths list is a named
  list of lists defining a set of subsequent mortgages. The
  bottom level list has elements \itemize{ \item{debt}{The
  amount borrowed. For lists not the first in the mortgage
  set, this can be NA. In this case, the debt is calculated
  from the debt left by the previous mortgage. This allows
  for the debt to be adjusted after a mortgage period is
  over.} \item{fix.rate}{TRUE for a fixed rate of interest,
  FALSE for an interest rate that can change over time.
  These are calculated from \code{current.fix.rate} and
  \code{flex.rate}} \item{period}{Period of mortgage in
  years} \item{interest.only}{If TRUE then only interest is
  paid on the debt, if FALSE then the debt is amortized}
  \item{amortization.period}{if \code{interest.only = TRUE}
  then this gives the amortization period in years} }
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
flexRate <- flex.rate()
shinyPlan2plan(shinyPlan = shinyPlan, currentFixRates = currentFixRates,  flexRate = flexRate)
}

