\name{summaryPay}
\alias{summaryPay}
\title{Summary table of mortgages}
\usage{
  summaryPay(pay, timeHorizon = NULL, xtable = FALSE)
}
\arguments{
  \item{pay}{an object from \code{\link{plan.pay}}}

  \item{timeHorizon}{optional maximum time in years for the
  summary calculations}

  \item{xtable}{logical. If TRUE a xtable is printed as
  html}
}
\value{
  A summary table
}
\description{
  Summary table of mortgages
}
\examples{
plan <- list(
 "Fix1" = list(
   list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
   list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
 ),
 "Amortization" = list(
   list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
 )
)

pay <- plan.pay(plan)
summaryPay(pay)
}

