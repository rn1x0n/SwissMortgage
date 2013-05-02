\name{summaryPay}
\alias{summaryPay}
\title{Summary table of mortgages}
\usage{
  summaryPay(pay, timeHorizon = NULL)
}
\arguments{
  \item{pay}{an object from plan.pay}

  \item{timeHorizon}{optional maximum time in years for the
  summary calculations}
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

plan <- plan.pay(plan)
ribbon.plot.pay(plan)
}

