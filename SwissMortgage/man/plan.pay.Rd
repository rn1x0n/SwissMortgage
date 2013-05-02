\name{plan.pay}
\alias{plan.pay}
\title{Find the payments over time for a set of mortgages}
\usage{
  plan.pay(plan)
}
\arguments{
  \item{plan}{list giving details of a set of mortgages,
  see Details}
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
  interest, for each mortgage
}
\details{
  The set of mortages is defined by the list \code{plan}.
  Each element of ths list is a named list of lists
  defining a set of subsequent mortgages. The bottom level
  list has elements needed for \code{\link{interest.pay}}
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

plan.pay(plan)
}

