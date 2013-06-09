\name{interest.pay}
\alias{interest.pay}
\title{Find the payments over time for a mortgage}
\usage{
  interest.pay(debt = 100, rate = 1, period = 1,
    interest.only = TRUE, amortization.period = 20)
}
\arguments{
  \item{debt}{the amount borrowed}

  \item{rate}{either a single value giving the annual
  percentage rate, or a vector of length \code{12*period}
  giving the interest rate by month}

  \item{period}{period (years) to calculate mortgage for}

  \item{interest.only}{logical. If TRUE perform
  calculaitons for an interest only mortgage, if FALSE then
  amoritize}

  \item{amortization.period}{the repayment period of an
  amortization mortgage}
}
\value{
  A data frame with elements \item{month}{time in months}
  \item{interest}{interest that month}
  \item{amortization}{amortization repayment that month}
  \item{payment}{sum of interest and amortization
  repayment}
}
\description{
  This function finds the payment each month for a fixed or
  amortization mortgage. This is also broken down by how
  much is a amortization repayment and how much is
  interest.
}
\examples{
interest.pay(debt = 1000, rate = 1, period = 5, interest.only = FALSE, amortization.period = 20)
}

