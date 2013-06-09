\name{amortization}
\alias{amortization}
\title{Find the value of one amortization payment}
\usage{
  amortization(debt = 100, rate = 1, period = 5)
}
\arguments{
  \item{debt}{Current debt}

  \item{rate}{Annual percentage rate}

  \item{period}{Period of amortization repayments in years}
}
\value{
  The amortizaiton for one payment
}
\description{
  This function finds the value of one amortization
  payment, given the current total debt, the current annual
  interest rate and the period over which to pay the debt.
}
\examples{
amortization(debt = 1000, rate = 1, period = 5)
}

