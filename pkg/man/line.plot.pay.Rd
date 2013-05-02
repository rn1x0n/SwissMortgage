\name{line.plot.pay}
\alias{line.plot.pay}
\title{Line plot for payments}
\usage{
  line.plot.pay(pay, y = "payment", xmax = NULL,
    ymax = NULL)
}
\arguments{
  \item{pay}{an object from \code{\link{plan.pay}}}

  \item{y}{y value to plot, either "payment", "interest",
  "amortization"}

  \item{xmax}{optional x-axis upper limit}

  \item{ymax}{optional y-axis upper limit}
}
\value{
  A ggplot2 line plot
}
\description{
  Line ploy of payments
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
line.plot.pay(plan)
}

