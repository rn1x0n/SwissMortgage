\name{ribbon.plot.pay}
\alias{ribbon.plot.pay}
\title{Ribbon plot for payments}
\usage{
  ribbon.plot.pay(pay, y = "payment", xmax = NULL,
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
  A ggplot2 ribbon plot
}
\description{
  Ribbon ploy of payments
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

