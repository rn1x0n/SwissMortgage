# OLD STUFF

#' Find the flexible interest rates over given future period
#'
#' This function predicts the flexible interest rates for a given period.
#'
#' @param current.rate current interest rate
#' @param period.current period the current rate will stay as it is
#' @param final.rate   final interest rate
#' @param period       period in years over which the rate will change 
#' @param  extend.period after the period has ended keep at the final rate for this long
#' @return  A data frame with elements
#' \item{month}{time in months}
#' \item{rate}{interest rate that month}
#' @export
#' @examples
#' flex.rate(current.rate = 1, final.rate = 5, period = 10)
flex.rate <- function(
  current.rate = 1,   # Current interest rate
  period.current = 0, # Period the current rate will stay as it is
  final.rate = 5,     # Final interest rate
  period = 10,    # Period in years over which the rate will change 
  extend.period = 50 # After the period has ended keep at the final rate for this long
){
  
  total.period <- period.current + period
  
  # Flat period
  rate <- rep(current.rate, length = period.current * 12)
  
  # Change period 
  month <- seq(1, period * 12, by = 1)
  rate <- c(rate, current.rate + (month-1) * (final.rate - current.rate)/(period * 12 -1))
  
  # Extend period
  rate <- c(rate, rep(rate[length(rate)], times = (extend.period - total.period)*12))
  
  data.frame(month = 1:length(rate), rate)
}
