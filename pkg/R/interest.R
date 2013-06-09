# FUNCITONS FOR FINDING INTEREST PAYMENTS

#####################################################################
# PAYMENTS

#====================================================================

#' Find the flexible interest rates over given future period
#'
#' This function defines the flexible interest rates for a given period.
#'
#' @param times     numeric vector of times in years (starts at time 0)
#' @param rates     numeric vector of rates for each time (first element is current rate)
#' @param last.time extent the curve upto the this time
#' @return  A data frame with elements
#' \item{month}{time in months. Starts at month = 1, for the rate in the first month}
#' \item{rate}{interest rate that month}
#' @export
#' @examples
#' flex.rate(times = c(0, 3, 6, 10), rates = c(1, 2, 2.5, 5), last.time = 12)
flex.rate <- function(
  times,     
  rates,     
  last.time = 50  
){
  
  # Data checks
  # times and rates are the same length
  if(length(times) != length(rates)) stop("Times and rates must be the same length")
  # first element of times much be zero
  if(times[1] != 0) stop("First element of times must = 0")
  
  # Convert years to months
  months <- times * 12
  
  # Find the gradients (beta) and intercepts (alpha) of the piecewisw linear function
  alpha <- beta <- c()
  for(i in 1:(length(times) - 1)){ 
    beta[i] <- (rates[i+1] - rates[i])/(months[i+1] - months[i])
    alpha[i] <- rates[i] - beta[i]*months[i]  
  }
  
  # Build the function
  month <- 0:max(max(months), last.time * 12)
  # function for each section, and section index for each month
  funs <- lapply(1:length(beta), function(i){alpha[i] + beta[i] * month})
  
  month.ind <- rep(NA, length(month))
  for(i in 1:length(beta)){ 
    month.ind[month >= months[i] & month < months[i+1]] <- i
  }
  month.ind[is.na(month.ind)] <- length(beta)
  
  # rate for each month
  rate <- sapply(1:length(month), function(m){funs[[month.ind[m]]][m]})
   
  # extend upto last time
    rate[month > max(months)] <- rate[month == max(months)]

  
  return(data.frame(month = month + 1, rate = rate))
}


#====================================================================
#' Find the fixed interest rates over given future period
#'
#' This function predicts the fixed interest rates for a future time, given 
#' the fixed rates now, and the flexibles rates in the future
#'
#' @param start.time        the time in years when the fixed rate mortgage will start
#' @param period            the fixed period for the mortgage
#' @param current.fix.rates vector of length n giving the current fixed rate mortges for a fixed period of 1 to n years
#' @param flex.rate         object from \code{\link{flex.rate}} giving the flexible rates. The period of this must be at least as long as \code{start.time}
#' @return The fixed interest rate
#' @export
#' @examples
#' flexRate <- flex.rate(times = c(0, 10), rates = c(1, 5))
#' current.fix.rates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
#' fix.rate(start.time = 0, period = 5, current.fix.rates = current.fix.rates, flex.rate = flexRate)
fix.rate <- function(
  start.time = 0,   # The time in years when the fixed rate mortgage will start
  period = 5,     # The fixed period for the mortgage
  current.fix.rates,   # vector of length n giving the current fixed mortgage rates for a fixed period of 1 to n years. period must be less than or equal to n.
  flex.rate  # Object from \code{\link{flex.rate}} giving the flexible rates. The period of this must be at least as long as \code{start.time}. If NULL then the current fix.rate is returned.
){
  
  # Check that flex.rate is long enough.
  start.time.month <- floor(start.time * 12)+1
  
#   if(is.null(flex.rate)){
#     flex.rate <- data.frame(month = 1:start.time.month, rate = c(1,1))
#   } 
  
  # Must have flex rate predicted for the start.time
  if(start.time > max(flex.rate$month/12) ) stop(paste("start.time = ", start.time, " years, but it must be less than or equal to the maximum month in flex.time (", max(flex.rate$month), " months)", sep = ""))
  
  # period must be less than or equal to the length of current.fix.rates
  if(period > length(current.fix.rates) ) stop(paste("period = ", period, " years, but it must be less than or equal to the length of current.fix.rates (", length(current.fix.rates), ")", sep = ""))
  
#   if(is.null(current.fix.rates)){
#     current.fix.rates <- c("1" = 0.980,
#                            "2" = 0.960,
#                            "3" = 1.020,
#                            "4" = 1.150,
#                            "5" = 1.300,
#                            "6" = 1.460,
#                            "7" = 1.620,
#                            "8" = 1.780,
#                            "9" = 1.920,
#                            "10" = 2.060)
#   }
  
  
  # difference between current.fix.rates to flex.rate for month 1
  # ASSUME these difference are the same over time
  
  rate.diff <- as.matrix(current.fix.rates - flex.rate$rate[1])
  flex.rate.mat <- as.matrix(flex.rate$rate)
  future.fix.rates <- sapply(rate.diff, function(x){flex.rate$rate + x})
  
  
  return(future.fix.rates[start.time.month, period])
  
}

#====================================================================
#' Convert a shinyPlan list to a plan list
#'
#' This function converts a shinyPlan list to a more general plan list
#' which can be processed by \code{\link{plan.pay}}
#'
#' @param shinyPlan           a shiny plan list 
#' @param currentFixRates   vector of length n giving the current fixed rate mortges for a fixed period of 1 to n years
#' @param flexRate           object from \code{\link{flex.rate}} giving the flexible rates. 
#' @return a plan list. See \code{\link{plan.pay}}
#' @details The set of mortages is defined by the list \code{ShinyPlan}. 
#' Each element of ths list is a named list of lists defining a set of subsequent mortgages.
#' The bottom level list has elements
#' \itemize{
#' \item{debt}{The amount borrowed. For lists not the first in the mortgage set, this can be NA. In this case, the debt is calculated from the debt left by the previous mortgage. This allows for the debt to be adjusted after a mortgage period is over.}
#' \item{fix.rate}{TRUE for a fixed rate of interest, FALSE for an interest rate that can change over time. These are calculated from \code{current.fix.rate} and \code{flex.rate}}
#' \item{period}{Period of mortgage in years}
#' \item{interest.only}{If TRUE then only interest is paid on the debt, if FALSE then the debt is amortized}
#' \item{amortization.period}{if \code{interest.only = TRUE} then this gives the amortization period in years}
#' }
#' @export
#' @examples
#' shinyPlan <- list(
#'   "Amortization" = list(
#'     list(debt = 1000, fix.rate = TRUE,  period = 8, interest.only = FALSE,  amortization.period = 20)
#'   ),
#'   "Fix1" = list(
#'     list(debt = 1000, fix.rate = FALSE, period = 4, interest.only = TRUE, amortization.period = NULL),
#'     list(debt = NULL, fix.rate = TRUE,  period = 3, interest.only = TRUE, amortization.period = NULL)
#'   )
#' )
#' currentFixRates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
#' flexRate <- flex.rate(times = c(0, 10), rates = c(1, 5))
#' shinyPlan2plan(shinyPlan = shinyPlan, currentFixRates = currentFixRates,  flexRate = flexRate)
shinyPlan2plan <- function(
  shinyPlan,
  currentFixRates,   # vector of length n giving the current fixed rate mortges for a fixed period of 1 to n years
  flexRate    # Object from \code{\link{flex.rate}} giving the flexible rates. The period of this must be at least as long as \code{start.time} 
#  timeHorizon = NULL # period to predict to
){
  
  # Find the longest period in the list
    timeHorizon <- max(sapply(shinyPlan, function(x){
      sum(sapply(x, function(y){y$period})) # period of a mortgage
      })
                       )
  
  # Build plan
  
  plan <- list()
  
  for(name in names(shinyPlan)){
    plan[[name]] <- list()
    start.time <- 0
    amortization.period <- shinyPlan[[name]][[1]]$amortization.period
    
    debt <- shinyPlan[[name]][[1]]$debt
    if(is.null(debt) || !is.numeric(debt)) stop(paste("shinyPlan[[", name, "]][[1]]$debt must be a numeric", sep = ""))
    
    for(i in 1:length(shinyPlan[[name]])){
      # Use debt if given, othewise use the calculated value
     if(!is.null(shinyPlan[[name]][[i]]$debt)){
       debt <- shinyPlan[[name]][[i]]$debt
     } 
     
      period <- shinyPlan[[name]][[i]]$period
     
    # Find the interest rate. 
      #  Fixed rate value. Function of time start
      if(shinyPlan[[name]][[i]]$fix.rate){
        rate <- fix.rate(
          start.time = start.time, 
          period = period, 
          current.fix.rates = currentFixRates, 
          flex.rate = flexRate)
      } else { # flexible interest rate vector. Function of time start and period
        rate <- subset(flexRate, 
                       subset = month >= (start.time*12 + 1) & month <= ((start.time+period)*12)
        )[,"rate"]
      }
    
    # Build the plan for this mortgage
    plan[[name]][[i]] <- list(debt = debt, 
                              rate = rate, 
                              period = period, 
                              interest.only = shinyPlan[[name]][[i]]$interest.only, 
                              amortization.period = amortization.period)
    
    # Update the start time, debt and amortization.period
    start.time <- start.time + period
    
    if(!shinyPlan[[name]][[i]]$interest.only){
      this.pay <- interest.pay(debt = debt, rate = rate, period = period, interest.only = FALSE, amortization.period = amortization.period)
      debt <- debt - sum(this.pay$amortization)
      amortization.period <- amortization.period - period
    } 
    }
  }
    return(plan)
    
}

#     plan[[name]] <- list()
#     start.time <- 0
#     debt <- shinyPlan[[name]]$debt
#     amortization.period <- shinyPlan[[name]]$amortization.period
#     i <- 0
#     
#     while(start.time < timeHorizon){ # Keep renewing until period is long enough
#       i <- i + 1
#       
#       # Period for this mortgage. Break out if period ==0
#       period <- ifelse(i == 1, shinyPlan[[name]]$period, shinyPlan[[name]]$renew)
#       if(period == 0) break()
#       
#       # Find the interest rate. 
#       #  Fixed rate value. Function of time start
#       if(shinyPlan[[name]]$fix.rate){
#         rate <- fix.rate(
#           start.time = start.time, 
#           period = period, 
#           current.fix.rates = currentFixRates, 
#           flex.rate = flexRate)
#       } else { # flexible interest rate vector. Function of time start and period
#         rate <- subset(flexRate, 
#                        subset = month >= (start.time*12 + 1) & month <= ((start.time+period)*12)
#         )[,"rate"]
#       }
#       
#       # Build the plan for this mortgage
#       plan[[name]][[i]] <- list(debt = debt, 
#                                 rate = rate, 
#                                 period = period, 
#                                 interest.only = shinyPlan[[name]]$interest.only, 
#                                 amortization.period = amortization.period)
#       
#       # Update the start time, debt and amortization.period
#       start.time <- start.time + period
#       
#       if(!shinyPlan[[name]]$interest.only){
#         this.pay <- interest.pay(debt = debt, rate = rate, period = period, interest.only = FALSE, amortization.period = amortization.period)
#         debt <- debt - sum(this.pay$amortization)
#         amortization.period <- amortization.period - period
#       } 
#       
#     }
#   }
#   

#====================================================================
#' Find the value of one amortization payment
#'
#' This function finds the value of one amortization payment, given 
#' the current total debt, the current annual interest rate and the 
#' period over which to pay the debt.
#'
#' @param   debt Current debt
#' @param   rate Annual percentage rate 
#' @param   period Period of amortization repayments in years 
#' @return  The amortizaiton for one payment
#' @export
#' @examples
#' amortization(debt = 1000, rate = 1, period = 5)
amortization <- function(
  debt = 100,   
  rate = 1,      
  period = 5    
  
){
  month.rate <- rate/12/100  
  num.pay <- period * 12
  payment <- (month.rate * debt * (1 + month.rate)^num.pay)/((1 + month.rate)^num.pay - 1)
  return(payment) 
}

#====================================================================
#' Find the payments over time for a mortgage
#'
#' This function finds the payment each month for a fixed or amortization
#' mortgage. This is also broken down by how much is a  amortization repayment and how
#' much is interest. 
#'
#' @param   debt    the amount borrowed 
#' @param   rate    either a single value giving the annual percentage rate, or a vector of length \code{12*period} giving the interest rate by month
#' @param   period  period (years) to calculate mortgage for
#' @param   interest.only     logical. If TRUE perform calculaitons for an interest only mortgage, if FALSE then amoritize
#' @param   amortization.period the repayment period of an amortization mortgage
#' @return  A data frame with elements
#' \item{month}{time in months}
#' \item{interest}{interest that month}
#' \item{amortization}{amortization repayment that month}
#' \item{payment}{sum of interest and amortization repayment}
#' @export
#' @examples
#' interest.pay(debt = 1000, rate = 1, period = 5, interest.only = FALSE, amortization.period = 20)
interest.pay <- function(
  debt = 100, 
  rate = 1,   # Annual percentage rate. Either a sinlge number, or a vector giving the rate each month
  period = 1, # Period of mortgage in years. Must be >=1
  interest.only = TRUE,  # it TRUE then a interest only mortgage, if FALSE then amoritize
  amortization.period = 20
){
  
  if(period < 1) stop("'period' must be >= 1")
  
  ln <- period * 12  
  month <- seq(1, ln, by = 1)
  
  if(length(rate) == 1) rate <- rep(rate, ln)
  if(length(rate) < ln) stop(paste("rate must be of length", ln, "(period * 12)"))
  
  # Find payment each month
  if(interest.only){
    interest <- payment <- debt * rate/12/100
    amortization <- rep(0, ln)
  } else {
    payment <- interest <- current.debt <- amortization <- rep(NA, ln)
    payment[1] <- amortization(debt = debt, rate = rate[1], period = amortization.period)
    interest[1] <- debt * rate[1]/12/100
    current.debt[1] <- debt + interest[1] - payment[1]
    amortization[1] <- payment[1] - interest[1]
    
    for(i in 2:ln){
      payment[i] <- amortization(
        debt = current.debt[i-1], 
        rate = rate[i], 
        period = amortization.period - (i-1)/12)
      
      interest[i] <- current.debt[i-1] * rate[i]/12/100
      current.debt[i] <- current.debt[i-1] + interest[i] - payment[i]
      amortization[i] <- payment[i] - interest[i]
    }
    
  }
  
  # Return payments
  return(
    data.frame(
      month = month,
      interest = interest,
      amortization = amortization,
      payment = payment
    )
  )
  
}

#====================================================================
#' Find the payments over time for a set of mortgages
#'
#' This function finds the payment each month for a set of fixed or amortization
#' mortgages. This is broken down by how much is a amortization repayment and how
#' much is interest, for each mortgage 
#'
#' @param   plan    list giving details of a set of mortgages, see Details 
#' @details The set of mortages is defined by the list \code{plan}. 
#' Each element of ths list is a named list of lists defining a set of subsequent mortgages.
#' The bottom level list has elements needed for \code{\link{interest.pay}}
#' @return  A long format data frame with one row per mortgage per month with elements
#' \item{month}{time in months}
#' \item{mortgage}{factor giving the name of the mortgage, taken from the names in \code{plan}}
#' \item{interest}{interest that month}
#' \item{amortization}{amortization repayment that month}
#' \item{payment}{sum of interest and repayment for that month}
#' @export
#' @examples
#' plan <- list(
#'  "Fix1" = list(
#'    list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
#'    list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
#'  ),
#'  "Amortization" = list(
#'    list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
#'  )
#' )
#' 
#' plan.pay(plan)
plan.pay <- function(
  plan
){
  
  require(plyr)
  
  # Error checks
  if(!is.na(match("Total", names(plan)))) stop("The name 'Total' is not allowed for a list name in plan")
   
  interest <- list()
  for(name in names(plan)){
    interest[[name]] <- list()
    month <- 0
    for(i in 1:length(plan[[name]])){
      interest[[name]][[i]] <- interest.pay(
        debt = plan[[name]][[i]]$debt, 
        rate = plan[[name]][[i]]$rate, 
        period = plan[[name]][[i]]$period, 
        interest.only = plan[[name]][[i]]$interest.only,
        amortization.period = plan[[name]][[i]]$amortization.period)   
      
      interest[[name]][[i]]$month <-  interest[[name]][[i]]$month + month
      month <- max(interest[[name]][[i]]$month)
      
      interest[[name]][[i]]$mortgage <- name
      
      interest[[name]][[i]]$sub.mortgage <- i
      interest[[name]][[i]]$starting.debt <- plan[[name]][[i]]$debt
      interest[[name]][[i]]$rate <- plan[[name]][[i]]$rate
      interest[[name]][[i]]$interest.only <- plan[[name]][[i]]$interest.only
      
    }
    interest[[name]] <- do.call(rbind, interest[[name]])
    
  }
  
  # Merge together 
  interest <- do.call(rbind, interest)
  row.names(interest) <- NULL
  
  # Find total  
  data.names <- names(interest.pay())[-1]
  total <- ddply(interest, "month", function(x){apply(x[, data.names], 2, sum, na.rm = TRUE)})
  total$mortgage <- "Total"  
  miss.names <- names(interest)[!names(interest) %in% names(total)]
  total[, miss.names] <- NA
  interest <- rbind(interest, total)   
  
  interest$mortgage <- factor(interest$mortgage, levels = c("Total", names(plan)))
  #   interest$mortgage <- factor(interest$mortgage, levels = names(plan))
  
  return(interest)
}

#====================================================================
#' Summary table of mortgages
#'
#' Summary table of mortgages
#'
#' @param   pay    an object from \code{\link{plan.pay}} 
#' @param   timeHorizon optional maximum time in years for the summary calculations
#' @param xtable logical. If TRUE a xtable is printed as html
#' @return  A summary table
#' @export
#' @examples
#' plan <- list(
#'  "Fix1" = list(
#'    list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
#'    list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
#'  ),
#'  "Amortization" = list(
#'    list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
#'  )
#' )
#' 
#' pay <- plan.pay(plan)
#' summaryPay(pay)
summaryPay <- function(
  pay, 
 timeHorizon = NULL,
  xtable = FALSE
){

  require(plyr)
  require(xtable)
  
  out <-  ddply(pay, c("mortgage", "sub.mortgage"), function(x){
    # x <- subset(pay, subset = mortgage == "Amortization" & sub.mortgage == 2)
    # x <- subset(pay, subset = mortgage == unique(pay$mortgage)[2] & sub.mortgage == 1)
  
    if(!is.null(timeHorizon)) x <- subset(x, subset = month <= 12*timeHorizon)    
    
    ln <- dim(x)[1]
    if(ln == 0) return(NULL)
    
    if(is.na(x$interest.only[1])){repayment.type <- ""} else {repayment.type <- switch(x$interest.only[1] + 1, "Amortization", "Interest")}
     
    data.frame(
      year.start = (x$month[1] - 1)/12,
      year.end = (x$month[ln])/12,
      debt.start = round(x$starting.debt[1]),
      average.rate = round(mean(x$rate), 2),
      repayment.type = repayment.type,
      interest = round(sum(x$interest)),
      amortizaton = round(sum(x$amortization)),
      total = round(sum(x$payment))  
      )    
  })
  
  #out[is.na(out)] <- ""
  out$mortgage <- as.character(out$mortgage)
  out$mortgage[out$sub.mortgage > 1] <- ""
  
  # Put total at end
  out <- out[c(2:dim(out)[1], 1), ]
  
  names(out) <- gsub("\\.", " ", names(out))
  names(out) <- paste(toupper(substring(names(out), 1, 1)), substring(names(out), 2), sep = "")
  
  # Render ax xtable is required
  if(xtable){
    print(xtable(out, digits = c(0, 0, 0, 1, 1, 0, 2, 0, 0, 0, 0)),
          type = "html",  
    include.rownames = FALSE, 
    format.args=list(big.mark = " ", decimal.mark = ".")
    )
  } else {    
return(out) 
}
}

#====================================================================
#' Ribbon plot for payments
#'
#' Ribbon ploy of payments 
#'
#' @param   pay    an object from \code{\link{plan.pay}} 
#' @param   y      y value to plot, either "payment", "interest", "amortization"
#' @param   xmax   optional x-axis upper limit
#' @param   ymax   optional y-axis upper limit
#' @return  A ggplot2 ribbon plot
#' @export
#' @examples
#' plan <- list(
#'  "Fix1" = list(
#'    list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
#'    list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
#'  ),
#'  "Amortization" = list(
#'    list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
#'  )
#' )
#' 
#' plan <- plan.pay(plan)
#' ribbon.plot.pay(plan)
ribbon.plot.pay <- function(
  pay, 
  y = "payment",
  xmax = NULL,
  ymax = NULL
){
  require(ggplot2)
  require(plyr)
  
  # Check y value
  if(is.na(match(y, c("payment", "interest", "amortization")))) stop('y must be one of "payment", "interest", or "amortization"')
  # Add in extra row to make steps in plot
  pay2 <- ddply(pay, "month", function(x){
    x0 <- x
    x0$month <- x0$month - (1 - 1E-6)
    rbind(x0, x) 
  }) 
  
  pay2$Year <- pay2$month/12
  
  plotdata <- subset(pay2, subset = mortgage != "Total")
  plotdata$mortgage <- factor(plotdata$mortgage, levels=levels(pay2$mortgage)[-match("Total", levels(pay2$mortgage))]) 
  
  p <- ggplot(plotdata,
              aes_string(x = "Year", y = y, group = "mortgage", fill = "mortgage", order = "-as.numeric(mortgage)")) +
    geom_ribbon(aes_string(ymin = "0", ymax = y), position = "stack") +
    ylab(paste("Monthly", y)) +
    scale_fill_discrete(name = "Mortgage")
  
  if(!is.null(xmax) & is.null(ymax)){
    p <- p + coord_cartesian(xlim = c(0, xmax*1.0))
  }
  
  if(is.null(xmax) & !is.null(ymax)){
    p <- p + coord_cartesian(ylim = c(0, ymax*1.05))
  }
  
  if(!is.null(xmax) & !is.null(ymax)){
    p <- p + coord_cartesian(xlim = c(0, xmax*1.0), ylim = c(0, ymax*1.05))
  }
  
  return(p)
  
}

#====================================================================
#' Line plot for payments
#'
#' Line ploy of payments 
#'
#' @param   pay    an object from \code{\link{plan.pay}} 
#' @param   y      y value to plot, either "payment", "interest", "amortization"
#' @param   xmax   optional x-axis upper limit
#' @param   ymax   optional y-axis upper limit
#' @return  A ggplot2 line plot
#' @export
#' @examples
#' plan <- list(
#'  "Fix1" = list(
#'    list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
#'    list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
#'  ),
#'  "Amortization" = list(
#'    list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
#'  )
#' )
#' 
#' plan <- plan.pay(plan)
#' line.plot.pay(plan)
line.plot.pay <- function(
  pay, 
  y = "payment",
  xmax = NULL,
  ymax = NULL
){
  require(ggplot2)
  require(plyr)
  
  # Check y value
  if(is.na(match(y, c("payment", "interest", "amortization")))) stop('y must be one of "payment", "interest", or "amortization"')
 
  pay$Year <- pay$month/12
  plotdata <- pay
  
  p <- ggplot(plotdata,  aes_string(x = "Year", y = y, group = "mortgage", colour= "mortgage")) +
    geom_step(direction = "vh", size = 1) +
    geom_step(data = subset(pay, subset = mortgage == "Total"), direction = "vh", size = 2) +
    ylab(paste("Monthly", y)) +
    scale_colour_discrete(name = "Mortgage") 
  
  if(!is.null(xmax) & is.null(ymax)){
    p <- p + coord_cartesian(xlim = c(0, xmax*1.0))
  }
  
  if(is.null(xmax) & !is.null(ymax)){
    p <- p + coord_cartesian(ylim = c(0, ymax*1.05))
  }
  
  if(!is.null(xmax) & !is.null(ymax)){
    p <- p + coord_cartesian(xlim = c(0, xmax*1.0), ylim = c(0, ymax*1.05))
  }
  
  return(p)
  
}
