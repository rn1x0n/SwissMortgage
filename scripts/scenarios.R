#' # Mortgage Calculations

#+ echo = FALSE
source(file.path("..", "_setup.R"))
opts_chunk$set(warning = FALSE, error = FALSE)

timeHorizon <- 10
ymax <- 2100

save.per.year <- 100000
react.time <- 0.25 # Time in years to takes to realize the rates are rising. 

# Fix rates
currentFixRates <- c(0.98, 0.96, 1.02, 1.15, 1.30, 1.46, 1.62, 1.78, 1.92, 2.06)

# Variable rates
current.rate <- 1   # Current interest rate
period.current <- 1 # Period the current rate will stay as it is
final.rate <- 5     # Final interest rate
period <- 10    # Period in years over which the rate will change 
extend.period <- 50 # After the period has ended keep at the final rate for this long

#' ## Total costs

#+ total_costs, echo = FALSE
total_costs <- list()
total_costs$Land <- 271000
total_costs$Haus <- 1024000
total_costs$Parkplatz <- 39000
total_costs$Velobox <- 5000
total_costs$Dusche <- 12700
total_costs$Total <-  sum(unlist(total_costs))

total_costs_mat <- as.matrix(total_costs)
dimnames(total_costs_mat)[[2]] <- "CHF"

#+ total_costs_print, echo = FALSE, results = "asis"
print(xtable(total_costs_mat, digits = 0),
      type = "html",
      include.rownames = TRUE, 
      format.args=list(big.mark = "'", decimal.mark = ",")
)
  
#' ## Mortage parts
#' * __Deposit__: Up to 20% of the total mortgage needed.
#' * __1st mortgage__: 66% of total. is typically an interest only mortage, that can be __fixed__ over a period of time, or linked to the __LIBOR__ rate.
#' * __Amortization mortgage__: The remainder. Must be amortized over the next 20 years. This can be fixed or LIBOR.
#' 
#' The amortization mortage can be paid by indirect amortization.
#' * Only the interest is paid directly
#' * The amortizaiton part is paid into a 3rd pillar, and is then used to pay off the mortgage. 
#' * This has tax advantages as the 3rd pillar payments are made pre-tax.
#' 
#' A building loan is needed for a new build house. 
#' * This usually has a different interest rate from the typical fixed or LIBOR rates for a 1st mortgage. 
#' * This may have an admin fee.
#' * It may be possible to take out a staggered 1st mortage during the building phase.
#' 
#' ### Mortgage is made up of

#+ mortgage_parts, echo = FALSE, results = "asis"
mortgage_parts <- list()
mortgage_parts$Deposit <- total_costs$Total * 0.2
mortgage_parts$First <- total_costs$Total * 0.66
mortgage_parts$Amortization <- total_costs$Total - mortgage_parts$Deposit - mortgage_parts$First

mortgage_parts_mat <- as.matrix(mortgage_parts)
dimnames(mortgage_parts_mat)[[2]] <- "CHF"

print(xtable(mortgage_parts_mat, digits = 0),
      type = "html",
      include.rownames = TRUE, 
      format.args=list(big.mark = "'", decimal.mark = ",")
)

#' ## Assumptions about interest rates
#' ### Plot of current fix rate interest rates

#+ echo = FALSE, fig.width = 7, fig.height = 4
#currentFixRates <- fix.rate(period = 1:10)
plotdata <- data.frame(year = 1:length(currentFixRates), rate = currentFixRates)

ggplot(plotdata, aes(x = year, y = rate)) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    xlab("Fixed rate period (Year)") + ylab("Interest rate (%)")

#' ### Plot of flexible rates interest rates over time  
#+ echo = FALSE, fig.width = 7, fig.height = 4
  plotdata <- flexRate <- flex.rate(current.rate = current.rate,
                        period.current = period.current,
              final.rate = final.rate, 
              period = period, 
              extend.period = extend.period) 

  plotdata$Year <- plotdata$month/12
  
ggplot(subset(plotdata, subset = Year <= period.current + period), aes(x = Year, y = rate)) +
    geom_line(size = 1.5) +
    ylab("Interest rate (%)")

#' ### Plot of fixed interest rates by period over time  
#+ echo = FALSE, fig.width = 9, fig.height = 4
  fixRatesByStartTime <- lapply(0:9, function(x){
    data.frame(
      year = x,
      fix.rate.period = 1:10,
      rate = fix.rate(start.time = x, period = 1:10, current.fix.rates = currentFixRates, flex.rate = flexRate)
    )})
  fixRatesByStartTime <- do.call("rbind", fixRatesByStartTime)
  fixRatesByStartTime$fix.rate.period <- as.factor(fixRatesByStartTime$fix.rate.period)
  levels(fixRatesByStartTime$fix.rate.period) <- rev(levels(fixRatesByStartTime$fix.rate.period))
  
  ggplot(fixRatesByStartTime, aes(x = year, y = rate, group = fix.rate.period, colour = fix.rate.period)) +
    geom_line(size = 1) +
    xlab("Year") + ylab("Interest rate (%)") +
    scale_colour_discrete(name = "Fixed rate period (year)")


#' ## Assumptions about savings
#' * Assume can save `r formatC(save.per.year, format = "d", big.mark = "'")` CHF per year.
#' 
#' ## General strategy
#' * Have several fixed rate mortgages. Structure them, so that when they come to the 
#' end of the period there should be enough cash to pay them off if wanted.
#' 
#+ echo = FALSE
desc <- list()
desc[["fix_3_times_3_years"]] <- "3 fixed rate mortages; of times 3, 6, and 9 years; for 300'000, 3000'000 and the remainder"

#' ### Option 1: `r desc[[1]]`
#+ echo = FALSE
shinyPlan <- plan <- pay <- list()

fix1 <- 3 * save.per.year; fix2 <- 3 * save.per.year
shinyPlan[[names(desc)[1]]] <- list(
  "Amortization: 10 year fix" = list(
    list(debt = mortgage_parts$Amortization, fix.rate = TRUE,  period = 10, interest.only = FALSE,  amortization.period = 20),
    list(debt = NULL, fix.rate = TRUE,  period = 10, interest.only = FALSE)
  ),
  "3 year fix" = list(
    list(debt = fix1, fix.rate = TRUE, period = 3, interest.only = TRUE)
  ),
  "6 year fix" = list(
    list(debt = fix2, fix.rate = TRUE, period = 6, interest.only = TRUE)
  ),
  "9 year fix" = list(
    list(debt = mortgage_parts$First - fix1 - fix2, fix.rate = TRUE, period = 9, interest.only = TRUE)
  )
)

#+ echo = FALSE
desc[["LIBOR_then_fix_3_times_3_years"]] <- paste("LIBOR mortgage. When rates rise switch to 3 fixed rate mortages; of times 3, 6, and 9 years; for 300'000, 3000'000 and the remainder. Assume these start", react.time*12, "months after the rate change. Use initial saved money to pay off 9 year fixed rate")
#' ### Option 2: `r desc[[2]]`
#+ echo = FALSE
lib1 <- 3 * save.per.year
fix1 <- lib1

lib2 <- 3 * save.per.year
fix2 <- lib2 

lib3 <- mortgage_parts$First - fix1 - fix2
fix3 <- lib3 - (period.current + react.time)*save.per.year # Take saving off last mortgage

# If have saved more than value of fix3
if(fix3 < 0){
  fix2 <- fix2 + fix3 # take off the extra from fix2
  fix3 <- 0  
}

shinyPlan[[names(desc)[2]]] <- list(
  "Amortization: LIBOR -> fix to up 10 years" = list(
    list(debt = mortgage_parts$Amortization, fix.rate = TRUE,  period = period.current + react.time, interest.only = FALSE,  amortization.period = 20),
    list(debt = NULL, fix.rate = TRUE,  period = ceiling(10 - period.current + react.time), interest.only = FALSE)
  ),
  "LIBOR -> 3 year fix" = list(
    list(debt = lib1, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),
    list(debt = fix1, fix.rate = TRUE, period = 3, interest.only = TRUE)
  ),
  "LIBOR -> 6 year fix" = list(
    list(debt = lib1, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),    
    list(debt = fix2, fix.rate = TRUE, period = 6, interest.only = TRUE)
  ),
  "LIBOR -> pay off with savings, 9 year fix" = list(
    list(debt = lib3, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),    
    list(debt = fix3, fix.rate = TRUE, period = 9, interest.only = TRUE)
  )
)

#+ echo = FALSE
desc[["LIBOR_fix_then_fix_3_times_3_years"]] <- paste("Fixed rate amortization mortgage, LIBOR on fixed mortgages. When rates rise switch to 3 fixed rate mortages; of times 3, 6, and 9 years; for 300'000, 3000'000 and the remainder. Assume these start", react.time*12, "months after the rate change. Use initial saved money to pay off 9 year fixed rate")
#' ### Option 3: `r desc[[3]]`
#+ echo = FALSE

shinyPlan[[names(desc)[3]]] <- list(
  "Amortization: 10 year fix" = list(
    list(debt = mortgage_parts$Amortization, fix.rate = TRUE,  period = 10, interest.only = FALSE,  amortization.period = 20)
  ),
  "LIBOR -> 3 year fix" = list(
    list(debt = lib1, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),
    list(debt = fix1, fix.rate = TRUE, period = 3, interest.only = TRUE)
  ),
  "LIBOR -> 6 year fix" = list(
    list(debt = lib1, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),    
    list(debt = fix2, fix.rate = TRUE, period = 6, interest.only = TRUE)
  ),
  "LIBOR -> 9 year fix" = list(
    list(debt = lib3, fix.rate = FALSE, period = period.current + react.time, interest.only = TRUE),    
    list(debt = fix3, fix.rate = TRUE, period = 9, interest.only = TRUE)
  )
)

# Find the payments
for(name in names(shinyPlan)){
  plan[[name]] <- shinyPlan2plan(
    shinyPlan = shinyPlan[[name]],
    currentFixRates = currentFixRates,
    flexRate = flexRate
  )
  
  pay[[name]] <- plan.pay(plan[[name]])
  
}

#' ## Summary of mortgages
#' ### Option 1: `r desc[[1]]`
#+ option1_fig, echo = FALSE, fig.width = 10, fig.height = 5
ribbon.plot.pay(pay[[1]], xmax = timeHorizon, ymax = ymax)
#+ option1_table, echo = FALSE, results = "asis"
summaryPay(pay[[1]], timeHorizon = timeHorizon, xtable = TRUE)

#' ### Option 2: `r desc[[2]]`
#+ option2_fig, echo = FALSE, fig.width = 10, fig.height = 5
ribbon.plot.pay(pay[[2]], xmax = timeHorizon, ymax = ymax)
#+ option2_table, echo = FALSE, results = "asis"
summaryPay(pay[[2]], timeHorizon = timeHorizon, xtable = TRUE)

#' ### Option 3: `r desc[[3]]`
#+ option3_fig, echo = FALSE, fig.width = 10, fig.height = 5
ribbon.plot.pay(pay[[3]], xmax = timeHorizon, ymax = ymax)
#+ option3_table, echo = FALSE, results = "asis"
summaryPay(pay[[3]], timeHorizon = timeHorizon, xtable = TRUE)

