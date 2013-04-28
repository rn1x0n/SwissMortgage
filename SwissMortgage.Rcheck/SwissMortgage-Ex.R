pkgname <- "SwissMortgage"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SwissMortgage')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("amortization")
### * amortization

flush(stderr()); flush(stdout())

### Name: amortization
### Title: Find the value of one amortization payment
### Aliases: amortization

### ** Examples

amortization(debt = 1000, rate = 1, period = 5)



cleanEx()
nameEx("fix.rate")
### * fix.rate

flush(stderr()); flush(stdout())

### Name: fix.rate
### Title: Find the fixed interest rates over given future period
### Aliases: fix.rate

### ** Examples

flexRate <- flex.rate()
current.fix.rates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
fix.rate(start.time = 0, period = 5, current.fix.rates = current.fix.rates, flex.rate = flexRate)



cleanEx()
nameEx("flex.rate")
### * flex.rate

flush(stderr()); flush(stdout())

### Name: flex.rate
### Title: Find the flexible interest rates over given future period
### Aliases: flex.rate

### ** Examples

flex.rate(current.rate = 1, final.rate = 5, period = 10)



cleanEx()
nameEx("interest.pay")
### * interest.pay

flush(stderr()); flush(stdout())

### Name: interest.pay
### Title: Find the payments over time for a mortgage
### Aliases: interest.pay

### ** Examples

interest.pay(debt = 1000, rate = 1, period = 5, interest.only = FALSE, amortization.period = 20)



cleanEx()
nameEx("plan.pay")
### * plan.pay

flush(stderr()); flush(stdout())

### Name: plan.pay
### Title: Find the payments over time for a set of mortgages
### Aliases: plan.pay

### ** Examples

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



cleanEx()
nameEx("ribbon.plot.pay")
### * ribbon.plot.pay

flush(stderr()); flush(stdout())

### Name: ribbon.plot.pay
### Title: Ribbon plot for payments
### Aliases: ribbon.plot.pay

### ** Examples

plan <- list(
 "Fix1" = list(
   list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
   list(debt = 2000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
 ),
 "Amortization" = list(
   list(debt = 1000, rate = 2, period = 8, interest.only = FALSE, amortization.period = 20)
 )
)

plan <- plan.pay(plan)
ribbon.plot.pay(plan)



cleanEx()
nameEx("shinyPlan2plan")
### * shinyPlan2plan

flush(stderr()); flush(stdout())

### Name: shinyPlan2plan
### Title: Convert a shinyPlan list to a plan list
### Aliases: shinyPlan2plan

### ** Examples

shinyPlan <- list(
   "Fix1" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 5),
   "Fix2" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 0),
   "Amm1" = list(debt = 200000, fix.rate = FALSE, period = 10, interest.only = FALSE, amortization.period = 20, renew = 0)
)
currentFixRates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
flexRate <- flex.rate()
shinyPlan2plan(shinyPlan = shinyPlan, currentFixRates = currentFixRates,  flexRate = flexRate, timeHorizon = 10)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
