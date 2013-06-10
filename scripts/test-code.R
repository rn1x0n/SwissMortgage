# TEST CODE

source("_setup.R")
runApp("pkg/inst/shiny")


#####################################################################
# Putting on web
# http://glimmer.rstudio.com:8787/
# Git pull the latest version to project SwissMortgage
# Copy server and ui to ShinyApps
system("cp /home/m1x0n/Projects/SwissMortgage/pkg/inst/shiny/*.* /home/m1x0n/ShinyApps/SwissMortgage/", intern = TRUE)

# External link
# http://glimmer.rstudio.com/m1x0n/SwissMortgage

# Building windows file
# http://win-builder.r-project.org/upload.aspx
#####################################################################
# Interest payment functions

debt <- 100000; rate <- 1; period <- 3

flexRate <- flex.rate(times = c(0, 3, 6, 10), rates = c(1, 2, 2.5, 2), last.time = 12)
ggplot(flexRate, aes(x = month, y = rate)) + geom_line()

currentFixRates <- current.fix.rates <- c(0.980, 0.960, 1.020, 1.150, 1.300, 1.460, 1.620, 1.780, 1.920, 2.060)
fix.rate(start.time = 0, period = 5, current.fix.rates = currentFixRates, flex.rate = flexRate)
fix.rate(start.time = 0, period = 1:10, current.fix.rates = currentFixRates, flex.rate = flexRate)
fix.rate(start.time = 15, period = 5, current.fix.rates = currentFixRates, flex.rate = flexRate)
fix.rate(start.time = 0, period = 15, current.fix.rates = currentFixRates, flex.rate = flexRate)

amortization(debt = debt, rate = rate, period = 10)
interest.pay(debt = debt, rate = rate, period = period, interest.only = TRUE)
interest.pay(debt = debt, rate = rate, period = period, interest.only = FALSE, amortization.period = 15)


shinyPlan <- list(
  "Amortization" = list(
    list(debt = 1000, fix.rate = TRUE,  period = 4, interest.only = FALSE,  amortization.period = 20),
    list(debt = NULL, fix.rate = TRUE,  period = 4, interest.only = FALSE)
  ),
  "Fix" = list(
    list(debt = 1000, fix.rate = FALSE, period = 4, interest.only = TRUE),
    list(debt = NULL, fix.rate = TRUE,  period = 3, interest.only = TRUE)
  )
)

plan <- shinyPlan2plan(
  shinyPlan = shinyPlan,
  currentFixRates = currentFixRates,
  flexRate = flexRate
  )


pay <- plan.pay(plan)

pay1 <- payments(shinyPlan = shinyPlan,
                   currentFixRates = currentFixRates,
                   flexRate = flexRate)

summaryPay(pay, timeHorizon = 6, xtable = TRUE)
ribbon.plot.pay(pay)
line.plot.pay(pay)

