# TEST CODE

rm(list = ls())
library(ggplot2)
library(shiny)
source("SwissMortgage/R/interest.R")

runApp("SwissMortgage/inst/SwissMortgage")

#library(SwissMortgage)
#R -e "shiny::runApp('/Users/Richard/Documents/R/Mortgage/SwissMortgage/inst/SwissMortgage')"

#####################################################################
# Copy server and ui to ShinyApps
system("cp /home/m1x0n/Projects/SwissMortgage/SwissMortgage/inst/SwissMortgage/*.* /home/m1x0n/ShinyApps/SwissMortgage/", intern = TRUE)

#####################################################################
# Roxygen2 and build

library(roxygen2)
roxygenize("SwissMortgage")
system("R CMD check SwissMortgage", intern = TRUE)
system("R CMD build SwissMortgage", intern = TRUE)
install.packages("~/Documents/R/Mortgage/SwissMortgage_0.1.tar.gz", repos = NULL, type = "source")

# Function to include in library to start a web server
# shiny::runApp(system.file('SwissMortgage', package='SwissMortgage'))

# Putting on web
# http://glimmer.rstudio.com:8787/
# Upload package to ~Packages
# install.packages("~/Packages/SwissMortgage_0.1.tar.gz", repos = NULL, type = "source")
# Upload  UI server files to SwissMortgage

# External link
# http://glimmer.rstudio.com/m1x0n/SwissMortgage
#####################################################################
# Interest payment functions

debt <- 100000; rate <- 1; period <- 3

flexRate <- flex.rate()
currentFixRates <- fix.rate(period = 1:10)
fix.rate(start.time = 0, period = 5, current.fix.rates = currentFixRates, flex.rate = flexRate)

amortization(debt = debt, rate = rate, period = 10)
interest.pay(debt = debt, rate = rate, period = period, interest.only = TRUE)
pay <- interest.pay(debt = debt, rate = rate, period = period, interest.only = FALSE, amortization.period = 15)
sum(pay$repayment)

shinyPlan <- list(
"Fix1" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 5), 
"Fix2" = list(debt = 200000, fix.rate = TRUE, period = 3, interest.only = TRUE, amortization.period = NULL, renew = 0),
"Amm1" = list(debt = 200000, fix.rate = FALSE, period = 10, interest.only = FALSE, amortization.period = 20, renew = 0) 
)

plan0 <- shinyPlan2plan(
  shinyPlan = shinyPlan,
  currentFixRates = fix.rate(period = 1:10),
  flexRate = flex.rate(),
  timeHorizon = 10
  )

 plan1 <- list(
  "Fix1" = list(
    list(debt = 1000, rate = 1, period = 5, interest.only = TRUE, amortization.period = NULL),
    list(debt = 1000, rate = 2, period = 3, interest.only = TRUE, amortization.period = NULL)
  ),
  "Amortization" = list(
    list(debt = 1000, rate = 2, period = 8, interest.only = FALSE,  amortization.period = 20)
  )
 )

payments <- payments0 <- plan.pay(plan0)
payments1 <- plan.pay(plan1)

summary <- ddply(payments, "mortgage", function(x){
  x <- subset(x, subset = month <= 12*10)
  data.frame(total = sum(x$payment))})
names(summary) <- c("Mortgage", "Total Payments")

ribbon.plot.pay(payments)
line.plot.pay(payments)

# Add in total
# total <- ddply(payments, "month", function(x){apply(x[, names(interest.pay())[-1]], 2, sum, na.rm = TRUE)})
# total$mortgage <- "Total" 
# pay_and_total <- rbind(payments, total)
# levels(pay_and_total$mortgage) <- c("Total", levels(payments$mortgage))

size <- 20
ylim2 <- max(payments$payment)
ggplot(payments, aes(x = month, y = payment, group = mortgage, colour = mortgage)) +
  geom_step(direction = "hv", size = 2) +
  coord_cartesian(xlim = c(1, 120), ylim = c(0, ylim2))
  theme(axis.title = element_text(size = size)) +
  theme(axis.text = element_text(size = size*0.8)) +
  theme(legend.title = element_text(size = size*0.8)) +
  theme(legend.text = element_text(size = size*0.8)) 

plotdata <- subset(payments, subset = mortgage != "Total")
plotdata$mortgage <- factor(plotdata$mortgage, levels=rev(levels(payments$mortgage)[-match("Total", levels(payments$mortgage))])) 
ggplot(plotdata,
       aes(x = month, y = payment, group = mortgage, fill = mortgage, order = -as.numeric(mortgage))) +
  geom_ribbon(direction = "vh", aes(ymin = 0, ymax = payment), position = "stack") +
  coord_cartesian(xlim = c(1, 120), ylim = c(0, ylim2))
  
currentRatesData <- data.frame(year = 1:length(currentFixRates), rate = currentFixRates)
ggplot(currentRatesData, aes(x = year, y = rate)) +
  geom_line(size = 2) +
  xlab("Fixed rate period") + ylab("Interest rate (%)") +
theme(axis.title = element_text(size = size)) +
  theme(axis.text = element_text(size = size*0.8)) +
  theme(legend.title = element_text(size = size*0.8)) +
  theme(legend.text = element_text(size = size*0.8)) 

flexRate$Year <- flexRate$month/12
ggplot(subset(flexRate, subset = Year <= 10), aes(x = Year, y = rate)) +
  geom_line(size = 2) +
  ylab("Interest rate (%)") +
  theme(axis.title = element_text(size = size)) +
  theme(axis.text = element_text(size = size*0.8)) +
  theme(legend.title = element_text(size = size*0.8)) +
  theme(legend.text = element_text(size = size*0.8)) 


