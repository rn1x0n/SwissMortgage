rm(list = ls())
if(file.exists("/home/m1x0n/Projects/SwissMortgage/pkg/R/interest.R")){
  source("/home/m1x0n/Projects/SwissMortgage/pkg/R/interest.R")
}
#library(SwissMortgage)
library(shiny)
library(ggplot2)
library(plyr)
library(xtable)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output){
  
  # Find the baseline rates over time
  LIBOR.times <- function(){
    out <- c(0, input$LIBOR.time1)
    if(input$moreLIBOR){
      out <- c(out, input$LIBOR.time2)
    }
    return(out)
  }
  
  LIBOR.rates <- function(){
    out <- c(input$LIBOR.rate0, input$LIBOR.rate1)
    if(input$moreLIBOR){
      out <- c(out, input$LIBOR.rate2)
    }
    return(out)
  }
  
  flexRate <- reactive({
    flex.rate(times = LIBOR.times(),
     rates = LIBOR.rates(),
     last.time = 20) # As may still need the interest rates calulated for longer
  })
  
  # # Find the current fix rates
  currentFixRates <- reactive({c(
    input$fix1year, input$fix2year, input$fix3year, input$fix4year, input$fix5year,
    input$fix6year, input$fix7year, input$fix8year, input$fix9year, input$fix10year  
  )})
  
  
  # shinyPlan
  shinyPlan <- function(){
    out <- list()
    out[[input$name1]] <- list()
    out[[input$name1]][[1]] <- list(debt = input$debt1a, fix.rate = input$fixRate1a, period = input$period1a, interest.only = input$interestOnly1a, amortization.period = input$amortizationPeriod1a)
    if(input$renew1b){
      if(input$changeDebt1b){debt1b <- input$debt1b} else { debt1b <- NULL} # can only change debt for interest only
      out[[input$name1]][[2]] <- list(debt = debt1b, fix.rate = input$fixRate1b, period = input$period1b, interest.only = input$interestOnly1a) # amortization.period is calculated    
    }
    
    if(input$numMortgages >= 2){
      out[[input$name2]] <- list()
      out[[input$name2]][[1]] <- list(debt = input$debt2a, fix.rate = input$fixRate2a, period = input$period2a, interest.only = TRUE)     
      if(input$renew2b){
        if(input$changeDebt2b){debt2b <- input$debt2b} else { debt2b <- NULL}
        out[[input$name2]][[2]] <- list(debt = debt2b, fix.rate = input$fixRate2b, period = input$period2b, interest.only = TRUE)
      }    
    }
    
    if(input$numMortgages >= 3){
      out[[input$name3]] <- list()
      out[[input$name3]][[1]] <- list(debt = input$debt3a, fix.rate = input$fixRate3a, period = input$period3a, interest.only = TRUE)     
      if(input$renew3b){
        if(input$changeDebt3b){debt3b <- input$debt3b} else { debt3b <- NULL}
        out[[input$name3]][[2]] <- list(debt = debt3b, fix.rate = input$fixRate3b, period = input$period3b, interest.only = TRUE)
      }    
    }
 
     if(input$numMortgages >= 4){
      out[[input$name4]] <- list()
      out[[input$name4]][[1]] <- list(debt = input$debt4a, fix.rate = input$fixRate4a, period = input$period4a, interest.only = TRUE)     
      if(input$renew4b){
        if(input$changeDebt4b){debt4b <- input$debt4b} else { debt4b <- NULL}
        out[[input$name4]][[2]] <- list(debt = debt4b, fix.rate = input$fixRate4b, period = input$period4b, interest.only = TRUE)
      }    
    }
    
    return(out)
  }
  
  # Value to feed back to default input values
  output$debt2a <- reactive(input$debt2a)
  
  # Build the plan
  plan <- reactive({shinyPlan2plan(
    shinyPlan = shinyPlan(),
    currentFixRates = currentFixRates(),
    flexRate = flexRate()
    #timeHorizon = input$timeHorizon
  )})
  
  # Find payments
  payments <- reactive({
    payments <- plan.pay(plan())
    payments$Year <- payments$month/12
    payments
  })
  
  # Plot of payments
  size <- 15 
  
  ymax <- function(){
    if(input$ylim2Fix){
      return(input$ylim2)
    } else {
      return(NULL)
    }
  }
  
  
  interestPlot <-   reactive({
    
    if(input$graphType == "Line graph"){    
      p <-  line.plot.pay(
        payments(), 
        y = input$yaxis,
        xmax = input$timeHorizon,
        ymax = ymax()
      )
      print(p)  
    }
    
    if(input$graphType == "Ribbon graph"){    
      p <-  ribbon.plot.pay(
        payments(), 
        y = input$yaxis,
        xmax = input$timeHorizon,
        ymax = ymax()
      )    
      print(p) 
    }
  })
  
  output$interestPlot <- renderPlot({interestPlot()})
  
  # Plot of current fix rate interest rates
  output$currentRatePlot <- renderPlot({
    plotdata <- data.frame(year = 1:length(currentFixRates()), rate = currentFixRates())
    p <- ggplot(plotdata, aes(x = year, y = rate)) +
      geom_line(size = 1.5) +
      geom_point(size = 1.5) +
      xlab("Fixed rate period (Year)") + ylab("Interest rate (%)") +
      theme(axis.title = element_text(size = size)) +
      theme(axis.text = element_text(size = size*0.8)) +
      theme(legend.title = element_text(size = size*0.8)) +
      theme(legend.text = element_text(size = size*0.8)) 
    print(p)
  })
  
  
  # Plot of flexible rates interest rates over time  
  output$flexRatePlot <- renderPlot({
    plotdata <-  flexRate()
    plotdata$Year <- plotdata$month/12
    p <- ggplot(subset(plotdata, subset = Year <= input$timeHorizon), aes(x = Year, y = rate)) +
      geom_line(size = 1.5) +
      ylab("Interest rate (%)") +
      theme(axis.title = element_text(size = size)) +
      theme(axis.text = element_text(size = size*0.8)) +
      theme(legend.title = element_text(size = size*0.8)) +
      theme(legend.text = element_text(size = size*0.8)) 
    print(p)
  })
  
  # Plot of Fixed interest rates by period over time  
  output$fixRatesByStartTimePlot <- renderPlot({
  fixRatesByStartTime <- lapply(0:9, function(x){
    data.frame(
      year = x,
      fix.rate.period = 1:10,
      rate = fix.rate(start.time = x, period = 1:10, current.fix.rates = currentFixRates(), flex.rate = flexRate())
    )})
  fixRatesByStartTime <- do.call("rbind", fixRatesByStartTime)
  fixRatesByStartTime$fix.rate.period <- as.factor(fixRatesByStartTime$fix.rate.period)
  levels(fixRatesByStartTime$fix.rate.period) <- rev(levels(fixRatesByStartTime$fix.rate.period))
  
  p <- ggplot(fixRatesByStartTime, aes(x = year, y = rate, group = fix.rate.period, colour = fix.rate.period)) +
    geom_line(size = 1) +
    xlab("Year") + ylab("Interest rate (%)") +
    scale_colour_discrete(name = "Fixed rate period (year)")
  print(p)
  })
  
  # Output table
  output$summaryTitle <- renderText(paste("Summary details over", input$timeHorizon, "years"))
  
  output$summary <- renderTable(summaryPay(payments(), timeHorizon = input$timeHorizon),
    
#   summary <- ddply(payments(), "mortgage", function(x){
#     x <- subset(x, subset = month <= 12*input$timeHorizon)
#     data.frame(total = round(sum(x[,input$yaxis])))
#   })
#   names(summary) <- c("Mortgage", paste("Total", input$yaxis, "over", input$timeHorizon, "years"))
#   summary 
digits = c(0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0),
include.rownames = FALSE, 
format.args=list(big.mark = " ", decimal.mark = ".")
  )
  
  output$debug <- renderText({shinyPlan()})
  
  # Goolge analytics
  output$googleAnalytics <- renderText({
    "<script>
       (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
         (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                                m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
       })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
     
     ga('create', 'UA-1659508-5', 'rstudio.com');
     ga('send', 'pageview'); 
     </script>"
  })
})







