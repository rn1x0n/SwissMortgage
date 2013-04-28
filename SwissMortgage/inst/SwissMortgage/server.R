rm(list = ls())
if(file.exists("/home/m1x0n/Projects/SwissMortgage/SwissMortgage/R/interest.R")){
  source("/home/m1x0n/Projects/SwissMortgage/SwissMortgage/R/interest.R")
}
#library(SwissMortgage)
library(shiny)
library(ggplot2)
library(plyr)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output){
  
  # Find the baseline rates over time
  flexRate <- reactive({
    flex.rate(current.rate = input$current.rate, 
              final.rate = input$final.rate, 
              period = input$flex.period, 
              extend.period = 50) # As may still need the interest rates calulated for longer
  })
  
  # # Find the current fix rates
  currentFixRates <- reactive({c(
    input$fix1year, input$fix2year, input$fix3year, input$fix4year, input$fix5year,
    input$fix6year, input$fix7year, input$fix8year, input$fix9year, input$fix10year  
  )})
  
  
  # shinyPlan
  shinyPlan <- function(){
    out <- list()
    out[[input$name1]] = list(debt = input$debt1, fix.rate = input$fixRate1, period = input$period1, interest.only = input$interestOnly1, amortization.period = input$amortizationPeriod1, renew = input$renew1)
    if(input$numMortgages >= 2){
      out[[input$name2]] = list(debt = input$debt2, fix.rate = input$fixRate2, period = input$period2, interest.only = TRUE, renew = input$renew2)
    }
    if(input$numMortgages >= 3){
      out[[input$name3]] = list(debt = input$debt3, fix.rate = input$fixRate3, period = input$period3, interest.only = TRUE, amortization.period = NULL, renew = input$renew3)
    }
    if(input$numMortgages >= 4){
      out[[input$name4]] = list(debt = input$debt4, fix.rate = input$fixRate4, period = input$period4, interest.only = TRUE, amortization.period = NULL, renew = input$renew4)
    }
    
    return(out)
  }
  
  # Build the plan
  plan <- reactive({shinyPlan2plan(
    shinyPlan = shinyPlan(),
    currentFixRates = currentFixRates(),
    flexRate = flexRate(),
    timeHorizon = input$timeHorizon
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
  
  # Output table
  output$summary <- renderTable({
    summary <- ddply(payments(), "mortgage", function(x){
      x <- subset(x, subset = month <= 12*input$timeHorizon)
      data.frame(total = round(sum(x$payment)/1000))
    })
    names(summary) <- c("Mortgage", "Total Payments (1000s)")
    summary
  })
  
  output$debug <- renderText({ymax()})
  
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







