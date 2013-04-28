library(shiny)
library(SwissMortgage)
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
  
  interestPlot <-   reactive({
    
    if(input$graphType == "Line graph"){
      
      plotdata <- payments()
      
      ylim2 <- ifelse(input$ylim2Fix, input$ylim2, max(plotdata$payment))
        
        p <- ggplot(plotdata, aes(x = Year, y = payment, group = mortgage, colour = mortgage)) +
        geom_step(direction = "vh", size = 1) +
        geom_step(data = subset(payments(), subset = mortgage == "Total"), direction = "vh", size = 2) +
        ylab("Monthly payment") +
        scale_colour_discrete(name = "Mortgage") +
        coord_cartesian(xlim = c(0, input$timeHorizon*1.05), ylim = c(0, ylim2*1.05))  
      print(p)  
    }
    
    if(input$graphType == "Ribbon graph"){
      plotdata <- subset(payments(), subset = mortgage != "Total")
      plotdata$mortgage <- factor(plotdata$mortgage, levels=levels(payments()$mortgage)[-match("Total", levels(payments()$mortgage))]) 
      
      p <- ggplot(plotdata,
                  aes(x = Year, y = payment, group = mortgage, fill = mortgage, order = -as.numeric(mortgage))) +
        geom_ribbon(aes(ymin = 0, ymax = payment), position = "stack") +
        ylab("Monthly payment") +
        scale_fill_discrete(name = "Mortgage") +
        coord_cartesian(xlim = c(0, input$timeHorizon*1.05))
      
         if(input$ylim2Fix){
            p <- p + coord_cartesian(xlim = c(0, input$timeHorizon*1.05), ylim = c(0, input$ylim2*1.05))
         } 
        
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
  
  output$debug <- renderText({input$ylim2Fix})
  
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







