library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Swiss Mortgage Calculator"),
  
  sidebarPanel(
    
    selectInput("numMortgages", "Number of mortgages:", choices = c(1,2,3,4), selected = 2),
    sliderInput("timeHorizon", "Years to predict mortgage for:", min = 1, max = 10, value = 10),
    
    h3("Mortgage Details"), 
    
    # First mortgage  
    h4("Mortgage 1"),  
    textInput("name1", "Name of mortgage:", "Amortization"),   
    checkboxInput("interestOnly1", "Interest only", FALSE),
    checkboxInput("fixRate1", "Fixed rate", TRUE),
    sliderInput("debt1", "Amount for mortgage:", min = 0, max = 1000000, value = 100000, step = 25000),
    sliderInput("period1", "Period (years) for mortgage:", min = 0, max = 10, value = 10, step = 1),
    conditionalPanel(
      condition = "input.interestOnly1 == false",
      sliderInput("amortizationPeriod1", "Amortization period (years) for mortgage:", min = 10, max = 25, value = 20, step = 1)  
    ),
    sliderInput("renew1", "Period for renewed mortgage (0 = not renew):", min = 0, max = 10, value = 0, step = 1),
    
    # Second mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 2",
      br(),
      h4("Mortgage 2"),
      textInput("name2", "Name of mortgage:", "Fix 1"),
      checkboxInput("fixRate2", "Fixed rate", TRUE),
      sliderInput("debt2", "Amount for mortgage:", min = 0, max = 1000000, value = 100000, step = 25000),
      sliderInput("period2", "Period (years) for mortgage:", min = 0, max = 10, value = 5, step = 1),
      sliderInput("renew2", "Period for renewed mortgage (0 = not renew):", min = 0, max = 10, value = 5, step = 1)
    ),
    
    # Third mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 3",
      br(),
      h4("Mortgage 3"),
      textInput("name3", "Name of mortgage:", "Fix 2"),
      checkboxInput("fixRate3", "Fixed rate", TRUE),
      sliderInput("debt3", "Amount for mortgage:", min = 0, max = 1000000, value = 100000, step = 25000),
      sliderInput("period3", "Period (years) for mortgage:", min = 0, max = 10, value = 5, step = 1),
      sliderInput("renew3", "Period for renewed mortgage (0 = not renew):", min = 0, max = 10, value = 5, step = 1)
    ),
      
    # Fourth mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 4",
      br(),
      h4("Mortgage 4"),
      textInput("name4", "Name of mortgage:", "Fix 3"),
      checkboxInput("fixRate4", "Fixed rate", TRUE),
      sliderInput("debt4", "Amount for mortgage:", min = 0, max = 1000000, value = 100000, step = 25000),
      sliderInput("period4", "Period (years) for mortgage:", min = 0, max = 10, value = 5, step = 1),
      sliderInput("renew4", "Period for renewed mortgage (0 = not renew):", min = 0, max = 10, value = 5, step = 1)
    ),
    
    # Graph options
    br(),
    h3("Graph options"),
    selectInput("graphType", "Type of graph:", choices = c("Ribbon graph", "Line graph"), selected = "Ribbon graph"),
    
    selectInput("yaxis", "What to plot on the y-axis:", choices = c("payment", "interest", "amortization"), selected = "payment"),
    helpText('"payments" plot the total monthy payment, this is the sum of the "interest" and the 
             "amortization" repayment.'),
       
    checkboxInput("ylim2Fix", "Fix upper limit on y-axis:", FALSE),
    conditionalPanel(
      condition = "input.ylim2Fix == true",
    sliderInput("ylim2", "Upper limit for y-axis:", min = 0, max = 10000, value = 2000, step = 100)
    ),
    
    
    # Forcast for baseline interest rates  
    br(),
    h3("Interest rate assumptions"),
    h4("Forcast for baseline interest rates"),
    helpText("Input the current baseline rate, and what you think the rate wil be in the future.
              This is the rate used for non-fixed rate mortgages. The future fixed rates are changed
              overtime by the same absolute amount as the baseline rate."),
    
    numericInput("current.rate", "Current baseline rate (%)", 0.98, min = 0, step = 0.01),
    numericInput("final.rate", "Future baseline rate (%)", 5, min = 0, step = 0.01),
    numericInput("flex.period", "Years to future rate", 10, min = 1, max = 10, step = 1),
    
    # Fixed mortgage interest rates
    
    h4("Fixed mortgage interest rates"),
    
    numericInput("fix1year", "1 year", 0.98, step = 0.01),
    numericInput("fix2year", "2 year", 0.96, step = 0.01),
    numericInput("fix3year", "3 year", 1.02, step = 0.01),
    numericInput("fix4year", "4 year", 1.15, step = 0.01),
    numericInput("fix5year", "5 year", 1.3, step = 0.01),
    numericInput("fix6year", "6 year", 1.46, step = 0.01),
    numericInput("fix7year", "7 year", 1.62, step = 0.01),
    numericInput("fix8year", "8 year", 1.78, step = 0.01),
    numericInput("fix9year", "9 year", 1.92, step = 0.01),
    numericInput("fix10year", "10 year", 2.06, step = 0.01)  
    
  ),
  
  #####################################################################  
  # OUTPUTS 
  mainPanel(
    tabsetPanel(
      tabPanel("Results", 
               plotOutput("interestPlot"), 
               tableOutput("summary"),
               #textOutput("debug"),
               #htmlOutput("summaryXtable"),
               htmlOutput("googleAnalytics")
               ), 
      tabPanel("Interest rate assumptions",
               h4("Fixed interest rates for each period"),
               plotOutput("currentRatePlot"), 
               
               h4("Flexible baseline interest rates over time"),
               plotOutput("flexRatePlot") ,
               
               h4("Fixed interest rates by period over time"),
               p("Put the two plots above together, and find the fixed interest rates that we assume will be offered in the future."),
               plotOutput("fixRatesByStartTimePlot")
      )
    )  
  )
  
))



# current.fix.rates <- c("1" = 0.980,
#                        "2" = 0.960,
#                        "3" = 1.020,
#                        "4" = 1.150,
#                        "5" = 1.300,
#                        "6" = 1.460,
#                        "7" = 1.620,
#                        "8" = 1.780,
#                        "9" = 1.920,
#                        "10" = 2.060)

#    for(i in 1:length(current.fix.rates)){ 
#     cat(paste('numericInput("fix.', i, 'year", "', i, ' year", ', current.fix.rates[i], ', step = 0.01),\n',sep = ""))
#    }

# current.fix.rate.parse <- c()
#    for(i in 1:length(current.fix.rates)){ 
#      current.fix.rate.parse[i] <- parse(text = paste(
#      'numericInput("fix.', i, 'year", "', i, ' year", ', current.fix.rates[i], ')',
#                         sep = ""
# ))
# }

