library(shiny)

# Constants
max.mortgage <- 1000000
value.mortgage <- 100000
step.mortgage <- 25000

max.period <- 10
value.period <- 5
step.period <- 1
cellpadding <- "0"
cellspacing <- "6"

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Swiss Mortgage Calculator"),
  
  sidebarPanel(
    
    selectInput("numMortgages", "Number of mortgages:", choices = c(1,2,3,4), selected = 2),
    sliderInput("timeHorizon", "Years to predict mortgage for:", min = 1, max = 10, value = 10),
    
    h3("Mortgage Details"), 
    
    # First mortgage  
    h4("Mortgage 1"),  
    helpText("Only mortgage 1 can be amortized"),
    textInput("name1", "Name of mortgage:", "Amortization"),   
    
    tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing,
               tags$tr(
                 tags$td(checkboxInput("fixRate1a", "Fixed rate ", TRUE)),
                 tags$td(checkboxInput("interestOnly1a", "Interest only", FALSE))
               )
    ),
    
    sliderInput("debt1a", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage),
    sliderInput("period1a", "Period (years) for mortgage:", min = 0, max = max.period, value = 10, step = 1),
    conditionalPanel(
      condition = "input.interestOnly1a == false",
      sliderInput("amortizationPeriod1a", "Amortization period (years) for mortgage:", min = 10, max = 25, value = 20, step = 1)  
    ),
    
    # Renew?
    tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
      tags$td(checkboxInput("renew1b", "Renew mortgage ", FALSE)),   
    
      tags$td(conditionalPanel(
        condition = "input.renew1b == true",
        
        tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
          tags$td(checkboxInput("fixRate1b", "Fixed rate ", TRUE)),
          
          tags$td(conditionalPanel(
            condition = "input.interestOnly1a == true",
            checkboxInput("changeDebt1b", "Change debt", FALSE) 
          ))
          
        )) # end of inner table
      
        )) # end of renew condition panel cell
      
    )), # end of outer table
       
    conditionalPanel(
      condition = "input.renew1b == true",
         
    conditionalPanel(
      condition = "input.changeDebt1b == true",
      sliderInput("debt1b", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage)
    ),
    
    conditionalPanel(
      condition = "input.interestOnly1a == false",
      helpText("Debt can't be changed for an amortized mortgage")
    ),
    
      sliderInput("period1b", "Period (years) for mortgage:", min = 0, max = max.period, value = 10, step = 1)
    ),
    
    # Second mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 2",
      br(),
      h4("Mortgage 2"),
      textInput("name2", "Name of mortgage:", "Fix 1"),
      
      checkboxInput("fixRate2a", "Fixed rate", TRUE),
      sliderInput("debt2a", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage),
      sliderInput("period2a", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step =  step.period),
      
      # Renew?
      tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
        tags$td(checkboxInput("renew2b", "Renew mortgage ", FALSE)),   
        
        tags$td(conditionalPanel(
          condition = "input.renew2b == true",
          tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
            tags$td(checkboxInput("fixRate2b", "Fixed rate ", TRUE)),
                    tags$td(checkboxInput("changeDebt2b", "Change debt", FALSE))
          )) # end of inner table
        )) # end of renew condition panel cell
      
        )), # end of outer table
      
      conditionalPanel(
        condition = "input.renew2b == true",
        
        conditionalPanel(
          condition = "input.changeDebt2b == true",
          sliderInput("debt2b", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage)
        ),
        
        sliderInput("period2b", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step = step.period)
      )
      
    ),
    
    
    
    # Third mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 3",
      br(),
      h4("Mortgage 3"),
      textInput("name3", "Name of mortgage:", "Fix 2"),
      checkboxInput("fixRate3a", "Fixed rate", TRUE),
      sliderInput("debt3a", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage),
      sliderInput("period3a", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step = 1),
      
      # Renew?
      tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
        tags$td(checkboxInput("renew3b", "Renew mortgage ", FALSE)),   
        
        tags$td(conditionalPanel(
          condition = "input.renew3b == true",
          tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
            tags$td(checkboxInput("fixRate3b", "Fixed rate ", TRUE)),
            tags$td(checkboxInput("changeDebt3b", "Change debt", FALSE))
          )) # end of inner table
        )) # end of renew condition panel cell
        
      )), # end of outer table
      
      conditionalPanel(
        condition = "input.renew3b == true",
        
        conditionalPanel(
          condition = "input.changeDebt3b == true",
          sliderInput("debt3b", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage)
        ),
        
        sliderInput("period3b", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step = step.period)
      )
      
    ),
    
    # Fourth mortgage
    conditionalPanel(
      condition = "input.numMortgages >= 4",
      br(),
      h4("Mortgage 4"),
      textInput("name4", "Name of mortgage:", "Fix 3"),
      checkboxInput("fixRate4a", "Fixed rate", TRUE),
      sliderInput("debt4a", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage),
      sliderInput("period4a", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step = 1),
      
      # Renew?
      tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
        tags$td(checkboxInput("renew4b", "Renew mortgage ", FALSE)),   
        
        tags$td(conditionalPanel(
          condition = "input.renew4b == true",
          tags$table(border="0", cellpadding=cellpadding, cellspacing=cellspacing, tags$tr(
            tags$td(checkboxInput("fixRate4b", "Fixed rate ", TRUE)),
            tags$td(checkboxInput("changeDebt4b", "Change debt", FALSE))
          )) # end of inner table
        )) # end of renew condition panel cell
        
      )), # end of outer table
      
      conditionalPanel(
        condition = "input.renew4b == true",
        
        conditionalPanel(
          condition = "input.changeDebt4b == true",
          sliderInput("debt4b", "Amount for mortgage:", min = 0, max = max.mortgage, value = value.mortgage, step = step.mortgage)
        ),
        
        sliderInput("period4b", "Period (years) for mortgage:", min = 0, max = max.period, value = value.period, step = step.period)
      )
      
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
    h4("Forcast for LIBOR interest rates"),
    helpText('Input the current LIBOR rate, and what you think the rate wil be in the future.
              This is the rate used for LIBOR (non-fixed rate) mortgages. The future fixed rates are changed
              overtime by the same absolute amount as the LIBOR rate. 
              A graph of your assumptions in given in the "Interest rate assumptions" tab.'),
    
    numericInput("LIBOR.rate0", "Current LIBOR rate (%):", 0.98, min = 0, step = 0.01),
    numericInput("LIBOR.rate1", "I think the future LIBOR rate (%) will be:", 5, min = 0, step = 0.01),
    numericInput("LIBOR.time1", "Years to this future rate", 10, min = 1, max = 10, step = 1),
    checkboxInput("moreLIBOR", "Add extra time periods:", FALSE),
    conditionalPanel(
      condition = "input.moreLIBOR == true",
      numericInput("LIBOR.rate2", "Then I think the future LIBOR rate (%) will be:", 5, min = 0, step = 0.01),
      numericInput("LIBOR.time2", "Years to this future rate", 10, min = 1, max = 10, step = 1)
    ),
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
               h4(textOutput("summaryTitle")),
               tableOutput("summary"),
               #textOutput("debug"),
               #htmlOutput("summaryXtable"),
               htmlOutput("googleAnalytics")
      ), 
      tabPanel("Interest rate assumptions",
               h4("Fixed interest rates for each period"),
               plotOutput("currentRatePlot"), 
               
               h4("LIBOR interest rates over time"),
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

