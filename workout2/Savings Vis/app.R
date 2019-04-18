#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape)

#Define FV, FVA and FVGA functions
#  @ Title:  Future value
#  @ Description:  FV calculates future value of a single fixed deposit
#  @ Param:  deposit amount, rate of return, years of maturity
#  @ Return:  future value after 'years' years

FV <- function(amount, rate, years) {
    if (min(years) < 0) {
      stop("years must be positive")
    }
    return(amount*((1+rate)^years))
}

#  @ Title:  Future value annuity
#  @ Description:  FVA calculates future value of a stream of constant yearly deposits
#  @ Param:  deposit amount, rate of return, years of maturity
#  @ Return:  future value of annuity after 'years' years

FVA <- function(contrib, rate, years){
  if (min(years) < 0) {
    stop("years must be positive")
  }
  return(contrib*((((1+rate)^years) - 1)/rate))
}

#  @ Title:  Future value growth annuity
#  @ Description:  FVGA calculates future value of a stream of consistently timed deposits growing at a constant rate
#  @ Inputs:  deposit amount, rate of return, years of maturity
#  @ Output:  future value of growing annuity after 'years' years

FVGA <- function(contrib, rate, growth, years) {
  if (min(years) < 0){
    stop("years must be positive")
  }
  return(contrib*((((1+rate)^years) - ((1+growth)^years))/(rate-growth)))
}
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Visualisation"),
   
   # Sidebar with a slider input for number of bins 
   wellPanel(
      fluidRow(
        column(4, 
               sliderInput("in.amnt",
                              "Inital Amount",
                              min = 0,
                              max = 100000,
                              value = 1000,
                              step = 500),
               sliderInput("an.cnt",
                           "Annual Contribution",
                           min = 0,
                           max = 50000,
                           value = 2000,
                           step = 500),
               sliderInput("rtrn",
                           "Return Rate",
                           min = 0,
                           max = 0.2,
                           value = 0.05,
                           step = 0.001)
               ),
         
         column(4, 
                sliderInput("grth",
                     "Growth Rate",
                     min = 0,
                     max = 0.2,
                     value = 0.02,
                     step = 0.001),
               sliderInput("yrs",
                           "Years",
                           min = 0,
                           max = 50,
                           value = 20,
                           step = 1),
               selectInput("fct", 
                          "Facet?",
                           c("Yes", "No"),
                          selected = "No")
      ))),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(strong("Investments")),
        hr(),
        h4(strong("Plot of Savings Plan Options")),
        plotOutput("savings.plt"),
        hr(),
        h4(strong("Table of Savings Plan Options")),
        verbatimTextOutput("savings.tbl", placeholder=TRUE)
      )
)


server <- function(input, output) {
  
#Generate blank table to fill with results
  modalities <- reactive({
    #Blank Table
    modalities <- data.frame(array(0, c(input$yrs + 1, 4)))
    
    #Years Column
    modalities[ , 1] <- c(0:input$yrs)
    
    #Column Names
    names(modalities) <- c("Year", "No contribution", "Fixed contribution", "Growing Contribution")
    
    #Columns with data
    modalities[ , 2] <- FV(input$in.amnt, input$rtrn, 0:input$yrs)
    modalities[ , 3] <- FV(input$in.amnt, input$rtrn, 0:input$yrs) + FVA(input$an.cnt, input$rtrn, 0:input$yrs)
    modalities[ , 4] <- FV(input$in.amnt, input$rtrn, 0:input$yrs) + FVGA(input$an.cnt, input$rtrn, input$grth, 0:input$yrs)
    
    return(modalities)
  })
  
  #Rearrange longways - for plotting
  long.table <- reactive({
    melt.data.frame(modalities(), id.vars = "Year", variable_name = "Plan")
  })
  
  #Generate image plot
  output$savings.plt <- renderPlot({
    balance.long.table <- long.table()
    
    if (input$fct == "No") {
      ggplot(data = balance.long.table) + geom_line(aes(x = Year, y = value, colour = Plan)) + theme_gray()
    }
    else {
      ggplot(data = balance.long.table, aes(x = Year, y = value, colour = Plan, fill = Plan)) + facet_grid( .~ Plan) + geom_area(alpha = 0.4) + theme_gray()
    }
  })

  #Generate table
  output$savings.tbl <- renderPrint({modalities()})
}

# Run the application 
shinyApp(ui = ui, server = server)

