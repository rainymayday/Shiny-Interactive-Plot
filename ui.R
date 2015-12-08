library(shiny)
setwd('C:/Users/Sunny/Desktop/Recent/Internship')

leads <- read.csv('Leads.csv')
leads$Date.Created <- strptime(as.character(leads$Date.Created), "%d/%m/%Y")
cols <- c("Lead.Generator","segment")
leads$Lead.Generator <- apply(leads[,cols], 1 ,paste, collapse = "-" )
leads_date_generator <- count(leads,c("Date.Created","Lead.Generator"))
leads_date_generator$Lead.Generator <- as.factor(leads_date_generator$Lead.Generator)


shinyUI(fluidPage(
  
  titlePanel("Sales Lead report"),
  
  sidebarLayout(
    sidebarPanel(
    selectInput('y.factor', 
                  'sales_rep', 
                  unique(as.character(leads_date_generator$Lead.Generator))),
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 14, end = Sys.Date() 
    )

    ),
    
    
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("LeadsPlot",plotOutput("LeadsPlot")),
                  tabPanel("LeadsTable",dataTableOutput("LeadsTable")))
                  
      )
      
      
    )
  )
)