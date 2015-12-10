library(shiny)
leads <- source('Leads.R')
leads <- as.data.frame(leads[1])
names(leads) <- c("Date.Created","Lead.Generator","segment", "freq")
shinyUI(fluidPage(
  
  titlePanel("Sales Activity report"),
  
  sidebarLayout(
    sidebarPanel(

      selectInput('segment', 
                    'segment', 
                    unique(as.character(leads$segment))),

      selectInput('y.factor', 
                  'sales_rep', 
                  unique(as.character(leads$Lead.Generator)),
                  selectize = TRUE),
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 14, end = Sys.Date() 
      )
      

    ),

    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("LeadsPlot by day",plotOutput("LeadsPlot")),
                  tabPanel("LeadsPlot by Week",plotOutput("LeadsPlot_by_week")),
                  tabPanel("SalesPlot by Week",plotOutput("SalesPlot")),
                  tabPanel("LeadsTable",dataTableOutput("LeadsTable")),
                  tabPanel("SalesTable",dataTableOutput("SalesTable"))
                  )
                 
                  
      )
      
      
    )
  )
)