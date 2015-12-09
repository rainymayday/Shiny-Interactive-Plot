library(shiny)
df <- source('Leads.R')
df <- as.data.frame(df[1])
names(df) <- c("Date.Created","Lead.Generator","segment", "freq")
shinyUI(fluidPage(
  
  titlePanel("Sales Activity report"),
  
  sidebarLayout(
    sidebarPanel(

      selectInput('segment', 
                    'segment', 
                    unique(as.character(df$segment))),

      selectInput('y.factor', 
                  'sales_rep', 
                  unique(as.character(df$Lead.Generator)),
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
                  tabPanel("LeadsTable",dataTableOutput("LeadsTable")))
                  
      )
      
      
    )
  )
)