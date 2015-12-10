library(shiny)
leads <- source('Leads.R')
leads <- as.data.frame(leads[1])
names(leads) <- c("Date.Created","Lead.Generator","segment", "freq")
shinyUI(fluidPage(
  
  titlePanel("Sales Activity Report"),
  
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
                     label = 'Date Range',
                     start = Sys.Date() - 14, end = Sys.Date() 
      ),
      h5(strong('Plot Options')),
      checkboxInput("Month",
                    label = "By Month",value = FALSE),
      checkboxInput("Week",
                    label = "By Week",value = FALSE),
      checkboxInput("Day",
                    label = "By Day",value = FALSE),
      downloadButton('downloadSales', 'Download Sales Table'),
      downloadButton('downloadLeads', 'Download Leads Table')
      
      

    ),
    

    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("LeadsPlot by day",plotOutput("LeadsPlot")),
                  tabPanel("LeadsPlot by Week",plotOutput("LeadsPlot_by_week")),
                  tabPanel("SalesPlot by day",plotOutput("SalesPlot")),
                  tabPanel("SalesPlot by Week",plotOutput("SalesPlot_by_week")),
                  tabPanel("LeadsTable",dataTableOutput("LeadsTable")),
                  tabPanel("SalesTable",dataTableOutput("SalesTable"))
                  )
                 
                  
      )
      
      
    )
  )
)