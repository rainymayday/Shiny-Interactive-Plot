library(shiny)
load_df <- function(Rfile){
  df <- source(Rfile)
  df <- as.data.frame(df[1])
  names(df) <- gsub("value.","",names(df))
  return (df)
}

leads <- load_df('Leads.R')
sales <- load_df('SO.R')






shinyUI(navbarPage("Sales Activity Report",
                   tabPanel("Leads",
                            column(4, wellPanel( 
                              selectInput('segment', 'Segment',  unique(as.character(leads$segment))),
                              selectInput('LeadsGen', 
                                          'Leads Generator', 
                                          unique(as.character(leads$Lead.Generator)),
                                          selectize = TRUE,
                                          multiple = FALSE),
                              uiOutput('dateRange'),
                              
                              br(),
                              
                              fluidRow(
                                column(7, radioButtons("plotty", "Plot Type",
                                                       c("By day"="day","By week"="week")
                                                       , selected="day")
                                       
                                )
                            
                              ),
                              
                                fluidRow(
                                  column(7,h5(strong('Plot Options')),
                                         checkboxInput('avg_line', 'Compare with average level within segment'),
                                         downloadButton('downloadLeads', 'Download Leads Table'))
                                
                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("LeadsPlot",plotOutput("LeadsPlot")),
                                          tabPanel("LeadsTable",dataTableOutput("LeadsTable"))
                                          
                                          
                              )
                              
                              
                            )
                   ),
                   tabPanel("Sales",
                            column(4, wellPanel( 
                              selectInput('segment_sale', 'Segment',  unique(as.character(sales$segment))),
                              selectInput('sales_rep', 
                                          'sales_rep', 
                                          unique(as.character(sales$Sales.Rep.1)),
                                          selectize = TRUE),
                              uiOutput('dateRange_sale'),
                              br(),
                              
                              fluidRow(
                                column(7, radioButtons("plotty_sale", "Plot Type",
                                                       c("By day"="day","By week"="week")
                                                       , selected="day")
                                )
                              ),
                              
                              fluidRow(
                                column(7,
                                       h5(strong('Plot Options')),
                                       checkboxInput('avg_line_sale', 'Compare with average level within segment'),
                                       downloadButton('downloadSales', 'Download Sales Table'))

                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("SalesPlot",plotOutput("SalesPlot")),
                                          tabPanel("SalesTable",dataTableOutput("SalesTable"))
                                          
                                          
                              )
                              
                              
                            )
                   ),
                   tabPanel("SummaryTable"
                            )
                   
                   
                   
                   ))