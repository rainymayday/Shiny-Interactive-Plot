library(shiny)
leads <- source('Leads.R')
leads <- as.data.frame(leads[1])
names(leads) <- c("Date.Created","Lead.Generator","segment", "freq")
shinyUI(navbarPage("Sales Activity Report",
                   tabPanel("Leads",
                            column(4, wellPanel( 
                              selectInput('segment', 'Segment',  unique(as.character(leads$segment))),
                              selectInput('y.factor', 
                                          'sales_rep', 
                                          unique(as.character(leads$Lead.Generator)),
                                          selectize = TRUE),
                              dateRangeInput('dateRange','Choose date range'
                                             ,start = Sys.Date() - 14, end = Sys.Date()),
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
                              selectInput('segment_sale', 'Segment',  unique(as.character(leads$segment))),
                              selectInput('sales_rep', 
                                          'sales_rep', 
                                          unique(as.character(leads$Lead.Generator)),
                                          selectize = TRUE),
                              dateRangeInput('dateRange_sale','Choose date range'
                                             ,start = Sys.Date() - 14, end = Sys.Date()),
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