library(shiny)
shinyUI(navbarPage("Sales Activity Report",
                   tabPanel("Upload Data File For Analysis",
                            column(3, wellPanel(
                              fileInput('file0', 'Upload Sales_rep Table',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file1', 'Upload Leads Table',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file3', 'Upload Contract Table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file4', 'Upload Proposal Table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file2', 'Upload Sales Order Table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE)
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                type = "tab",
                                tabPanel("Sales_Rep",tableOutput('salesperson')),
                                tabPanel("Leads",tableOutput('leads')),
                                tabPanel("contract",tableOutput("contact")),
                                tabPanel("Proposal",tableOutput("proposal")),
                                tabPanel("SalesOrder",tableOutput("sales"))
                              )
                            )),
                            
                   tabPanel("Leads",
                            column(3, wellPanel(
                              uiOutput('segment'),
                              uiOutput('LeadsGen'),
                              uiOutput('dateRange'),
                              uiOutput("year"),
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty", "Plot Type",
                                                       c("By day"="day","By week"="week","By month"="month")
                                                       , selected="day")
                                )
                              ),
                              
                              fluidRow(
                                column(12,h5(strong('Plot Options')),
                                       checkboxInput('avg_line','Compare with average level'),
                                       checkboxInput("avg_self","Compare with self average level"),
                                       downloadButton('downloadLeads','Download Leads Table'))
                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("LeadsPlot",plotOutput("LeadsPlot")),
                                          tabPanel("LeadsTable",dataTableOutput("LeadsTable"))))),
                   tabPanel("Contract",
                            column(3,wellPanel(
                              uiOutput("segment_con"),
                              uiOutput("Contract_creator"),
                              uiOutput('dateRange_con'),
                              uiOutput("year_con"),
                              br(),
                              
                              fluidRow(
                                column(7, radioButtons("plotty_con", "Plot Type",
                                                       c("By day"="day","By week"="week","By month"="month")
                                                       , selected="day")
                                )
                              ),
                              downloadButton('downloadContract'
                                             ,'Download Contract Table')
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Contract Plots",plotOutput("ContractPlots")),
                                          tabPanel("Contract Tables",tableOutput("ContractTable")))  
                              
                            )),
                   tabPanel("Proposal",
                            column(4,wellPanel(
                              uiOutput("segment_pro"),
                              uiOutput("Proposal_creator"),
                              uiOutput('dateRange_pro'),
                              uiOutput("year_pro"),
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty_pro", "Plot Type",
                                                       c("By day"="day","By week"="week","By month" ="month")
                                                       , selected="day")
                                )
                              ),
                              downloadButton('downloadProposal'
                                             ,'Download Proposal Table'))
                            ),
                            mainPanel(tabsetPanel(type="tab",
                                                  tabPanel("Proposal Plots",plotOutput("ProposalPlots")),
                                                  tabPanel("Proposal Tables",
                                                           dataTableOutput("ProposalTable"))))),
                   
                   tabPanel("Sales Order",
                            column(3, wellPanel(
                              uiOutput("segment_sale"),
                              uiOutput("sales_rep"),
                              uiOutput('dateRange_sale'),
                              uiOutput('year_sale'),
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty_sale", "Plot Type",
                                                       c("By day"="day","By week"="week","By month"="month")
                                                       , selected="day")
                                )
                              ),
                              
                              fluidRow(
                                column(12,
                                       h5(strong('Plot Options')),
                                       checkboxInput('avg_line_sale'
                                                     ,'Compare with average level within segment'),
                                       checkboxInput('avg_self_sale'
                                                     ,'Compare with self average level'),
                                       downloadButton('downloadSales'
                                                      ,'Download Sales Table'))
                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("SalesPlot",plotOutput("SalesPlot")),
                                          tabPanel("SalesTable",dataTableOutput("SalesTable"))
                                          
                              ))
                            
                            ),
                  
                   tabPanel("Summary",
                            column(3,wellPanel(
                              fluidRow(
                                column(7, radioButtons("reportty", "Report Type",
                                                       c("By month"="month","By week"="week")
                                                       , selected="week")
                                )),
                              uiOutput("year_summary"),
                              uiOutput("selectList"),
                              selectInput("level","Level",choices = c("segment","sales rep")),
                              uiOutput("segLevel"),
                              uiOutput("RepLevel")
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Highlights",
                                                   h2(textOutput("title")),
                                                   br(),
                                                   fluidRow(
                                                     column(6,h3("Leads Summary"),
                                                            tableOutput("leads1")),
                                                     column(6,h3("Contract Summary"),
                                                            tableOutput("contract1")
                                                      )
                                                     ),
                                                   fluidRow(
                                                     column(6,h3("Proposal Summary"),
                                                            tableOutput("proposal1")
                                                            ),
                                                     column(6,h3("Sales Order Summary"),
                                                            tableOutput("so1")))
                                          ),
                                          tabPanel("TEST",
                                                   tableOutput("TEST"),
                                                   textOutput("VALUE")
                                                   )
                                          )
                            ))
                            ))
                