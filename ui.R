library(shiny)
require(rCharts)
shinyUI(
  navbarPage("Sales Activity Report",
                   tabPanel("Upload Data File For Analysis",
                            column(3, wellPanel(
                              fileInput('file0', 'Upload Sales_rep Table',
                                        multiple = FALSE),
                              fileInput('file1', 'Upload Leads Table',
                                        multiple = FALSE),
                              fileInput('file4', 'Upload Proposal Table ',
                                        multiple = FALSE),
                              fileInput('file2', 'Upload Sales Order Table ',
                                        multiple = FALSE)
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                type = "tab",
                                tabPanel("Sales Rep",tableOutput('salesperson'), icon = icon("table")),
                                tabPanel("Leads",tableOutput('leads'), icon = icon("table")),
                                tabPanel("Proposal",tableOutput("proposal"), icon = icon("table")),
                                tabPanel("Contract",tableOutput("contact"), icon = icon("table")),
                                tabPanel("Sales Order",tableOutput("sales"), icon = icon("table"))
                              )
                            )),
                            
                   tabPanel("Leads",
                            column(3, wellPanel(
                              uiOutput('segment'),
                              uiOutput('LeadsGen'),
                              uiOutput("LeadsGen1"),
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
                                       checkboxInput('avg_line','Team Average'),
                                       checkboxInput("avg_self","Individual Average"),
                                       checkboxInput("compare_rep","Compare with Other sales Rep"),
                                       checkboxInput("compare_last_year","Compare with last year performance"),
                                       downloadButton('downloadLeads','Download Leads Table'))
                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("Leads Plot",showOutput("LeadsPlot","highcharts"), icon = icon("bar-chart-o")),
                                          tabPanel("Leads Table",dataTableOutput("LeadsTable"), icon = icon("table"))
                                          ))),
                   tabPanel("Proposal",
                            column(3,wellPanel(
                              uiOutput("segment_pro"),
                              uiOutput("Proposal_creator"),
                              uiOutput("Proposal_creator1"),
                              uiOutput('dateRange_pro'),
                              uiOutput("year_pro"),
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty_pro", "Plot Type",
                                                       c("By day"="day","By week"="week","By month" ="month")
                                                       , selected="day")
                                )
                              ),
                              fluidRow(
                                column(12,h5(strong('Plot Options')),
                                       checkboxInput("avg_line_pro","Team Average"),
                                       checkboxInput("avg_self_pro","Individual Average"),
                                       checkboxInput("proposal_no","Show No of Proposal"),
                                       checkboxInput("comp_pro","Comapre with other Proposal Creator"),
                                       downloadButton('downloadProposal','Download Proposal Table')
                                )
                                
                              )
                            )
                            ),
                            mainPanel(tabsetPanel(type="tab",
                                                  tabPanel("Proposal Plot",showOutput("ProposalPlots","highcharts"), icon = icon("bar-chart-o")),
                                                  tabPanel("Proposal Table",
                                                           dataTableOutput("ProposalTable"), icon = icon("table"))))),
                   tabPanel("Contract",
                            column(3,wellPanel(
                              uiOutput("segment_con"),
                              uiOutput("Contract_creator"),
                              uiOutput("Contract_creator1"),
                              uiOutput('dateRange_con'),
                              uiOutput("year_con"),
                              br(),
                              
                              fluidRow(
                                column(7, radioButtons("plotty_con", "Plot Type",
                                                       c("By day"="day","By week"="week","By month"="month")
                                                       , selected="day")
                                )
                              ),
                              fluidRow(
                                column(12,h5(strong('Plot Options')),
                                       checkboxInput("avg_line_con","Team Average"),
                                       checkboxInput("avg_self_con","Individual Average"),
                                       checkboxInput("contract_no","Show No of Contracts"),
                                       checkboxInput("com_con","Compare with other contractor creators"),
                                       downloadButton('downloadContract','Download Contract Table')
                                       )
                              )
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Contract Plot",showOutput("ContractPlots","highcharts"), icon = icon("bar-chart-o")),
                                          tabPanel("Contract Table",dataTableOutput("ContractTable"), icon = icon("table")))  
                            )),
                   tabPanel("Sales Order",
                            column(3, wellPanel(
                              uiOutput("segment_sale"),
                              uiOutput("sales_rep"),
                              uiOutput("sales_rep1"),
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
                                                     ,'Team Average'),
                                       checkboxInput('avg_self_sale'
                                                     ,'Individual Average'),
                                       checkboxInput("SO_no","Show No of Sales Order"),
                                       checkboxInput("com_so","Compare with other sales rep"),
                                       downloadButton('downloadSales'
                                                      ,'Download Sales Table'))
                              ))
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("Sales Plot",showOutput("SalesPlot","highcharts"), icon = icon("bar-chart-o")),
                                          tabPanel("Sales Table",dataTableOutput("SalesTable"), icon = icon("table"))
                                          
                              ))
                            ),
                  
                   tabPanel("Summary",
                            column(3,wellPanel(
                              fluidRow(
                                column(7, radioButtons("reportty", "Report Type",
                                                       c("By month"="month","By week"="week")
                                                       , selected="month")
                                )),
                              uiOutput("year_summary"),
                              uiOutput("selectList"),
                              selectInput("level","Level",choices = c("Segment","Sales Rep"),
                                          selected = "Segment"),
                              uiOutput("segLevel"),
                              uiOutput("RepLevel")
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Highlights",
                                                   h2(textOutput("title")),
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
                                                            tableOutput("so1"))), icon = icon("table")
                                          )
                                          )
                            ))
                            ))
                