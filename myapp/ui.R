library(shiny)
require(rCharts)
library(DT)
shinyUI(
  navbarPage(
    tags$head(tags$style("h4 {font-family:
                            font-weight: bold;
                         line-height: 0.2; 
                         color: #4d3a7d; }")),
     tabPanel(
       h4("Upload Data File For Analysis"),
                            column(3, wellPanel(
                              helpText("Choose spreadsheet file:(xlsx)"),
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
                            )
                     ),
                            
                   tabPanel(
                     h4("Leads"),
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
                                       checkboxInput("compare_rep","Compare with Other sales Rep")
                                       )
                              ))
                            ),
                            mainPanel(
                              showOutput("LeadsPlot","highcharts")
                              )),
                   tabPanel(
                     h4("Proposal"),
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
                                       checkboxInput("comp_pro","Comapre with other Proposal Creator")
                                       
                                )
                                
                              )
                            )
                            ),
                            mainPanel(
                              showOutput("ProposalPlots","highcharts")
                              )),
                   tabPanel(
                     h4("Contract"),
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
                                       checkboxInput("com_con","Compare with other contractor creators")
                                       
                                       )
                              )
                            )),
                            mainPanel(
                              showOutput("ContractPlots","highcharts")
                            )),
                   tabPanel(
                     h4("Sales Order"),
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
                                   checkboxInput("com_so","Compare with other sales rep")
                                   )
                              ))
                            ),
                        mainPanel(
                          showOutput("SalesPlot","highcharts")
                          )
                            ),
                   tabPanel(
                     h4("Summary"),
                            column(3,wellPanel(
                              fluidRow(
                                column(7, radioButtons("reportty", "Report Type",
                                                       c("By month"="month","By week"="week")
                                                       , selected="month")
                                )),
                              uiOutput("year_summary"),
                              uiOutput("segLevel")
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Highlights",
                                                   DT::dataTableOutput('SummaryTable'),
                                                   icon = icon("list-alt")
                                          ),
                                          tabPanel("Summary Plots",
                                                   fluidRow(
                                                     showOutput("SummaryLeads","nvd3") 
                                                   ),
                                                   fluidRow(
                                                     showOutput("SummaryProposals","nvd3") 
                                                   ),
                                                   fluidRow(
                                                     showOutput("SummaryContracts","nvd3") 
                                                   ),
                                                   fluidRow(
                                                     showOutput("SummarySOs","nvd3") 
                                                   ),
                                                   
                                                   icon = icon("bar-chart-o"))
                                          )
                            )),windowTitle = "Sales Activity Report"
                            ))
                