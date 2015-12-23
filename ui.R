library(shiny)
library(dygraphs)
shinyUI(navbarPage("Sales Activity Report",
                   tabPanel("Upload Data File For Analysis",
                            column(3, wellPanel(
                              fileInput('file0', 'Upload sales_rep table',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file1', 'Upload leads table',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file2', 'Upload sales order table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file3', 'Upload contract table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              fileInput('file4', 'Upload prposal table ',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv'),multiple = FALSE),
                              checkboxInput('header', 'Header', TRUE),
                              radioButtons('sep', 'Separator',
                                           c(Comma=',',
                                             Semicolon=';',
                                             Tab='\t'),
                                           ','),
                              radioButtons('quote', 'Quote',
                                           c(None='',
                                             'Double Quote'='"',
                                             'Single Quote'="'"),
                                           '"')
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
                                                       c("By day"="day","By week"="week")
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
                            column(4,wellPanel(
                              uiOutput("segment_con"),
                              uiOutput("Contract_creator"),
                              uiOutput('dateRange_con'),
                              br(),
                              
                              fluidRow(
                                column(7, radioButtons("plotty_con", "Plot Type",
                                                       c("By day"="day","By week"="week")
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
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty_pro", "Plot Type",
                                                       c("By day"="day","By week"="week")
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
                            column(4, wellPanel(
                              uiOutput("segment_sale"),
                              uiOutput("sales_rep"),
                              uiOutput('dateRange_sale'),
                              br(),
                              fluidRow(
                                column(7, radioButtons("plotty_sale", "Plot Type",
                                                       c("By day"="day","By week"="week")
                                                       , selected="day")
                                )
                              ),
                              
                              fluidRow(
                                column(9,
                                       h5(strong('Plot Options')),
                                       checkboxInput('avg_line_sale'
                                                     ,'Compare with average level within segment'),
                                       checkboxInput('compare2'
                                                     ,'Compare with other sales representatives'),
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
                                                       , selected="week"),
                                       uiOutput("selectList")
                                )),
                              selectInput("level","Level",choices = c("segment","sales rep")),
                              #,
                              #,
                              uiOutput("segLevel"),
                              uiOutput("RepLevel"),
                              downloadButton("downloadReport","Export Report")
                              
                            )),
                            mainPanel(
                              tabsetPanel(type = "tab",
                                          tabPanel("Highlights",
                                                   h2(textOutput("title")),
                                                   br(),
                                                   fluidRow(
                                                     column(6,h3("Leads Summary"),
                                                            #tableOutput("leads1"),
                                                            textOutput("total_leads"),
                                                            textOutput("avgperperson_leads"),
                                                            textOutput("avginSeg_leads")),
                                                     column(6,h3("Contract Summary"),
                                                            #tableOutput("contract1"),
                                                            textOutput("total_contracts"),
                                                            textOutput("avgperperson_contracts"),
                                                            textOutput("avginSeg_contracts"),
                                                            br(),
                                                            textOutput("total_contracts_no"),
                                                            textOutput("avgperperson_contracts_no"),
                                                            textOutput("avginSeg_contracts_no")
                                                            
                                                     )),
                                                   fluidRow(
                                                     column(6,h3("Proposal Summary"),
                                                            #tableOutput("proposal1"),
                                                            textOutput("total_proposals"),
                                                            textOutput("avgperperson_proposals"),
                                                            textOutput("avginSeg_proposals"),
                                                            br(),
                                                            textOutput("total_proposals_no"),
                                                            textOutput("avgperperson_proposals_no"),
                                                            textOutput("avginSeg_proposals_no")),
                                                     column(6,h3("Sales Order Summary"),
                                                            #tableOutput("so1"),
                                                            textOutput("total_SO"),
                                                            textOutput("avgperperson_SO"),
                                                            textOutput("avginSeg_SO"),
                                                            br(),
                                                            textOutput("total_SO_no"),
                                                            textOutput("avgperperson_SO_no"),
                                                            textOutput("avginSeg_SO_no")
                                                     ))
                                          ),
                                          
                                          tabPanel("Details"))
                            ))
                            ))
                