options(shiny.maxRequestSize=30*1024^2) 
library(shiny)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plyr)
library(scales)
require(lubridate)
library(ISOweek)
if(!require(rCharts)){
  require(devtools)
  install_github('ramnathv/rCharts')
  library(rCharts)
}
require(rCharts)
options(RCHART_LIB = 'polycharts')
date_in_week <- function(year, week, weekday=7){
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  return(paste(ISOweek2date(w),ISOweek2date(w)+7,sep = " ~ "))
}
factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
}

shinyServer(function(input, output,session) {
  # import data frame from uploaded csv
  salesrep <- reactive({
    infile <- input$file0
    if (is.null(infile)) {
      return(NULL)
    }
    df <- read.csv(infile$datapath)
    return(df)
  })
  num_confex <- reactive({
    num_confex <- length(salesrep()$sale_rep[salesrep()$segment=="Confex"])
    return(num_confex)
  })
  num_corporate <- reactive({
    num_corporate <-length(salesrep()$sale_rep[salesrep()$segment=="Corporate"])
    return(num_corporate)
  })
  leads <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    isolate(
      {
        leads <- read.csv(infile$datapath)
        leads$Date.Created <- as.character(as.Date(leads$Date.Created, "%d/%m/%Y"))
        leads <- merge(leads, salesrep(),by.x= "Lead.Generator",by.y = "sale_rep")
        leads_date_generator <- count(leads,c("Date.Created","Lead.Generator","segment"))
        leads_date_generator$Lead.Generator <- as.factor(leads_date_generator$Lead.Generator)
        leads_date_generator$week <- week(leads_date_generator$Date.Created)
        leads_date_generator$month <- month(leads_date_generator$Date.Created,label=TRUE)
        leads_date_generator$year <- year(leads_date_generator$Date.Created)
      }
    )
    return(leads_date_generator)
  })
  sales <- reactive({
    infile <- input$file2
    if (is.null(infile)){
      return(NULL)
    }
    isolate({
      sales <- read.csv(infile$datapath)
      sales <- subset(sales,sales$Sales.Rep.2 %in% as.character(salesrep()$sale_rep))
      col <- c("SO.Number","Date.Created","Name","Sales.Rep.2","Event.Name"
               ,"Maximum.of.Amount..Net.of.Tax.")
      sales <- sales[,col]
      sales$Maximum.of.Amount..Net.of.Tax. <- factor2numeric(sales$Maximum.of.Amount..Net.of.Tax.)
      sales <- merge(sales, salesrep(),by.x= "Sales.Rep.2",by.y = "sale_rep")
      sales$Date.Created <- as.character(as.Date(sales$Date.Created, "%d/%m/%Y"))
      sales$week <- week(sales$Date.Created)
      sales$month <- month(sales$Date.Created,label = TRUE)
      sales$year <- year(sales$Date.Created)
    })
    return(sales)
  })
  contract <- reactive({
    infile <- input$file3
    if (is.null(infile)){
      return(NULL)
    }
    isolate({
      contract <- read.csv(infile$datapath)
      contract <- subset(contract,contract$Created.By %in% as.character(salesrep()$sale_rep))
      col <- c("Date.Created","Created.By","Name","Amount..Net.of.Tax.")
      contract <- contract[,col]
      contract$Amount..Net.of.Tax. <- factor2numeric(contract$Amount..Net.of.Tax.)
      contract$Date.Created <- as.character(as.Date(contract$Date.Created, "%d/%m/%Y"))
      contract <- merge(contract, salesrep(),by.x= "Created.By",by.y = "sale_rep")
      contract$week <- week(contract$Date.Created)
      contract$month <-month(contract$Date.Created,label = TRUE)
      contract$year <- year(contract$Date.Created)
    })
    return (contract)
  })
  proposal <- reactive({
    infile <- input$file4
    if (is.null(infile)){
      return(NULL)
    }
    isolate({
      proposal <- read.csv(infile$datapath)
      proposal <- proposal[!duplicated(proposal),]
      cols <- c("Date.Created","Name","Created.By"
                ,"Amount..Net.of.Tax.")
      proposal <- proposal[,cols]
      proposal$Amount..Net.of.Tax. <- factor2numeric(proposal$Amount..Net.of.Tax.)
      proposal <- subset(proposal,proposal$Created.By %in% as.character(salesrep()$sale_rep))
      proposal$Date.Created <- as.character(as.Date(proposal$Date.Created, "%d/%m/%Y"))
      proposal <- merge(proposal, salesrep(),by.x= "Created.By",by.y = "sale_rep")
      proposal$week <- week(proposal$Date.Created)
      proposal$month <- month(proposal$Date.Created,label = TRUE)
      proposal$year <- year(proposal$Date.Created)
    })
    return (proposal)
    
  })
  
  # generate summary tables
  LeadsTable <- reactive({
    validate(
      need(leads() != "","Please Upload leads table!")
    )
    LeadsTable <- aggregate(leads()$freq,by = list(leads()$Lead.Generator,leads()$segment),FUN = sum)
    names(LeadsTable) <- c("Sale_rep","Segment","No of Leads")
    return(LeadsTable)
  })
  SalesTable <- reactive({
    validate(
      need(sales() != "","Please Upload sales table!")
    )
    SalesTable <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                            ,by = list(sales()$Sales.Rep.2,sales()$segment),FUN = sum)
    names(SalesTable) <- c("Sale_rep","Segment","Sales_Amount")
    SalesTable_No <- aggregate(sales()$Event.Name
                               ,by = list(sales()$Sales.Rep.2,sales()$segment),FUN = length)
    SalesTable$No_of_Sales <- SalesTable_No[,3]
    SalesTable$Sales_Amount <- paste("$",comma(SalesTable$Sales_Amount))
    return(SalesTable)
  })
  ContractTable <- reactive({
    validate(
      need(contract() != "","Please Upload contract table")
    )
    contractTable <- aggregate(contract()$Amount..Net.of.Tax.
                               ,by = list(contract()$Created.By,contract()$segment),FUN = sum)
    names(contractTable) <- c("Created.By","Segment","Total Amount")
    contractTable_No <- aggregate(contract()$Amount..Net.of.Tax.
                                  ,by = list(contract()$Created.By,contract()$segment),FUN=length)
    contractTable$No_Of_Contract <- contractTable_No[,3]
    contractTable$`Total Amount` <- paste("$",comma(contractTable$`Total Amount`))
    return(contractTable)
  })
  ProposalTable <- reactive({
    validate(
      need(proposal() != "","Please Upload proposal table")
    )
    ProposalTable <- aggregate(proposal()$Amount..Net.of.Tax.,by = list(proposal()$Created.By,proposal()$segment),FUN = sum)
    names(ProposalTable) <- c("Created.By","Segment","Total Amount")
    ProposalTable_No <- aggregate(proposal()$Amount..Net.of.Tax.,by = list(proposal()$Created.By,proposal()$segment),FUN = length)
    ProposalTable$No_of_Proposal <- ProposalTable_No[,3]
    ProposalTable$`Total Amount` <- paste("$",comma(ProposalTable$`Total Amount`))
    return(ProposalTable)
  })
  
  # datasets for summary part
  leads1 <- reactive({
    leads1 <-subset(leads(),
                    leads()$year == input$year_summary)
    if(input$level == "sales rep"){
      leads1 <- subset(leads1,trimws(leads1$Lead.Generator,"both") %in% trimws(input$RepLevel,"both"))
    }else if(input$level == "segment"){
      leads1 <- subset(leads1,trimws(leads1$segment,"both") %in% trimws(input$segLevel,"both"))
    }
    if (input$reportty == "month"){
      leads1 <- subset(leads1,trimws(leads1$month,"both") == trimws(input$list,"both"))
    }
    else if(input$reportty == "week"){
      leads1 <- subset(leads1,trimws(leads1$week,"both") == trimws(input$list,"both"))
    }
    return(leads1)
  })
  contract1 <- reactive({
    contract1 <-subset(contract(),
                    contract()$year == input$year_summary)
    if(input$level == "sales rep"){
      contract1 <- subset(contract1,trimws(contract1$Created.By,"both") %in% trimws(input$RepLevel,"both"))
    }else if(input$level == "segment"){
      contract1 <- subset(contract1,trimws(contract1$segment,"both") %in% trimws(input$segLevel,"both"))
    }
    if (input$reportty == "month"){
      contract1 <- subset(contract1,trimws(contract1$month,"both") == trimws(input$list,"both"))
    }
    else if(input$reportty == "week"){
      contract1 <- subset(contract1,trimws(contract1$week,"both") == trimws(input$list,"both"))
    }
    return(contract1)
  })
  proposal1 <- reactive({
    proposal1 <-subset(proposal(),
                       proposal()$year == input$year_summary)
    if(input$level == "sales rep"){
      proposal1 <- subset(proposal1,trimws(proposal1$Created.By,"both") %in% trimws(input$RepLevel,"both"))
    }else if(input$level == "segment"){
      proposal1 <- subset(proposal1,trimws(proposal1$segment,"both") %in% trimws(input$segLevel,"both"))
    }
    if (input$reportty == "month"){
      proposal1 <- subset(proposal1,trimws(proposal1$month,"both") == trimws(input$list,"both"))
    }
    else if(input$reportty == "week"){
      proposal1 <- subset(proposal1,trimws(proposal1$week,"both") == trimws(input$list,"both"))
    }
    return(proposal1)
  })
  so1 <- reactive({
    so1 <-subset(sales(),
                       sales()$year == input$year_summary)
    if(input$level == "sales rep"){
      so1 <- subset(so1,trimws(so1$Sales.Rep.2,"both") %in% trimws(input$RepLevel,"both"))
    }else if(input$level == "segment"){
      so1 <- subset(so1,trimws(so1$segment,"both") %in% trimws(input$segLevel,"both"))
    }
    if (input$reportty == "month"){
      so1 <- subset(so1,trimws(so1$month,"both") == trimws(input$list,"both"))
    }
    else if(input$reportty == "week"){
      so1 <- subset(so1,trimws(so1$week,"both") == trimws(input$list,"both"))
    }
      return(so1)
    })
  # team avg level
  team_avg_leads <- reactive({
    df <- leads()
    if(input$reportty == "month"){
        df.sum <- aggregate(df$freq,by = list(df$year,df$month,df$segment),FUN = sum)
        names(df.sum) <- c("year","month","segment","leads")
    }else if(input$reportty == "week"){
        df.sum <- aggregate(df$freq,by = list(df$year,df$week,df$segment),FUN = sum)
        names(df.sum) <- c("year","week","segment","leads")
    }
      df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  team_avg_so <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,by = list(df$year,df$month,df$segment),FUN = sum)
      names(df.sum) <- c("year","month","segment","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,by = list(df$year,df$week,df$segment),FUN = sum)
      names(df.sum) <- c("year","week","segment","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
    
  })
  team_avg_so_no <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","segment"))
      names(df.sum) <- c("year","month","segment","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","segment"))
      names(df.sum) <- c("year","week","segment","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  team_avg_cont <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month,df$segment),FUN = sum)
      names(df.sum) <- c("year","month","segment","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week,df$segment),FUN = sum)
      names(df.sum) <- c("year","week","segment","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  team_avg_cont_no <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","segment"))
      names(df.sum) <- c("year","month","segment","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","segment"))
      names(df.sum) <- c("year","week","segment","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  team_avg_prop <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month,df$segment),FUN = sum)
      names(df.sum) <- c("year","month","segment","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week,df$segment),FUN = sum)
      names(df.sum) <- c("year","week","segment","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  team_avg_prop_no <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","segment"))
      names(df.sum) <- c("year","month","segment","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","segment"))
      names(df.sum) <- c("year","week","segment","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$segment %in% input$segLevel))
    return(df.sum.1)
  })
  # personal avg level
  personal_avg_leads <- reactive({
    df <- leads()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$freq,by = list(df$year,df$month,df$Lead.Generator),FUN = sum)
      names(df.sum) <- c("year","month","person","leads")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$freq,by = list(df$year,df$week,df$Lead.Generator),FUN = sum)
      names(df.sum) <- c("year","week","person","leads")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  personal_avg_so <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,
                          by = list(df$year,df$month,df$Sales.Rep.2),FUN = sum)
      names(df.sum) <- c("year","month","person","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,
                          by = list(df$year,df$week,df$Sales.Rep.2),FUN = sum)
      names(df.sum) <- c("year","week","person","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  personal_avg_so_no <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","Sales.Rep.2"))
      names(df.sum) <- c("year","month","person","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","Sales.Rep.2"))
      names(df.sum) <- c("year","week","person","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  personal_avg_prop <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month,df$Created.By),FUN = sum)
      names(df.sum) <- c("year","month","person","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week,df$Created.By),FUN = sum)
      names(df.sum) <- c("year","week","person","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  personal_avg_prop_no <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","Created.By"))
      names(df.sum) <- c("year","month","person","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","Created.By"))
      names(df.sum) <- c("year","week","person","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1) 
  })
  personal_avg_cont <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month,df$Created.By),FUN = sum)
      names(df.sum) <- c("year","month","person","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week,df$Created.By),FUN = sum)
      names(df.sum) <- c("year","week","person","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  personal_avg_cont_no <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month","Created.By"))
      names(df.sum) <- c("year","month","person","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week","Created.By"))
      names(df.sum) <- c("year","week","person","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary & df.sum$person %in% input$RepLevel))
    return(df.sum.1)
  })
  # whole avg level
  whole_avg_leads <- reactive({
    df <- leads()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$freq,by = list(df$year,df$month),FUN = sum)
      names(df.sum) <- c("year","month","leads")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$freq,by = list(df$year,df$week),FUN = sum)
      names(df.sum) <- c("year","week","leads")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary))
    return(df.sum.1)
  })
  whole_avg_so <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,by = list(df$year,df$month),FUN = sum)
      names(df.sum) <- c("year","month","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,by = list(df$year,df$week),FUN = sum)
      names(df.sum) <- c("year","week","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary))
    return(df.sum.1)
  })
  whole_avg_so_no <- reactive({
    df <- sales()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month"))
      names(df.sum) <- c("year","month","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week"))
      names(df.sum) <- c("year","week","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary ))
    return(df.sum.1)
  })
  whole_avg_cont <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month),FUN = sum)
      names(df.sum) <- c("year","month","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week),FUN = sum)
      names(df.sum) <- c("year","week","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary))
    return(df.sum.1)
  })
  whole_avg_cont_no <- reactive({
    df <- contract()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month"))
      names(df.sum) <- c("year","month","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week"))
      names(df.sum) <- c("year","week","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary ))
    return(df.sum.1)
  })
  whole_avg_prop <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$month),FUN = sum)
      names(df.sum) <- c("year","month","amount")
    }else if(input$reportty == "week"){
      df.sum <- aggregate(df$Amount..Net.of.Tax.,by = list(df$year,df$week),FUN = sum)
      names(df.sum) <- c("year","week","amount")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary))
    return(df.sum.1)
  })
  whole_avg_prop_no <- reactive({
    df <- proposal()
    if(input$reportty == "month"){
      df.sum <- count(df,c("year","month"))
      names(df.sum) <- c("year","month","no")
    }else if(input$reportty == "week"){
      df.sum <- count(df,c("year","week"))
      names(df.sum) <- c("year","week","no")
    }
    df.sum.1 <- subset(df.sum,(df.sum$year==input$year_summary ))
    return(df.sum.1)
  })
 
  # show tables in the beginning, for users to check the correctness of the table
  output$salesperson <- renderTable({
    validate(
      need(salesrep() != "","Please Upload sales rep table!")
    )
    salesrep()
  })
  output$leads <- renderTable({
    validate(
      need(leads() != "","Please Upload leads table!")
    )
      head(leads())
    })
  output$sales <- renderTable({
    validate(
      need(sales() != "","Please Upload sales table!")
    )
    head(sales())
    })
  output$contact <- renderTable({
    validate(
      need(contract() != "","Please Upload contract table!")
    )
    head(contract())
  })
  output$proposal <- renderTable({
    validate(
      need(proposal() != "","Please Upload proposal table")
    )
    head(proposal())
  })
  
  # reactive UI
  output$segment <- renderUI({
    selectInput("segment","Segment",choices = as.character(salesrep()$segment))
  })
  output$LeadsGen <- renderUI({
    
    selectInput("LeadsGen","Leads Generator",choices = as.character(salesrep()$sale_rep))
  })
  output$dateRange <- renderUI({
    if(input$plotty == "day"){
      dateRangeInput('dateRange','Choose date range'
                     ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
    }
  })
  output$year <- renderUI({
    if(input$plotty =="week"||input$plotty == "month"){
      selectInput("year","Choose the year you want:",choices = leads()$year)
    }
  })
  
  output$segment_sale <- renderUI({
    selectInput('segment_sale','Segment',choices = as.character(salesrep()$segment))
  })
  output$sales_rep <- renderUI({
    selectInput('sales_rep','Sales Representative',choices = as.character(salesrep()$sale_rep))
  })
  output$dateRange_sale <- renderUI({
    if(input$plotty_sale == "day"){
      dateRangeInput('dateRange_sale','Choose date range'
                     ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
    }
  })
  output$year_sale <- renderUI({
    if(input$plotty_sale == "week"||input$plotty_sale == "month"){
      selectInput("year_sale","Choose the year you want:",choices = sales()$year)
    }
    
  })
  
  output$segment_con <- renderUI({
    selectInput('segment_con', 'Segment',  choices = as.character(salesrep()$segment))
  })
  output$Contract_creator <- renderUI({
    selectInput('Contract_creator', 'Created by', as.character(salesrep()$sale_rep))
  })
  output$dateRange_con <- renderUI({
    if(input$plotty_con == "day"){
      dateRangeInput("dateRange_con",'Choose date range'
                     ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
    }
  })
  output$year_con <- renderUI({
    if(input$plotty_con == "week"||input$plotty_con == "month"){
      selectInput("year_con","Choose the year you want to see:",choices = contract()$year)
    }
  })
  
  output$segment_pro <- renderUI({
    selectInput('segment_pro', 'Segment',  as.character(salesrep()$segment))
  })
  output$Proposal_creator <- renderUI({
    selectInput('Proposal_creator','Created by',as.character(salesrep()$sale_rep))
  })
  output$dateRange_pro <- renderUI({
    if(input$plotty_pro == "day"){
      dateRangeInput('dateRange_pro','Choose date range'
                     ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
    }
  })
  output$year_pro <- renderUI({
    if(input$plotty_pro =="week"||input$plotty_pro == "month"){
      selectInput("year_pro","Choose the year you want:",choices = sort(proposal()$year))
    }
  })
  
  output$year_summary <- renderUI({
    selectInput("year_summary","Choose the year you want:",choices = sort(sales()$year))
  })
  output$selectList <- renderUI({
    validate(
      need(sales() != "","Please Upload sales table!")
    )
    if (input$reportty == "week"){
      selectInput("list",input$reportty,
                  choices = sort(week(sales()$Date.Created)))
    }
    else if(input$reportty == "month"){
      selectInput("list",input$reportty,
                  choices = as.character(sort(month(sales()$Date.Created,
                                                    label = TRUE))))
    }
  })
  output$segLevel <- renderUI({
    selectInput("segLevel","segment",choices = as.character(sales()$segment))})
  output$RepLevel<- renderUI({
    if(input$level == "sales rep"){
      selectInput("RepLevel","Sales Rep",choices = unique(as.character(sales()$Sales.Rep.2) ))
    }
    
  })

  # show tables in each module
  output$LeadsTable <- renderDataTable({

    LeadsTable()
  })
  output$SalesTable <- renderDataTable({
 
    SalesTable()
  })
  output$ContractTable <- renderTable({
    validate(
      need(contract() != "","Please Upload contract table!")
    )
    ContractTable()
  })
  output$ProposalTable <- renderDataTable({
    validate(
      need(proposal() != "","Please Upload proposal table!")
    )
    ProposalTable()
  })
  
  # tables in summary part
  output$leads1 <- renderTable({
    
    if(input$level == "Sales Rep" & input$segLevel == "Corporate"){
      mydf <- data.frame(Leads = c('Total Leads','Team Average','Individual Average'), 
                         No = c(sum(leads1()$freq),
                                mean(team_avg_leads()$leads)/num_corporate(),
                                mean(personal_avg_leads()$leads)), 
                         check.names = FALSE)
    }
    else if(input$level == "Sales Rep" & input$segLevel == "Confex"){
      mydf <- data.frame(Leads = c('Total Leads','Team Average','Individual Average'), 
                         No = c(sum(leads1()$freq),
                                mean(team_avg_leads()$leads)/num_confex(),
                                mean(personal_avg_leads()$leads)), 
                         check.names = FALSE)
    }
    else if(input$level == "Segment"){
      mydf <- data.frame(Leads = c('Total Leads','Team Average','Division Average'), 
                         No = c(sum(leads1()$freq),
                                mean(team_avg_leads()$leads),
                                mean(whole_avg_leads()$leads)), 
                         check.names = FALSE)
    }
    return(mydf)
  },digits=1)
  output$contract1 <- renderTable({
    if(input$level == "Sales Rep" & input$segLevel == "Corporate"){
      mydf <- data.frame(Contract = c('Total Contracts','Team Average','Individual Average'),
                         No = c(nrow(contract1()),
                                mean(team_avg_cont_no()$no)/num_corporate(),
                                mean(personal_avg_cont_no()$no)),
                         Amount = c(sum(contract1()$Amount..Net.of.Tax.),
                                    mean(team_avg_cont()$amount)/num_corporate(),
                                    mean(personal_avg_cont()$amount)),
                         check.names = FALSE)
    }else if(input$level == "Sales rep"& input$segLevel == "Confex"){
      mydf <- data.frame(Contract = c('Total Contracts','Team Average','Individual Average'),
                         No = c(nrow(contract1()),
                                mean(team_avg_cont_no()$no)/num_confex(),
                                mean(personal_avg_cont_no()$no)),
                         Amount = c(sum(contract1()$Amount..Net.of.Tax.),
                                    mean(team_avg_cont()$amount)/num_confex(),
                                    mean(personal_avg_cont()$amount)), check.names = FALSE)
    }
    else if(input$level == "Segment"){
      mydf <- data.frame(Contract = c('Total Contracts','Team Average','Division Average'),
                         No = c(nrow(contract1()),
                                mean(team_avg_cont_no()$no),
                                mean(whole_avg_cont_no()$no)),
                         Amount = c(sum(contract1()$Amount..Net.of.Tax.),
                                    mean(team_avg_cont()$amount),
                                    mean(whole_avg_cont()$amount)), check.names = FALSE)
    }
    mydf$Amount <- round(mydf$Amount,0)
    mydf$Amount <- paste("$",comma(mydf$Amount))
    return(mydf)
  },digits=1)
  output$proposal1 <- renderTable({
    if(input$level == "Sales Rep" & input$segLevel == "Corporate"){
      mydf <- data.frame(Proposal = c('Total Proposals','Team Average','Individual Average'),
                         No = c(nrow(proposal1()),
                                mean(team_avg_prop_no()$no)/num_corporate(),
                                mean(personal_avg_prop_no()$no)),
                         Amount = c(sum(proposal1()$Amount..Net.of.Tax.),
                                    mean(team_avg_cont()$amount)/num_corporate(),
                                    mean(personal_avg_prop()$amount)), 
                         check.names = FALSE)
    }
    else if(input$level == "Sales Rep" & input$segLevel == "Confex"){
      mydf <- data.frame(Proposal = c('Total Proposals','Team Average','Individual Average'),
                         No = c(nrow(proposal1()),
                                mean(team_avg_prop_no()$no)/num_confex(),
                                mean(personal_avg_prop_no()$no)),
                         Amount = c(sum(proposal1()$Amount..Net.of.Tax.),
                                    mean(team_avg_prop()$amount)/num_confex(),
                                    mean(personal_avg_prop()$amount)),
                         check.names = FALSE)
    }
    else if(input$level == "Segment"){
      mydf <- data.frame(Proposal = c('Total Proposals','Team Average','Division Average'),
                         No = c(nrow(proposal1()),
                                mean(team_avg_prop_no()$no),
                                mean(whole_avg_prop_no()$no)),
                         Amount = c(sum(proposal1()$Amount..Net.of.Tax.),
                                    mean(team_avg_prop()$amount),
                                    mean(whole_avg_prop()$amount)),
                         check.names = FALSE)
    }
    mydf$No <- round(mydf$No,0)
    mydf$Amount <- round(mydf$Amount,0)
    mydf$Amount <- paste("$",comma(mydf$Amount))
    return(mydf)
  },digits=1)
  output$so1 <- renderTable({
    if(input$level == "Sales Rep" & input$segLevel == "Corporate"){
      mydf <- data.frame(SalesOrder = c('Total Sales Order','Team Average','Individual Average'),
                         No = c(nrow(so1()),
                                mean(team_avg_so_no()$no)/num_corporate(),
                                mean(personal_avg_so_no()$no)),
                         Amount = c(sum(so1()$Maximum.of.Amount..Net.of.Tax.),
                                    mean(team_avg_so()$amount)/num_corporate(),
                                    mean(personal_avg_so()$amount)),
                         check.names = FALSE)
    }else if (input$level == "Sales Rep" & input$segLevel == "Confex"){
      mydf <- data.frame(SalesOrder = c('Total Sales Order','Team Average','Individual Average'),
                         No = c(nrow(so1()),
                                mean(team_avg_so_no()$no)/num_confex(),
                                mean(personal_avg_so_no()$no)),
                         Amount = c(sum(so1()$Maximum.of.Amount..Net.of.Tax.),
                                    mean(team_avg_so()$amount)/num_confex(),
                                    mean(personal_avg_so()$amount)),
                         check.names = FALSE)
    }
    else if(input$level == "Segment"){
      mydf <- data.frame(SalesOrder = c('Total Sales Order','Team Average','Division Average'),
                         No = c(nrow(so1()),
                                mean(team_avg_so_no()$no),
                                mean(whole_avg_so_no()$no)),
                         Amount = c(sum(so1()$Maximum.of.Amount..Net.of.Tax.),
                                    mean(team_avg_so()$amount),
                                    mean(whole_avg_so()$amount)), 
                         check.names = FALSE)
    }
    mydf$Amount <- round(mydf$Amount,0)
    mydf$Amount <- paste("$",comma(mydf$Amount))
    return(mydf)
  },digits=1)

  # update sales rep based on selected segment
  observe({
    updateSelectInput(session,inputId = "LeadsGen" ,label = 'Leads Generator',
                      choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment]))
  })
  observe({
    updateSelectInput(session, inputId = "sales_rep" ,label = 'sales_rep'
                      ,choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment_sale])) 
  })
  observe({
    updateSelectInput(session, inputId = "Contract_creator",label = "Created by",
                      choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment_con]))
  })
  observe({
    updateSelectInput(session,inputId = "Proposal_creator",label = "Created by",
                      choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment_pro]))
  })
  observe({
    updateSelectInput(session,inputId ="RepLevel" ,
                      label ="Sales Rep" ,
                      choices = unique(as.character(sales()$Sales.Rep.2[sales()$segment == input$segLevel])))
    
  })
  
  # calculate avg level within segment for comparison
  leads_avg_day <- reactive({
    df <- aggregate(leads()$freq,
                    by = list(leads()$year,leads()$Date.Created,leads()$segment),
                    FUN = sum)
    names(df) <- c("Year","Date","Segment","Leads")
    df$Leads[df$Segment == "Confex"] <- df$Leads[df$Segment == "Confex"]/num_confex()
    df$Leads[df$Segment == "Corporate"] <- df$Leads[df$Segment == "Corporate"]/num_corporate()
    return(df)
  })
  leads_avg_week <- reactive({
    leads_avg_week <- aggregate(leads()$freq
              ,by = list(leads()$year,leads()$week,leads()$segment)
              ,FUN=sum)
    names(leads_avg_week) <- c("Year","Week","Segment","Leads")
    leads_avg_week$Leads[leads_avg_week$Segment == "Confex"] <- leads_avg_week$Leads[leads_avg_week$Segment == "Confex"]/num_confex()
    leads_avg_week$Leads[leads_avg_week$Segment == "Corporate"] <- leads_avg_week$Leads[leads_avg_week$Segment == "Corporate"]/num_corporate()
    return(leads_avg_week)
    
  })
  leads_avg_month <- reactive({
    leads_avg_month <- aggregate(leads()$freq,
                                 by = list(leads()$year,leads()$month,leads()$segment),
                                 FUN = sum)
    names(leads_avg_month) <- c("Year","Month","Segment","Leads")
    leads_avg_month$Leads[leads_avg_month$Segment == "Confex"] <- leads_avg_month$Leads[leads_avg_month$Segment == "Confex"]/num_confex()
    leads_avg_month$Leads[leads_avg_month$Segment == "Corporate"] <- leads_avg_month$Leads[leads_avg_month$Segment == "Corporate"]/num_corporate()
    return(leads_avg_month)
  })
  sales_avg_day <- reactive({
    sales_avg_day <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                               ,by = list(sales()$year,sales()$Date.Created,sales()$segment)
                               ,FUN = sum)
    names(sales_avg_day) <- c("Year","Date","Segment","Sales Amount")
    sales_avg_day$`Sales Amount`[sales_avg_day$Segment == "Confex"] <- sales_avg_day$`Sales Amount`[sales_avg_day$Segment == "Confex"]/num_confex()
    sales_avg_day$`Sales Amount`[sales_avg_day$Segment == "Corporate"] <- sales_avg_day$`Sales Amount`[sales_avg_day$Segment == "Corporate"]/num_corporate()
    return (sales_avg_day)
  })
  sales_avg_week <- reactive({
    sales_avg_week <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                                ,by = list(sales()$year,sales()$week,sales()$segment)
                                ,FUN = sum)
    names(sales_avg_week) <- c("Year","Date","Segment","Sales Amount")
    sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Confex"] <- sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Confex"]/num_confex()
    sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Corporate"] <- sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Corporate"]/num_corporate()
    return(sales_avg_week)
  })
  sales_avg_month <- reactive({
    df <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                    ,by = list(sales()$year,sales()$month,sales()$segment)
                    ,FUN = sum)
    names(df) <- c("Year","Date","Segment","Sales Amount")
    df$`Sales Amount`[df$Segment == "Confex"] <- df$`Sales Amount`[df$Segment == "Confex"]/num_confex()
    df$`Sales Amount`[df$Segment == "Corporate"] <- df$`Sales Amount`[df$Segment == "Corporate"]/num_corporate()
    return(df)
  })
  
  output$ContractPlots <- renderPlot({
    validate(
      need(contract() != "","Please Upload contract table!")
    )
    validate(
      need(input$Contract_creator != "","Please select sales rep you want to see")
    )
    
    data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator,"both"))
    contract_sub = subset(data,as.Date(as.character(Date.Created))>= input$dateRange_con[1]&as.Date(as.character(Date.Created)) <= input$dateRange_con[2])
    
    contract.week <- aggregate(data$Amount..Net.of.Tax.,
                               by = list(data$year,data$week),
                               FUN = sum)
    names(contract.week) <- c("year","week","amount")
    contract.week <- subset(contract.week,contract.week$year == input$year_con)
    
    contract.day <- aggregate(contract_sub$Amount..Net.of.Tax.,
                              by = list(contract_sub$year,contract_sub$Date.Created),
                              FUN=sum)
    names(contract.day) <- c("year","day","amount")
    contract.week <- subset(contract.week,contract.week$year == input$year_con)
    
    contract.month <- aggregate(data$Amount..Net.of.Tax.,
                                by = list(data$year,data$month),
                                FUN = sum)
    names(contract.month) <- c("year","month","amount")
    contract.month <- subset(contract.month,contract.month$year == input$year_con)
    
    switch(input$plotty_con,
           "month" = {aesthetics1 = aes(x=contract.month[,2], y=contract.month[,3],group = "week")
           pro_data = contract.month
           xlabtxt = "Month"
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")
           },
           "week" = {aesthetics1 = aes(x=contract.week[,2], y=contract.week[,3],group = "week")
           pro_data = contract.week
           xlabtxt = "Week"
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
             aesthetics1 = aes(x=contract.day[,2], y=contract.day[,3],group = "day")
             pro_data = contract.day
             xlabtxt = "Day"
             plotty = geom_line(aesthetics1,data = contract.day,size = 1.2,colour = "#00CCCC")
           })
    p <- ggplot(data = pro_data,mapping = aesthetics1)+
      plotty+
      geom_point(size = 1.5)+
      ggtitle(paste("Contract Created by",input$Contract_creator))+
      xlab(xlabtxt)+
      ylab("Total Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels = comma)
    return(p)
  })
  output$ProposalPlots <- renderPlot({
    validate(
      need(proposal() != "","Please Upload proposal table!")
    )
    validate(
      need(input$Proposal_creator != "","Please select sales rep you want to see")
    )
    data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator,"both"))
    proposal_sub = subset(data
                          ,as.Date(as.character(Date.Created))>= input$dateRange_pro[1]&as.Date(as.character(Date.Created)) <= input$dateRange_pro[2])
    proposal.month <- aggregate(data$Amount..Net.of.Tax.,
                                by = list(data$year,data$month),FUN = sum)
    names(proposal.month) <- c("year","month","amount")
    proposal.week <- aggregate(data$Amount..Net.of.Tax.,
                               by = list(data$year,data$week),FUN = sum)
    names(proposal.week) <- c("year","week","amount")
    proposal.day <- aggregate(proposal_sub$Amount..Net.of.Tax.,
                              by = list(proposal_sub$Date.Created),FUN=sum)
    names(proposal.day) <- c("date","amount")
    proposal.week <- subset(proposal.week,proposal.week$year %in% input$year_pro)
    proposal.month <- subset(proposal.month,proposal.month$year %in% input$year_pro)
    switch(input$plotty_pro,
           "week" = {
             aesthetics1 = aes(x=proposal.week[,2], y=proposal.week[,3],group = "week")
             pro_data = proposal.week
             xlabtxt = "Week"
             plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           
           "day"  = {
             aesthetics1 = aes(x=proposal.day[,1], y=proposal.day[,2],group = "day")
             pro_data = proposal.day
             xlabtxt = "Day"
             plotty = geom_line(aesthetics1,data = proposal.day,size = 1.2,colour = "#00CCCC")},
           
           "month" = {
             aesthetics1 = aes(x=proposal.month[,2],y=proposal.month[,3],group = "month")
             pro_data = proposal.month
             xlabtxt = "Month"
             plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")
           }
           )
    p <- ggplot(data = pro_data,mapping = aesthetics1)+
      plotty+geom_point(size = 1.5)+
      ggtitle(paste("Proposal Created by",input$Proposal_creator))+
      xlab(xlabtxt)+
      ylab("Total Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels = comma)
    return(p)
  })
  output$LeadsPlot <- renderChart2({
    data <-subset(leads(),trimws(Lead.Generator,"both") %in% trimws(input$LeadsGen,"both"))
    if(input$plotty == "month"){
      data.month <- aggregate(data$freq, by = list(data$year,data$month),FUN = sum)
      names(data.month) <- c("year","month","leads")
      data.month <- subset(data.month,trimws(data.month$year,"both") == trimws(input$year,"both"))
      data.month$avg <- mean(data.month$leads)
      data.month$month <- as.character(sort(data.month$month))
      leads_avg_month <- subset(leads_avg_month(),trimws(Segment,"both") ==trimws(input$segment,"both"))
      leads_avg_month <- subset(leads_avg_month,leads_avg_month$Year == input$year)
      P <- dPlot(x = "month", y = "leads", data = data.month, type = 'bar',groups="Total Leads")
      P$xAxis(orderRule = data.month$month)
      
      if(input$avg_line){
        P$layer(x="Month",y="Leads",data=leads_avg_month, type='line',groups = "Team Average")
      }
      if(input$avg_self){
        P$layer(x="month",y= "avg",data = data.month,type='line',groups = "Individual Average")
      }
      P$legend(x = 60, y = 10, width = 620, height = 20,
               horizontalAlign = "right")
      P$yAxis('No of Leads',overrideMin = 0, overrideMax = max(max(leads_avg_month$Leads),max(data.month$avg),max(data.month$leads)))
      return(P)
    }
    else if(input$plotty == "week"){
      data.week <- aggregate(data$freq, by = list(data$year,data$week),FUN = sum)
      names(data.week) <- c("year","week","leads")
      data.week <- subset(data.week,trimws(data.week$year,"both") == trimws(input$year,"both"))
      data.week$avg <- mean(data.week$leads)
      data.week$week <- as.character(sort(data.week$week))
      leads_avg_week <- subset(leads_avg_week(),trimws(Segment,"both") %in% trimws(input$segment,"both"))
      leads_avg_week <- subset(leads_avg_week,leads_avg_week$Year == input$year)
      P <- dPlot(x = "week", y = "leads", data = data.week, type = 'bar',groups="Total Leads")
      P$xAxis(orderRule = data.week$week)
      if(input$avg_line){
        P$layer(x="Week",y="Leads",data=leads_avg_week, type='line',groups = "Team Average")  
      }
      if(input$avg_self){
        P$layer(x="week",y= "avg",data = data.week,type='line',groups = "Individual Average")
      }
      P$legend(x = 60, y = 10, width = 620, height = 20,
               horizontalAlign = "right")
      P$yAxis('No of Leads',overrideMin = 0, overrideMax = max(max(leads_avg_week$Leads),max(data.week$avg),max(data.week$leads)))
      return(P)
    }
    else if(input$plotty =="day"){
      leads_sub <- subset(data,as.Date(Date.Created) >= input$dateRange[1]&as.Date(Date.Created) <= input$dateRange[2])
      leads_sub$avg <- mean(leads_sub$freq)##argument is not numeric or logical
      leads_avg_day <- subset(leads_avg_day(),trimws(Segment,"both") == trimws(input$segment,"both") )
      leads_avg_day <- subset(leads_avg_day,as.Date(Date) >= input$dateRange[1]&as.Date(Date) <= input$dateRange[2])
      P <- dPlot(x = "Date.Created",y = "freq",data= leads_sub,type = "line",groups = "Total Leads")
      if(input$avg_line){
        P$layer(x="Date",y="Leads", data = leads_avg_day, type = 'line',groups = "Team Average")  
      }
      if(input$avg_self){
        P$layer(x ="Date.Created",y = "avg",data=leads_sub,type ="line",groups = "Individual Average")
      }
      P$legend(x = 60, y = 10, width = 620, height = 20,
               horizontalAlign = "right")
    }
    P$yAxis('No of Leads',overrideMin = 0, overrideMax = max(max(leads_avg_day$Leads),max(leads_sub$avg),max(leads_sub$freq)))
    return(P)
  })
  output$SalesPlot <- renderChart2({
    data <- subset(sales(),trimws(Sales.Rep.2,"both") %in% trimws(input$sales_rep,"both"))
    if(input$plotty_sale =="month"){
      sales.month <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$year,data$month),FUN = sum)
      names(sales.month) <- c("Year","Month","Amount")
      sales.month.no <- aggregate(data$SO.Number,by =list(data$year,data$month),FUN = length) 
      sales.month$No <- sales.month.no[,3]
      sales.month <-subset(sales.month,trimws(sales.month$Year,"both") %in% trimws(input$year_sale,"both"))
      sales_avg_month <- subset(sales_avg_month(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
      sales_avg_month <- subset(sales_avg_month,trimws(sales_avg_month$Year,"both") %in% trimws(input$year_sale,"both"))
      sales.month$avg_amount <- mean(sales.month$Amount)
      h <- Highcharts$new()
      h$xAxis(categories = sales.month$Month)
      h$yAxis(list(list(title = list(text = 'Sales Amount'))
                   , list(title = list(text = 'Sales No'), opposite = TRUE)
      )
      )
      h$series(name = 'Sales Amount', type = 'column', color = '#4572A7',
               data = sales.month$Amount)
      h$series(name = 'Sales No', type = 'scatter', color = '#89A54E',
               data = sales.month$No,
               yAxis = 1)
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_month$`Sales Amount`)
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.month$avg_amount)      
      }
      return(h)

    }
    else if(input$plotty_sale == "week"){
      sales.week <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$year,data$week),FUN = sum)
      names(sales.week) <- c("Year","Week","Amount")
      sales.week.no <- aggregate(data$SO.Number,by =list(data$year,data$week),FUN = length)
      sales.week$No <- sales.week.no[,3]
      sales.week <- subset(sales.week,trimws(sales.week$Year,"both") %in% trimws(input$year_sale,"both"))
      sales_avg_week <- subset(sales_avg_week(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
      sales_avg_week <- subset(sales_avg_week,trimws(sales_avg_week$Year,"both") %in% trimws(input$year_sale,"both"))
      sales.week$avg_amount <- mean(sales.week$Amount)
      h <- Highcharts$new()
      h$xAxis(categories = sales.week$Week)
      h$yAxis(list(list(title = list(text = 'Sales Amount'))
                   , list(title = list(text = 'Sales No'), opposite = TRUE)
      )
      )
      h$series(name = 'Sales Amount', type = 'column', color = '#4572A7',
               data = sales.week$Amount)
      h$series(name = 'Sales No', type = 'scatter', color = '#89A54E',
               data = sales.week$No,
               yAxis = 1)
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_week$`Sales Amount`)
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.week$avg_amount)      
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      return(h)
    }
    else if(input$plotty_sale == "day"){
      sales <- subset(data,as.Date(Date.Created) >= input$dateRange_sale[1]&as.Date(Date.Created) <= input$dateRange[2])
      sales.day <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.,by = list(sales$Date.Created),FUN = sum)
      names(sales.day) <- c("Date","Amount")
      sales.day.no <- aggregate(sales$SO.Number,by = list(sales$Date.Created),FUN = length)
      sales.day$No <- sales.day.no[,2]
      sales_avg_day <- subset(sales_avg_day(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
      sales_avg_day <- subset(sales_avg_day,
                              as.Date(Date) >= input$dateRange_sale[1]&as.Date(Date) <= input$dateRange_sale[2])
      sales.day$avg_amount <- mean(sales.day$Amount)
      h <- Highcharts$new()
      h$xAxis(categories = sales.day$Date)
      h$yAxis(list(list(title = list(text = 'Sales Amount'))
                   , list(title = list(text = 'Sales No'), opposite = TRUE)
      )
      )
      h$series(name = 'Sales Amount', type = 'spline', color = '#4572A7',
               data = sales.day$Amount,groups = "Total Sales Amount")
      h$series(name = 'Sales No', type = 'scatter', color = '#89A54E',data = sales.day$No,yAxis = 1)
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_day$`Sales Amount`)
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.day$avg_amount)      
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      return(h)
    }
  })
  # Title in the summary part
  output$title <- renderText(
    if (input$reportty=="month" ){
      if(input$level == "Segment"){
        paste(paste(input$year_summary,input$list,"Summary Report by",input$level,sep = " "),
              input$segLevel,sep=":")  
      }
      else if (input$level == "Sales Rep"){
        paste(paste(input$year_summary,input$list,"Summary Report by",input$level,sep = " "),
              input$RepLevel,sep=":")
      }
    }else if (input$reportty =="week"){
      validate(need(input$list !="","select week"))
      if(input$level == "Segment"){
        paste(paste(input$year_summary,"Week",input$list,
                    "(",date_in_week(as.numeric(input$year_summary),as.numeric(input$list)),")",
                    "Summary Report by",input$level,sep = " "),input$segLevel,sep=":")  
      }
      else if (input$level == "Sales Rep"){
        paste(paste(input$year_summary,"Week",input$list,
                    "(",date_in_week(as.numeric(input$year_summary),as.numeric(input$list)),")",
                    "Summary Report by",input$level,sep = " "),input$RepLevel,sep=":")
      } 
    }
    )
 
  # download all the tables
  output$downloadLeads <- downloadHandler(
    filename = function(){
      paste('Leads','.csv',sep='')
    },
    content = function(file){
      write.csv(LeadsTable(),file)
    }
  )
  output$downloadSales <- downloadHandler(
    filename = function(){
      paste('sales','.csv',sep='')
    },
    content = function(file){
      write.csv(SalesTable(),file)
    }
  )
  output$downloadContract <- downloadHandler(
    filename = function(){
      paste('Contract','.csv',sep='')
    },
    content = function(file){
      write.csv(ContractTable(),file)
    }
  )
  output$downloadProposal <- downloadHandler(
    filename = function(){
      paste('Proposal','.csv',sep='')
    },
    content = function(file){
      write.csv(ProposalTable(),file)
    }
  )
 })