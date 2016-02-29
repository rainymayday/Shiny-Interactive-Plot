options(java.parameters = "-Xmx8g" )
library(shiny)
library(plyr)
library(scales)
require(lubridate)
library(ISOweek)
require(XLConnect)
require(rCharts)
library(DT)

date_in_week <- function(year, week, weekday=7){
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  return(paste(ISOweek2date(w),ISOweek2date(w)+7,sep = " ~ "))
}
factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
}
merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by=c("Created.By","Segment"))
}
shinyServer(function(input, output,session) {
  salesrep <- reactive({
    infile <- input$file0
    if (is.null(infile)) {
      return(NULL)
    }
    wb <- XLConnect::loadWorkbook(infile$datapath)
    df <- XLConnect::readWorksheet(wb,sheet = 1)
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
        wb <- XLConnect::loadWorkbook(infile$datapath)
        leads <- XLConnect::readWorksheet(wb,sheet = 1)
        leads$Date.Created <- as.character(as.Date(leads$Date.Created, "%d/%m/%Y"))
        leads <- merge(leads, salesrep(),by.x= "Created.By",by.y = "sale_rep")
        leads <- aggregate(leads$Internal.ID,by = list(leads$Date.Create,leads$Created.By,leads$segment),
                                          FUN = length)
        names(leads) <- c("Date.Created","Created.By","segment","freq")
        leads$Created.By <- as.factor(leads$Created.By)
        leads$week <- week(leads$Date.Created)
        leads$month <- lubridate::month(leads$Date.Created,label = TRUE)
        leads$year <- year(leads$Date.Created)
      }
    )
    return(leads)
  })
  sales <- reactive({
    infile <- input$file2
    if (is.null(infile)){
      return(NULL)
    }
    isolate({
      wb <- XLConnect::loadWorkbook(infile$datapath)
      sales <- XLConnect::readWorksheet(wb,sheet = 1)
      sales <- subset(sales,sales$Pro...Created.By %in% as.character(salesrep()$sale_rep))
      col <- c("SO.Number","Date.Created","Name","Event.Name"
               ,"Maximum.of.Amount..Net.of.Tax.","Pro...Created.By")
      sales <- sales[,col]
      sales$Maximum.of.Amount..Net.of.Tax. <- factor2numeric(sales$Maximum.of.Amount..Net.of.Tax.)
      sales <- merge(sales, salesrep(),by.x= "Pro...Created.By",by.y = "sale_rep")
      sales$Date.Created <- as.character(as.Date(sales$Date.Created, "%d/%m/%Y",origin="1970-01-01"))
      sales$week <- lubridate::week(sales$Date.Created)
      sales$month <- lubridate::month(sales$Date.Created,label = TRUE)
      sales$year <- lubridate::year(sales$Date.Created)
    })
    return(sales)
  })
  proposal <- reactive({
    infile <- input$file4
    if (is.null(infile)){
      return(NULL)
    }
    isolate({
      wb <- XLConnect::loadWorkbook(infile$datapath)
      proposal <- XLConnect::readWorksheet(wb,sheet = 1)
      proposal <- proposal[!duplicated(proposal),]
      cols <- c("Internal.ID","Date.Created","Name","Created.By"
                ,"Amount..Net.of.Tax.","Contract.Sent.Date")
      proposal <- proposal[,cols]
      proposal$Amount..Net.of.Tax. <- factor2numeric(proposal$Amount..Net.of.Tax.)
      proposal <- subset(proposal,proposal$Created.By %in% as.character(salesrep()$sale_rep))
      proposal$Date.Created <- as.character(as.Date(proposal$Date.Created, "%d/%m/%Y",origin="1970-01-01"))
      proposal <- merge(proposal, salesrep(),by.x= "Created.By",by.y = "sale_rep")
      proposal$week <- week(proposal$Date.Created)
      proposal$month <- lubridate::month(proposal$Date.Created,label = TRUE)
      proposal$year <- year(proposal$Date.Created)
    })
    return (proposal)
  })
  contract <- reactive({
    contract <- proposal()
    contract <- subset(contract,contract$Contract.Sent.Date!="")
    contract <- subset(contract,contract$Created.By %in% as.character(salesrep()$sale_rep))
    col <- c("Internal.ID","Date.Created","Created.By","Name","Amount..Net.of.Tax.","year","month","week","segment")
    contract <- contract[,col]
    return (contract)
  })
  # generate summary tables
  LeadsTable <- reactive({
    validate(
      need(leads() != "","Please Upload leads table!")
    )
    LeadsTable <- aggregate(leads()$freq,by = list(leads()$Created.By,leads()$segment),FUN = sum)
    names(LeadsTable) <- c("Created.By","Segment","No_of_Leads")
    return(LeadsTable)
  })
  SalesTable <- reactive({
    validate(
      need(sales() != "","Please Upload sales table!")
    )
    SalesTable <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                            ,by = list(sales()$Pro...Created.By,sales()$segment),FUN = sum)
    names(SalesTable) <- c("Created.By","Segment","Sales_Amount")
    SalesTable_No <- aggregate(sales()$Event.Name
                               ,by = list(sales()$Pro...Created.By,sales()$segment),FUN = length)
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
    names(contractTable) <- c("Created.By","Segment","Contract_Amount")
    contractTable_No <- aggregate(contract()$Internal.ID
                                  ,by = list(contract()$Created.By,contract()$segment),FUN=length)
    contractTable$No_Of_Contract <- contractTable_No[,3]
    contractTable$Contract_Amount <- paste("$",comma(contractTable$Contract_Amount))
    return(contractTable)
  })
  ProposalTable <- reactive({
    validate(
      need(proposal() != "","Please Upload proposal table")
    )
    ProposalTable <- aggregate(proposal()$Amount..Net.of.Tax.,by = list(proposal()$Created.By,proposal()$segment),FUN = sum)
    names(ProposalTable) <- c("Created.By","Segment","Proposal_Amount")
    ProposalTable_No <- aggregate(proposal()$Internal.ID,by = list(proposal()$Created.By,proposal()$segment),FUN = length)
    ProposalTable$No_of_Proposal <- ProposalTable_No[,3]
    ProposalTable$Proposal_Amount <- paste("$",comma(ProposalTable$Proposal_Amount))
    return(ProposalTable)
  })
  SummaryTable <- reactive({
    DataList <- list(LeadsTable(),SalesTable(),ContractTable(),ProposalTable())
    output <- Reduce(merge.all, DataList)
    return(output)
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
  
  output$proposal <- renderTable({
    validate(
      need(proposal() != "","Please Upload proposal table")
    )
    head(proposal())
  })
  output$contact <- renderTable({
    validate(
      need(contract() != "","Please Upload contract table!")
    )
    head(contract())
  })
  # reactive UI
  output$segment <- renderUI({
    selectInput("segment","Segment",choices = as.character(salesrep()$segment),selected = 1)
  })
  output$LeadsGen <- renderUI({
    
    selectInput("LeadsGen","Leads Generator",choices = as.character(salesrep()$sale_rep))
  })
  output$dateRange <- renderUI({
    if(input$plotty == "day"){
      dateRangeInput('dateRange','Choose date range'
                     ,start = Sys.Date()-30, end = Sys.Date())
    }
  })
  output$year <- renderUI({
    if(input$plotty =="week"||input$plotty == "month"){
      selectInput("year","Choose the year you want:",choices = leads()$year,selected = 1)
    }
  })
  output$LeadsGen1 <- renderUI({
    if(input$compare_rep){
      selectInput("LeadsGen1","Leads Generator to compare:",
                  choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment]))
    }
  })
  
  output$segment_sale <- renderUI({
    selectInput('segment_sale','Segment',choices = as.character(salesrep()$segment),selected = 1)
  })
  output$sales_rep <- renderUI({
    selectInput('sales_rep','Sales Representative',choices = as.character(salesrep()$sale_rep))
  })
  output$sales_rep1 <- renderUI({
    if(input$com_so){
      selectInput("sales_rep1",'Sales Representative to Compare:'
                  ,choices = as.character(salesrep()$sale_rep[salesrep()$segment==input$segment_sale]))
    }
  })
  output$dateRange_sale <- renderUI({
    if(input$plotty_sale == "day"){
      dateRangeInput('dateRange_sale','Choose date range'
                     ,start = Sys.Date()-30, end = Sys.Date())
    }
  })
  output$year_sale <- renderUI({
    if(input$plotty_sale == "week"||input$plotty_sale == "month"){
      selectInput("year_sale","Choose the year you want:",choices = sort(sales()$year),selected = 1)
    }
    
  })
  
  output$segment_con <- renderUI({
    selectInput('segment_con', 'Segment',  choices = as.character(salesrep()$segment),selected = 1)
  })
  output$Contract_creator <- renderUI({
    selectInput('Contract_creator', 'Created by', as.character(salesrep()$sale_rep))
  })
  output$Contract_creator1 <- renderUI({
    if(input$com_con){
      selectInput('Contract_creator1', 'Contract Creator to compare:', as.character(salesrep()$sale_rep[salesrep()$segment%in%input$segment_con])) 
    }
  })
  output$dateRange_con <- renderUI({
    if(input$plotty_con == "day"){
      dateRangeInput("dateRange_con",'Choose date range'
                     ,start = Sys.Date()-30, end = Sys.Date())
    }
  })
  output$year_con <- renderUI({
    if(input$plotty_con == "week"||input$plotty_con == "month"){
      selectInput("year_con","Choose the year you want to see:",choices = contract()$year,selected = 1)
    }
  })
  
  output$segment_pro <- renderUI({
    selectInput('segment_pro', 'Segment',  as.character(salesrep()$segment),selected = 1)
  })
  output$Proposal_creator <- renderUI({
    selectInput('Proposal_creator','Created by',as.character(salesrep()$sale_rep))
  })
  output$Proposal_creator1 <- renderUI({
    if(input$comp_pro){
      selectInput('Proposal_creator1','Proposal Creator to compare:',as.character(salesrep()$sale_rep[salesrep()$segment%in%input$segment_pro]))
    }
    
  })
  output$dateRange_pro <- renderUI({
    if(input$plotty_pro == "day"){
      dateRangeInput('dateRange_pro','Choose date range'
                     ,start = Sys.Date()-30, end = Sys.Date())
    }
  })
  output$year_pro <- renderUI({
    if(input$plotty_pro =="week"||input$plotty_pro == "month"){
      selectInput("year_pro","Choose the year you want:",choices = sort(proposal()$year),selected = 1)
    }
  })
  
  output$year_summary <- renderUI({
    selectInput("year_summary","Choose the year you want:",choices = sort(sales()$year),selected = 1)
  })
  
  output$segLevel <- renderUI({
    selectInput("segLevel","segment",choices = as.character(salesrep()$segment),selected = 1)})
 
  output$SummaryTable <- DT::renderDataTable(
    SummaryTable(), options = list(lengthChange = FALSE)
  )

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
    names(sales_avg_week) <- c("Year","Week","Segment","Sales Amount")
    sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Confex"] <- sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Confex"]/num_confex()
    sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Corporate"] <- sales_avg_week$`Sales Amount`[sales_avg_week$Segment == "Corporate"]/num_corporate()
    return(sales_avg_week)
  })
  sales_avg_month <- reactive({
    df <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                    ,by = list(sales()$year,sales()$month,sales()$segment)
                    ,FUN = sum)
    names(df) <- c("Year","Month","Segment","Sales Amount")
    df$`Sales Amount`[df$Segment == "Confex"] <- df$`Sales Amount`[df$Segment == "Confex"]/num_confex()
    df$`Sales Amount`[df$Segment == "Corporate"] <- df$`Sales Amount`[df$Segment == "Corporate"]/num_corporate()
    return(df)
  })
  contract_avg_day <- reactive({
    contract_avg_day <- aggregate(contract()$Amount..Net.of.Tax.,
                                  by =list(contract()$year,contract()$Date.Created,contract()$segment),
                                  FUN = sum)
    names(contract_avg_day) <- c("Year","Date","Segment","Contract Amount")
    contract_avg_day$`Contract Amount`[contract_avg_day$Segment =="Confex"] <- contract_avg_day$`Contract Amount`[contract_avg_day$Segment =="Confex"]/num_confex()
    contract_avg_day$`Contract Amount`[contract_avg_day$Segment =="Corporate"] <- contract_avg_day$`Contract Amount`[contract_avg_day$Segment =="Corporate"]/num_confex()
    return(contract_avg_day)
  })
  contract_avg_week <- reactive({
    contract_avg_week <- aggregate(contract()$Amount..Net.of.Tax.,
                                  by =list(contract()$year,contract()$week,contract()$segment),
                                  FUN = sum)
    names(contract_avg_week) <- c("Year","Week","Segment","Contract Amount")
    contract_avg_week$`Contract Amount`[contract_avg_week$Segment =="Confex"] <- contract_avg_week$`Contract Amount`[contract_avg_week$Segment =="Confex"]/num_confex()
    contract_avg_week$`Contract Amount`[contract_avg_week$Segment =="Corporate"] <- contract_avg_week$`Contract Amount`[contract_avg_week$Segment =="Corporate"]/num_confex()
    return(contract_avg_week)
  })
  contract_avg_month <- reactive({
    contract_avg_month <- aggregate(contract()$Amount..Net.of.Tax.,
                                   by =list(contract()$year,contract()$month,contract()$segment),
                                   FUN = sum)
    names(contract_avg_month) <- c("Year","Month","Segment","Contract Amount")
    contract_avg_month$`Contract Amount`[contract_avg_month$Segment =="Confex"] <- contract_avg_month$`Contract Amount`[contract_avg_month$Segment =="Confex"]/num_confex()
    contract_avg_month$`Contract Amount`[contract_avg_month$Segment =="Corporate"] <- contract_avg_month$`Contract Amount`[contract_avg_month$Segment =="Corporate"]/num_confex()
    return(contract_avg_month)
  })
  proposal_avg_day <- reactive({
    proposal_avg_day <- aggregate(proposal()$Amount..Net.of.Tax.,
                                  by =list(proposal()$year,proposal()$Date.Created,proposal()$segment),
                                  FUN = sum)
    names(proposal_avg_day) <- c("Year","Date","Segment","Proposal Amount")
    proposal_avg_day$`Proposal Amount`[proposal_avg_day$Segment =="Confex"] <- proposal_avg_day$`Proposal Amount`[proposal_avg_day$Segment =="Confex"]/num_confex()
    proposal_avg_day$`Proposal Amount`[proposal_avg_day$Segment =="Corporate"] <- proposal_avg_day$`Proposal Amount`[proposal_avg_day$Segment =="Corporate"]/num_confex()
    return(proposal_avg_day)
  })
  proposal_avg_week <- reactive({
    proposal_avg_week <- aggregate(proposal()$Amount..Net.of.Tax.,
                                  by =list(proposal()$year,proposal()$week,proposal()$segment),
                                  FUN = sum)
    names(proposal_avg_week) <- c("Year","Week","Segment","Proposal Amount")
    proposal_avg_week$`Proposal Amount`[proposal_avg_week$Segment =="Confex"] <- proposal_avg_week$`Proposal Amount`[proposal_avg_week$Segment =="Confex"]/num_confex()
    proposal_avg_week$`Proposal Amount`[proposal_avg_week$Segment =="Corporate"] <- proposal_avg_week$`Proposal Amount`[proposal_avg_week$Segment =="Corporate"]/num_confex()
    return(proposal_avg_week)
  })
  proposal_avg_month <- reactive({
    proposal_avg_month <- aggregate(proposal()$Amount..Net.of.Tax.,
                                   by =list(proposal()$year,proposal()$month,proposal()$segment),
                                   FUN = sum)
    names(proposal_avg_month) <- c("Year","Month","Segment","Proposal Amount")
    proposal_avg_month$`Proposal Amount`[proposal_avg_month$Segment =="Confex"] <- proposal_avg_month$`Proposal Amount`[proposal_avg_month$Segment =="Confex"]/num_confex()
    proposal_avg_month$`Proposal Amount`[proposal_avg_month$Segment =="Corporate"] <- proposal_avg_month$`Proposal Amount`[proposal_avg_month$Segment =="Corporate"]/num_confex()
    return(proposal_avg_month)
  })
  
  output$LeadsPlot <- renderChart2({
    data <-subset(leads(),trimws(Created.By,"both") %in% trimws(input$LeadsGen,"both"))
    if(input$plotty == "month"){
      data.month <- aggregate(data$freq, by = list(data$year,data$month),FUN = sum)
      names(data.month) <- c("year","month","leads")
      data.month <- subset(data.month,trimws(data.month$year,"both") == trimws(input$year,"both"))
      data.month$month <- as.character(sort(data.month$month))
      leads_avg_month <- subset(leads_avg_month(),trimws(Segment,"both") ==trimws(input$segment,"both"))
      leads_avg_month <- subset(leads_avg_month,leads_avg_month$Year == input$year)
      leads_avg_month <- subset(leads_avg_month,leads_avg_month$Month %in% data.month$month)
      h <- Highcharts$new()
      h$xAxis(categories = data.month$month,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(title = list(text = 'Total No')))
      h$series(name = 'Total No', type = 'column', color = '#4572A7',
               data = data.month$leads)
      h$plotOptions(column = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = leads_avg_month$Leads,dashStyle = "Dash")
      }
      if(input$avg_self){
        h$yAxis(title = list(text = 'Total No'),
                plotLines = list(list(
                  value = mean(data.month$leads),
                  color = '#ff0000',
                  width = 2,
                  zIndex = 4,
                  label = list(text = paste('mean',round(mean(data.month$leads),2),sep = ':'),
                               style = list( color = '#ff0000', fontWeight = 'bold' )
                  ))))
      }
      if(input$compare_rep){
        data <-subset(leads(),trimws(Created.By,"both") %in% trimws(input$LeadsGen1,"both"))
        data.month <- aggregate(data$freq, by = list(data$year,data$month),FUN = sum)
        names(data.month) <- c("year","month","leads")
        data.month <- subset(data.month,trimws(data.month$year,"both") == trimws(input$year,"both"))
        h$series(name = input$LeadsGen1, type = 'column', color = 'purple',
                 data = data.month$leads)
      }
      
      h$title(text = paste("Leads Generated by ",input$LeadsGen,"Show By Month"))
      # try to show plots inline
      h$show("inline")
      return(h)
    }
    else if(input$plotty == "week"){
      data.week <- aggregate(data$freq, by = list(data$year,data$week),FUN = sum)
      names(data.week) <- c("year","week","leads")
      data.week <- subset(data.week,trimws(data.week$year,"both") == trimws(input$year,"both"))
      leads_avg_week <- subset(leads_avg_week(),trimws(Segment,"both") %in% trimws(input$segment,"both"))
      leads_avg_week <- subset(leads_avg_week,leads_avg_week$Year == input$year)
      leads_avg_week <- subset(leads_avg_week,leads_avg_week$Week %in% data.week$week)
      h <- Highcharts$new()
      h$xAxis(categories = unique(data.week$week),labels = list(rotation = 90, align = "left"))
      h$yAxis(title = list(text = 'Total No'))

      h$series(name = 'Total No', type = 'column', color = '#4572A7',
               data = data.week$leads)
      h$plotOptions(column = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = leads_avg_week$Leads,dashStyle = "Dash")
      }
      if(input$avg_self){
        h$yAxis(title = list(text = 'Total No'),
                plotLines = list(list(
                  value = mean(data.week$leads),
                  color = '#ff0000',
                  width = 2,
                  zIndex = 4,
                  label = list(text = paste('mean',round(mean(data.week$leads),2),sep = ':'),
                               style = list( color = '#ff0000', fontWeight = 'bold' )
                  ))))      
      }
      if(input$compare_rep){
        data <-subset(leads(),trimws(Created.By,"both") %in% trimws(input$LeadsGen1,"both"))
        data.week <- aggregate(data$freq, by = list(data$year,data$week),FUN = sum)
        names(data.week) <- c("year","week","leads")
        data.week <- subset(data.week,trimws(data.week$year,"both") == trimws(input$year,"both"))
        h$series(name = input$LeadsGen1, type = 'column', color = 'purple',
                 data = data.week$leads)
      }
      h$title(text = paste("Leads Generated by ",input$LeadsGen,"Show By Week"))
      return(h)
    }
    else if(input$plotty =="day"){
      leads_sub <- subset(data,as.Date(Date.Created) >= input$dateRange[1]&as.Date(Date.Created) <= input$dateRange[2])
      leads_avg_day <- subset(leads_avg_day(),trimws(Segment,"both") == trimws(input$segment,"both") )
      leads_avg_day <- subset(leads_avg_day,leads_avg_day$Date %in% leads_sub$Date.Created)
      h <- Highcharts$new()
      h$xAxis(categories = unique(leads_sub$Date.Created),labels = list(rotation = 90, align = "left"))
      h$yAxis(list(title = list(text = 'Total No')))
      h$series(name = 'Total No', type = 'spline', color = '#4572A7',
               data = leads_sub$freq)
      if (input$avg_line){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = leads_avg_day$Leads,dashStyle = "Dash")
      }
      if(input$avg_self){
        
        h$yAxis(title = list(text = 'Total No'),
                plotLines = list(list(
                  value = mean(leads_sub$freq),
                  color = '#ff0000',
                  width = 2,
                  zIndex = 4,
                  label = list(text = paste('mean',round(mean(leads_sub$freq),2),sep = ':'),
                               style = list( color = '#ff0000', fontWeight = 'bold' )
                  )))) 
      
      }
      if(input$compare_rep){
        data <-subset(leads(),trimws(Created.By,"both") %in% trimws(input$LeadsGen1,"both"))
        leads_sub <- subset(data,as.Date(Date.Created) >= input$dateRange[1]&as.Date(Date.Created) <= input$dateRange[2])
        h$series(name = input$LeadsGen1, type = 'spline', color = 'purple',data = leads_sub$freq,dashStyle = "Dash")
      }
      h$title(text = paste("Leads Generated by ",input$LeadsGen,"Show By Day"))
      return(h)
    }
  })
  output$ContractPlots <- renderChart2({
    data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator,"both"))
    if(input$plotty_con =="month"){
      contract.month <- aggregate(data$Amount..Net.of.Tax.,
                                  by = list(data$year,data$month),
                                  FUN = sum)
      names(contract.month) <- c("year","month","amount")
      contract.month.no <-aggregate(data$Internal.ID,by =list(data$year,data$month),FUN = length)
      contract.month$No <- contract.month.no[,3]
      contract.month <- subset(contract.month,contract.month$year == input$year_con)
      contract_avg_month <- subset(contract_avg_month(),trimws(Segment,"both") %in% trimws(input$segment_con,"both"))
      contract_avg_month <- subset(contract_avg_month,trimws(contract_avg_month$Year,"both") %in% trimws(input$year_con,"both"))
      contract_avg_month <- subset(contract_avg_month,contract_avg_month$Month %in% contract.month$month)
      contract.month$avg_amount <- mean(contract.month$amount)
      h <- Highcharts$new()
      h$xAxis(categories = contract.month$month,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = input$Contract_creator, type = 'column',color = '#4572A7',
               data = contract.month$amount)
      
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_con){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = contract_avg_month$`Contract Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_con){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = contract.month$avg_amount,dashStyle = "Solid")      
      }
      if(input$contract_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = contract.month$No,
                 yAxis = 1)
      }
      if(input$com_con){
        data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator1,"both"))
        contract.month <- aggregate(data$Amount..Net.of.Tax.,
                                    by = list(data$year,data$month),
                                    FUN = sum)
        names(contract.month) <- c("year","month","amount")
        contract.month <- subset(contract.month,contract.month$year == input$year_con)
        h$series(name = input$Contract_creator1, type = 'column',color = 'purple',
                 data = contract.month$amount,dashStyle = "Dash")
      }

      h$title(text = paste("Contract Generated by ",input$Contract_creator,"Show By Month"))
      return(h)
    }
    else if(input$plotty_con =="week"){
      contract.week <- aggregate(data$Amount..Net.of.Tax.,
                                 by = list(data$year,data$week),
                                 FUN = sum)
      names(contract.week) <- c("year","week","amount")
      contract.week.no <-aggregate(data$Internal.ID,by =list(data$year,data$week),FUN = length)
      contract.week$No <- contract.week.no[,3]
      contract.week <- subset(contract.week,contract.week$year == input$year_con)
      contract.week$avg_amount <- mean(contract.week$amount)
      contract_avg_week <- subset(contract_avg_week(),trimws(Segment,"both") %in% trimws(input$segment_con,"both"))
      contract_avg_week <- subset(contract_avg_week,trimws(contract_avg_week$Year,"both") %in% trimws(input$year_con,"both"))
      contract_avg_week <- subset(contract_avg_week,contract_avg_week$Week %in% contract.week$week)
      h <- Highcharts$new()
      h$xAxis(categories = contract.week$week,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'column', color = '#4572A7',
               data = contract.week$amount)
      
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_con){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = contract_avg_week$`Contract Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_con){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = contract.week$avg_amount,dashStyle = "Solid")      
      }
      if(input$contract_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = contract.week$No,
                 yAxis = 1)
      }
      if(input$com_con){
        data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator1,"both"))
        contract.week <- aggregate(data$Amount..Net.of.Tax.,
                                   by = list(data$year,data$week),
                                   FUN = sum)
        names(contract.week) <- c("year","week","amount")
        contract.week <- subset(contract.week,contract.week$year == input$year_con)
        h$series(name = input$Contract_creator1, type = 'column',
                 data = contract.week$amount,color = "purple",dashStyle = "Dash")
      }
      h$title(text = paste("Contract Generated by ",input$Contract_creator,"Show By Week"))
      return(h)
    }
    else if(input$plotty_con == "day"){
      contract_sub <- subset(data,as.Date(as.character(Date.Created))>= input$dateRange_con[1]&as.Date(as.character(Date.Created)) <= input$dateRange_con[2])
      contract.day <- aggregate(contract_sub$Amount..Net.of.Tax.,
                                by = list(contract_sub$year,contract_sub$Date.Created),
                                FUN=sum)
      names(contract.day) <- c("year","day","amount")
      contract.day.no <- aggregate(contract_sub$Internal.ID,by = list(contract_sub$Date.Created),FUN = length)
      contract.day$No <- contract.day.no[,2]
      contract_avg_day <- subset(contract_avg_day(),trimws(Segment,"both") %in% trimws(input$segment_con,"both"))
      contract.day$avg_amount <- mean(contract.day$amount)
      contract_avg_day <- subset(contract_avg_day,contract_avg_day$Date %in% contract.day$day)
      h <- Highcharts$new()
      h$xAxis(categories = contract.day$day,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'spline', color = '#4572A7',
               data = contract.day$amount)
      
      if (input$avg_line_con){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = contract_avg_day$`Contract Amount`,dashStyle = "Dash")
      }
      if(input$avg_self_con){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = contract.day$avg_amount,dashStyle = "Solid")      
      }
      if(input$contract_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = contract.day$No,yAxis = 1)
      }
      if(input$com_con){
        data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator1,"both"))
        contract_sub <- subset(data,as.Date(as.character(Date.Created))>= input$dateRange_con[1]&as.Date(as.character(Date.Created)) <= input$dateRange_con[2])
        contract.day <- aggregate(contract_sub$Amount..Net.of.Tax.,by = list(contract_sub$year,contract_sub$Date.Created),
                                  FUN=sum)
        names(contract.day) <- c("year","day","amount")
        h$series(name = input$Contract_creator1, type = 'spline', color = 'purple',
                 data = contract.day$amount,dashStyle = "Dash")
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      h$title(text = paste("Contract Generated by ",input$Contract_creator,"Show By Day"))
      return(h)
    }
    
  })
  output$ProposalPlots <- renderChart2({
    data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator,"both"))
    if(input$plotty_pro == "month"){
      proposal.month <- aggregate(data$Amount..Net.of.Tax.,
                                  by = list(data$year,data$month),FUN = sum)
      names(proposal.month) <- c("year","month","amount")
      proposal.month.no <- aggregate(data$Internal.ID,by =list(data$year,data$month),FUN = length)
      proposal.month$No <- proposal.month.no[,3]
      proposal.month <- subset(proposal.month,proposal.month$year %in% input$year_pro)
      proposal_avg_month <- subset(proposal_avg_month(),trimws(Segment,"both") %in% trimws(input$segment_pro,"both"))
      proposal_avg_month <- subset(proposal_avg_month,trimws(proposal_avg_month$Year,"both") %in% trimws(input$year_pro,"both"))
      proposal_avg_month <- subset(proposal_avg_month,proposal_avg_month$Month %in% proposal.month$month)
      proposal.month$avg_amount <- mean(proposal.month$amount)
      h <- Highcharts$new()
      h$xAxis(categories = proposal.month$month,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'column', color = '#4572A7',
               data = proposal.month$amount)
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_pro){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = proposal_avg_month$`Proposal Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_pro){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = proposal.month$avg_amount,dashStyle = "Solid")      
      }
      if(input$proposal_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = proposal.month$No,
                 yAxis = 1)
      }
      if(input$comp_pro){
          data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator1,"both"))
          proposal.month <- aggregate(data$Amount..Net.of.Tax.,
                                      by = list(data$year,data$month),FUN = sum)
          names(proposal.month) <- c("year","month","amount")
          proposal.month <- subset(proposal.month,proposal.month$year == input$year_pro )
          h$series(name = input$Proposal_creator1, type = 'column',color="purple",
                   data = proposal.month$amount)
    
      }
      h$title(text = paste("Proposal Generated by ",input$Proposal_creator,"Show By Month"))
      return(h)
      
    }
    else if(input$plotty_pro == "week"){
      proposal.week <- aggregate(data$Amount..Net.of.Tax.,
                                 by = list(data$year,data$week),
                                 FUN = sum)
      names(proposal.week) <- c("year","week","amount")
      proposal.week.no <-aggregate(data$Internal.ID,by =list(data$year,data$week),FUN = length)
      proposal.week$No <- proposal.week.no[,3]
      proposal.week <- subset(proposal.week,proposal.week$year == input$year_pro)
      proposal.week$avg_amount <- mean(proposal.week$amount)
      proposal_avg_week <- subset(proposal_avg_week(),trimws(Segment,"both") %in% trimws(input$segment_pro,"both"))
      proposal_avg_week <- subset(proposal_avg_week,trimws(proposal_avg_week$Year,"both") %in% trimws(input$year_pro,"both"))
      proposal_avg_week <- subset(proposal_avg_week,proposal_avg_week$Week %in% proposal.week$week)
      h <- Highcharts$new()
      h$xAxis(categories = proposal.week$week,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'column', color = '#4572A7',
               data = proposal.week$amount)
      
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_pro){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = proposal_avg_week$`Proposal Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_pro){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = proposal.week$avg_amount,dashStyle = "Solid")      
      }
      if(input$proposal_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = proposal.week$No,
                 yAxis = 1)
      }
      if(input$comp_pro){
        data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator1,"both"))
        proposal.week <- aggregate(data$Amount..Net.of.Tax.,
                                   by = list(data$year,data$week),
                                   FUN = sum)
        names(proposal.week) <- c("year","week","amount")
        proposal.week <- subset(proposal.week,proposal.week$year == input$year_pro)
        h$series(name = input$Proposal_creator1, type = 'column', color = 'purple',
                 data = proposal.week$amount)
        
      }
      h$title(text = paste("Proposal Generated by ",input$Proposal_creator,"Show By Week"))
      return(h)
    }
    else if(input$plotty_pro == "day")
    {
      proposal_sub = subset(data,as.Date(as.character(Date.Created))>= input$dateRange_pro[1]&as.Date(as.character(Date.Created)) <= input$dateRange_pro[2])
      proposal.day <- aggregate(proposal_sub$Amount..Net.of.Tax.,
                                by = list(proposal_sub$year,proposal_sub$Date.Created),
                                FUN=sum)
      names(proposal.day) <- c("year","day","amount")
      proposal.day.no <- aggregate(proposal_sub$Internal.ID,by = list(proposal_sub$Date.Created),FUN = length)
      proposal.day$No <- proposal.day.no[,2]
      proposal_avg_day <- subset(proposal_avg_day(),trimws(Segment,"both") %in% trimws(input$segment_pro,"both"))
      proposal.day$avg_amount <- mean(proposal.day$amount)
      proposal_avg_day <- subset(proposal_avg_day,proposal_avg_day$Date %in% proposal.day$day)
      h <- Highcharts$new()
      h$xAxis(categories = proposal.day$day,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'spline', color = '#4572A7',
               data = proposal.day$amount)
      if (input$avg_line_pro){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = proposal_avg_day$`Proposal Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_pro){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = proposal.day$avg_amount,dashStyle = "Solid")      
      }
      if(input$proposal_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',data = proposal.day$No,yAxis = 1)
      }
      if(input$comp_pro){
        data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator1,"both"))
        proposal_sub = subset(data,as.Date(as.character(Date.Created))>= input$dateRange_pro[1]&as.Date(as.character(Date.Created)) <= input$dateRange_pro[2])
        proposal.day <- aggregate(proposal_sub$Amount..Net.of.Tax.,
                                  by = list(proposal_sub$year,proposal_sub$Date.Created),
                                  FUN=sum)
        names(proposal.day) <- c("year","day","amount")
        h$series(name = input$Proposal_creator1, type = 'spline', color = 'purple',
                 data = proposal.day$amount,dashStyle = "Dash")
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      h$title(text = paste("Proposal Generated by ",input$Proposal_creator,"Show By Day"))
      return(h)
    }    
  })
  output$SalesPlot <- renderChart2({
    data <- subset(sales(),trimws(Pro...Created.By,"both") %in% trimws(input$sales_rep,"both"))
    if(input$plotty_sale =="month"){
      sales.month <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$year,data$month),FUN = sum)
      names(sales.month) <- c("Year","Month","Amount")
      sales.month.no <- aggregate(data$SO.Number,by =list(data$year,data$month),FUN = length) 
      sales.month$No <- sales.month.no[,3]
      sales.month <-subset(sales.month,trimws(sales.month$Year,"both") %in% trimws(input$year_sale,"both"))
      sales_avg_month <- subset(sales_avg_month(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
      sales_avg_month <- subset(sales_avg_month,trimws(sales_avg_month$Year,"both") %in% trimws(input$year_sale,"both"))
      sales.month$avg_amount <- mean(sales.month$Amount)
      sales_avg_month <- subset(sales_avg_month,sales_avg_month$Month %in% sales.month$Month)
      h <- Highcharts$new()
      h$xAxis(categories = sales.month$Month,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'column', color = '#4572A7',
               data = sales.month$Amount)
      
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_month$`Sales Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.month$avg_amount,dashStyle = "Solid")      
      }
      if(input$SO_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = sales.month$No,
                 yAxis = 1)
      }
      if(input$com_so){
        data <- subset(sales(),trimws(Pro...Created.By,"both") %in% trimws(input$sales_rep1,"both"))
        sales.month <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$year,data$month),FUN = sum)
        names(sales.month) <- c("Year","Month","Amount")
        sales.month <-subset(sales.month,trimws(sales.month$Year,"both") %in% trimws(input$year_sale,"both"))
        h$series(name = input$sales_rep1, type = 'column', color = 'purple',
                 data = sales.month$Amount)
        
      }
      h$title(text = paste("Sales Order Generated by ",input$sales_rep,"Show By Month"))
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
      sales_avg_week <- subset(sales_avg_week,sales_avg_week$Week %in% sales.week$Week)
      h <- Highcharts$new()
      h$xAxis(categories = sales.week$Week,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'column', color = '#4572A7',
               data = sales.week$Amount,dashStyle = "Dash")
      
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_week$`Sales Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.week$avg_amount,dashStyle = "Solid")      
      }
      if(input$SO_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',
                 data = sales.week$No,
                 yAxis = 1)
      }
      if(input$com_so){
        data <- subset(sales(),trimws(Pro...Created.By,"both") %in% trimws(input$sales_rep1,"both"))
        sales.week <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$year,data$week),FUN = sum)
        names(sales.week) <- c("Year","Week","Amount")
        sales.week <- subset(sales.week,trimws(sales.week$Year,"both") %in% trimws(input$year_sale,"both"))
        h$series(name = input$sales_rep1, type = 'column', color = 'purple',
                 data = sales.week$Amount)
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      h$title(text = paste("Sales Order Generated by ",input$sales_rep,"Show By Week"))
      return(h)
    }
    else if(input$plotty_sale == "day"){
      sales <- subset(data,as.Date(Date.Created) >= input$dateRange_sale[1]&as.Date(Date.Created) <= input$dateRange[2])
      sales.day <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.,by = list(sales$Date.Created),FUN = sum)
      names(sales.day) <- c("Date","Amount")
      sales.day.no <- aggregate(sales$SO.Number,by = list(sales$Date.Created),FUN = length)
      sales.day$No <- sales.day.no[,2]
      sales_avg_day <- subset(sales_avg_day(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
      sales.day$avg_amount <- mean(sales.day$Amount)
      sales_avg_day <- subset(sales_avg_day,sales_avg_day$Date %in% sales.day$Date)
      h <- Highcharts$new()
      h$xAxis(categories = sales.day$Date,labels = list(rotation = 90, align = "left"))
      h$yAxis(list(list(title = list(text = 'Total Value'))
                   , list(title = list(text = 'Total No'), opposite = TRUE)
      )
      )
      h$series(name = 'Total Value', type = 'spline', color = '#4572A7',
               data = sales.day$Amount,groups = "Total Sales Amount")
      if (input$avg_line_sale){
        h$series(name = "Team Average", type = 'spline', color = "orange",
                 data = sales_avg_day$`Sales Amount`,dashStyle = "Dash")
        
      }
      if(input$avg_self_sale){
        h$series(name = "Individual Average", type = 'spline', color = "red",
                 data = sales.day$avg_amount,dashStyle = "Solid")      
      }
      if(input$SO_no){
        h$series(name = 'Total No', type = 'scatter', color = '#89A54E',data = sales.day$No,yAxis = 1)
      }
      if(input$com_so){
        data <- subset(sales(),trimws(Pro...Created.By,"both") %in% trimws(input$sales_rep1,"both"))
        sales <- subset(data,as.Date(Date.Created) >= input$dateRange_sale[1]&as.Date(Date.Created) <= input$dateRange[2])
        sales.day <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.,by = list(sales$Date.Created),FUN = sum)
        names(sales.day) <- c("Date","Amount")
        h$series(name = input$sales_rep1, type = 'spline', color = 'purple',
                 data = sales.day$Amount,groups = "Total Sales Amount",dashStyle = "Dash")
      }
      h$plotOptions(scatter = list(dataLabels = list(enabled = TRUE)))
      h$title(text = paste("Sales Order Generated by ",input$sales_rep,"Show By Day"))
      return(h)
    }
  })
  # Title in the summary part
  
  output$SummaryLeads <- renderChart2({
    df <- leads()
    max.year <- input$year_summary
    if(input$reportty == "week"){
      max.week <- max(df$week[df$year==max.year])
      df.w <- subset(df,df$week %in% c((max.week-3):max.week)&df$year==max.year)
      df.w.no <-aggregate(df.w$freq,by = list( df.w$year, df.w$week,  df.w$Created.By,df.w$segment),FUN = sum)
      names(df.w.no) <- c("year","week","created.by","segment","no")
      df = transform(df.w.no, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(no ~ week, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart") 
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "No of Leads by Week")
      
    }
    else if(input$reportty == "month"){
      df.m <- aggregate(df$freq,by = list( df$year, df$month,  df$Created.By,df$segment),FUN = sum)
      names(df.m) <- c("year","month","created.by","segment","no")
      df.m <- subset(df.m,df.m$year==max.year)
      df = transform(df.m, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(no ~ month, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart")
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "No of Leads by Month")
    }
    r1$chart(
      showControls = FALSE 
    )
    return(r1)
  })
  output$SummaryProposals <- renderChart2({
    df <- proposal()
    max.year <- input$year_summary
    if(input$reportty == "week"){
      max.week <- max(df$week[df$year==max.year])
      df.w <- subset(df,df$week %in% c((max.week-3):max.week)&df$year==max.year)
      df.w.no <-aggregate(df.w$Amount..Net.of.Tax.,by = list( df.w$year, df.w$week,  df.w$Created.By,df.w$segment),FUN = sum)
      names(df.w.no) <- c("year","week","created.by","segment","amount")
      df = transform(df.w.no, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ week, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart") 
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of Proposal by Week")
    }
    else if(input$reportty == "month"){
      df.m <- aggregate(df$Amount..Net.of.Tax.,by = list( df$year, df$month,  df$Created.By,df$segment),FUN = sum)
      names(df.m) <- c("year","month","created.by","segment","amount")
      df.m <- subset(df.m,df.m$year==max.year)
      df = transform(df.m, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ month, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart")
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of Proposal by Month")
      r1$yAxis(tickFormat = "#! function(d) {return d3.format(',.2f')(d/1000000) + 'M' } !#")
      
    }
    r1$chart(
      showControls = FALSE 
    )
    return(r1)
  })
  output$SummaryContracts <- renderChart2({
    df <- contract()
    max.year <- input$year_summary
    if(input$reportty == "week"){
      max.week <- max(df$week[df$year==max.year])
      df.w <- subset(df,df$week %in% c((max.week-3):max.week)&df$year==max.year)
      df.w.no <-aggregate(df.w$Amount..Net.of.Tax.,by = list( df.w$year, df.w$week,  df.w$Created.By,df.w$segment),FUN = sum)
      names(df.w.no) <- c("year","week","created.by","segment","amount")
      df = transform(df.w.no, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ week, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart") 
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of Contract by Week")
    }
    else if(input$reportty == "month"){
      df.m <- aggregate(df$Amount..Net.of.Tax.,by = list( df$year, df$month,  df$Created.By,df$segment),FUN = sum)
      names(df.m) <- c("year","month","created.by","segment","amount")
      df.m <- subset(df.m,df.m$year==max.year)
      df = transform(df.m, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ month, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart")
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of Contract by Month")
      r1$yAxis(tickFormat = "#! function(d) {return d3.format(',.2f')(d/1000000) + 'M' } !#")
      
    }
    r1$chart(
      showControls = FALSE 
    )
    return(r1)
  })
  output$SummarySOs <- renderChart2({
    df <- sales()
    max.year <- input$year_summary
    if(input$reportty == "week"){
      max.week <- max(df$week[df$year==max.year])
      df.w <- subset(df,df$week %in% c((max.week-3):max.week)&df$year==max.year)
      df.w.no <-aggregate(df.w$Maximum.of.Amount..Net.of.Tax.,
                          by = list( df.w$year, df.w$week,  df.w$Pro...Created.By,df.w$segment),FUN = sum)
      names(df.w.no) <- c("year","week","created.by","segment","amount")
      df = transform(df.w.no, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ week, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart") 
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of SalesOrder by Week")
    }
    else if(input$reportty == "month"){
      df.m <- aggregate(df$Maximum.of.Amount..Net.of.Tax.,by = list( df$year, df$month,  df$Pro...Created.By,df$segment),FUN = sum)
      names(df.m) <- c("year","month","created.by","segment","amount")
      df.m <- subset(df.m,df.m$year==max.year)
      df = transform(df.m, seg_creator = paste(segment, created.by, sep = "_"))
      r1<- nPlot(amount ~ month, group = 'seg_creator', data = subset(df,df$segment==input$segLevel), type="multiBarChart")
      r1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      r1$set(title = "Amount of SalesOrder by Month")
      r1$yAxis(tickFormat = "#! function(d) {return d3.format(',.2f')(d/1000000) + 'M' } !#")
      
    }
    r1$chart(
      showControls = FALSE 
    )
    return(r1)
  })
 

 })