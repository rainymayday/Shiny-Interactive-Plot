options(shiny.maxRequestSize=30*1024^2) 
library(shiny)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plyr)
require(lubridate)
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
    num_confex <- length(salesrep()$sale_rep[salesrep()$segment=="confex"])
    return(num_confex)
  })
  num_corporate <- reactive({
    num_corporate <-length(salesrep()$sale_rep[salesrep()$segment=="corporate"])
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
      sales <- subset(sales,sales$Sales.Rep.1 %in% as.character(salesrep()$sale_rep))
      col <- c("Date.Created","Name","Sales.Rep.1","Status","Event.Name"
               ,"Maximum.of.Amount","Maximum.of.Amount..Net.of.Tax."
               ,"Opp...Event.Start.Date","Opp...Event.End.Date")
      sales <- sales[,col]
      sales$Maximum.of.Amount <- factor2numeric(sales$Maximum.of.Amount)
      sales$Maximum.of.Amount..Net.of.Tax. <- factor2numeric(sales$Maximum.of.Amount..Net.of.Tax.)
      sales <- merge(sales, salesrep(),by.x= "Sales.Rep.1",by.y = "sale_rep")
      sales$Date.Created <- as.character(as.Date(sales$Date.Created, "%d/%m/%Y"))
      sales$week <- week(sales$Date.Created)
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
      proposal <- subset(proposal,proposal$Created.By %in% as.character(sales_rep$sale_rep))
      proposal$Date.Created <- as.character(as.Date(proposal$Date.Created, "%d/%m/%Y"))
      proposal <- merge(proposal, salesrep(),by.x= "Created.By",by.y = "sale_rep")
    })
    return (proposal)
    
  })
  
  # generate summary tables
  LeadsTable <- reactive({
    LeadsTable <- aggregate(leads()$freq,by = list(leads()$Lead.Generator,leads()$segment),FUN = sum)
    names(LeadsTable) <- c("Sale_rep","segment","No of Leads")
    return(LeadsTable)
  })
  SalesTable <- reactive({
    SalesTable <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                            ,by = list(sales()$Sales.Rep.1,sales()$segment),FUN = sum)
    names(SalesTable) <- c("Sale_rep","Segment","Sales_Amount")
    SalesTable_No <- aggregate(sales()$Event.Name
                               ,by = list(sales()$Sales.Rep.1,sales()$segment),FUN = length)
    SalesTable$No_of_Sales <- SalesTable_No[,3]
    return(SalesTable)
  })
  ContractTable <- reactive({
    contractTable <- aggregate(contract()$Amount..Net.of.Tax.
                               ,by = list(contract()$Created.By,contract()$segment),FUN = sum)
    names(contractTable) <- c("Created.By","Segment","Total Amount")
    contractTable_No <- aggregate(contract()$Amount..Net.of.Tax.
                                  ,by = list(contract()$Created.By,contract()$segment),FUN=length)
    contractTable$No_Of_Contract <- contractTable_No[,3]
    return(contractTable)
  })
  ProposalTable <- reactive({
    ProposalTable <- aggregate(proposal()$Amount..Net.of.Tax.,by = list(proposal()$Created.By,proposal()$segment),FUN = sum)
    names(ProposalTable) <- c("Created.By","Segment","Total Amount")
    ProposalTable_No <- aggregate(proposal()$Amount..Net.of.Tax.,by = list(proposal()$Created.By,proposal()$segment),FUN = length)
    ProposalTable$No_of_Proposal <- ProposalTable_No[,3]
    return(ProposalTable)
  })
  
  # datasets for summary part
  leads1 <- reactive({
    leads1 <-subset(LeadsTable(),
           tolower(trimws(LeadsTable()$segment,"both")) == tolower(trimws(input$segLevel,"both")))
    return(leads1)
  })
  contract1 <- reactive({
    contract1 <-subset(ContractTable(),
                       tolower(trimws(ContractTable()$Segment,"both")) == tolower(trimws(input$segLevel,"both")))
    return(contract1)
  })
  proposal1 <- reactive({
    proposal1 <- subset(ProposalTable(),
                        tolower(trimws(ProposalTable()$Segment,"both")) == tolower(trimws(input$segLevel,"both")))
    return(proposal1)
  })
  so1 <- reactive({
      so1 <- subset(SalesTable(),
                    tolower(trimws(SalesTable()$Segment,"both")) %in% tolower(trimws(input$segLevel,"both")))
      return(so1)
    })
  
  # show tables in the beginning, for users to check the correctness of the table
  output$salesperson <- renderTable({
    salesrep()
  })
  output$leads <- renderTable({
      leads()
    })
  output$sales <- renderTable({
    sales()
    })
  output$contact <- renderTable({
    contract()
  })
  output$proposal <- renderTable({
    proposal()
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
  output$selectList <- renderUI({
    if (input$reportty == "week"){
      selectInput("list",input$reportty,choices = unique(sort(week(sales()$Date.Created))))  
    }
    else if(input$reportty == "month"){
      selectInput("list",input$reportty,choices = unique((month(sales()$Date.Created,label = TRUE,abbr = TRUE))))
    }
  })
  output$segLevel <- renderUI({
    selectInput("segLevel","segment",choices = as.character(sales()$segment))
  })
  output$RepLevel<- renderUI({
    if(input$level == "sales rep"){
      selectInput("RepLevel","Sales Rep",choices = unique(as.character(sales()$Sales.Rep.1) ))
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
    ContractTable()
  })
  output$ProposalTable <- renderDataTable({
    ProposalTable()
  })
  
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
    updateSelectInput(session,inputId ="RepLevel" ,label ="Sales Rep" ,choices = as.character(sales()$Sales.Rep.1[sales()$segment == input$segLevel]))
    
  })
  
  # calculate avg level within segment for comparison
  leads_avg_day <- reactive({
    df <- aggregate(leads()$freq,
                    by = list(leads()$year,leads()$Date.Created,leads()$segment),
                    FUN = sum)
    names(df) <- c("Year","Date","Segment","Leads")
    df$Leads[df$Segment == "confex"] <- df$Leads[df$Segment == "confex"]/num_confex()
    df$Leads[df$Segment == "corporate"] <- df$Leads[df$Segment == "corporate"]/num_corporate()
    return(df)
  })
  leads_avg_week <- reactive({
    leads_avg_week <- aggregate(leads()$freq
              ,by = list(leads()$year,leads()$week,leads()$segment)
              ,FUN=sum)
    names(leads_avg_week) <- c("Year","Date","Segment","Leads")
    leads_avg_week$Leads[leads_avg_week$Segment == "confex"] <- leads_avg_week$Leads[leads_avg_week$Segment == "confex"]/num_confex()
    leads_avg_week$Leads[leads_avg_week$Segment == "corporate"] <- leads_avg_week$Leads[leads_avg_week$Segment == "corporate"]/num_corporate()
    return(leads_avg_week)
    
  })
  sales_avg_day <- reactive({
    sales_avg_day <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                               ,by = list(sales()$Date.Created,sales()$segment)
                               ,FUN = mean)
    names(sales_avg_day) <- c("Date","Segment","Sales Amount")
    return (sales_avg_day)
  })
  sales_avg_week <- reactive({
    sales_avg_week <- aggregate(sales()$Maximum.of.Amount..Net.of.Tax.
                                ,by = list(sales()$week,sales()$segment)
                                ,FUN = mean)
    names(sales_avg_week) <- c("Date","Segment","Sales Amount")
    return(sales_avg_week)
  })
  
  # show plots in each module
  output$LeadsPlot <- renderPlot({
    data=subset(leads(),trimws(Lead.Generator,"both") %in% trimws(input$LeadsGen,"both"))
    leads_sub = subset(data,as.Date(as.character(Date.Created)) >= input$dateRange[1]&as.Date(as.character(Date.Created)) <= input$dateRange[2])
    leads_sub$week <- week(leads_sub$Date.Created)
    data.week <- aggregate(leads_sub$freq, by = list(leads_sub$week),FUN = sum)
    leads_avg_day <- subset(leads_avg_day(),trimws(Segment,"both") == trimws(input$segment,"both") )
    leads_avg_day <- subset(leads_avg_day
                            ,as.Date(Date) >= input$dateRange[1]&as.Date(Date) <= input$dateRange[2])
    leads_avg_week <- subset(leads_avg_week(),trimws(Segment,"both") ==trimws(input$segment,"both"))
    
    switch(input$plotty,
           "week" = {aesthetics1 = aes(x=data.week[,1], y=data.week[,2],group = "week")
           leads_data = data.week
           xlabtxt = "Week"
           avg = leads_avg_week
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
             aesthetics1 = aes(x=Date.Created, y=freq,group = "day")
             leads_data = leads_sub
             xlabtxt = "Day"
             avg = leads_avg_day
             plotty = geom_line(aesthetics1,data = leads_data,size = 1.2,colour = "#00CCCC")
           }
    )
    p <- ggplot(data = leads_data,mapping = aesthetics1)+
      plotty+geom_point(size = 1.5)
    
    if(input$avg_line){
      p <- p+
        geom_line(mapping = aes(x=Date, y=Leads,group = "avg")
                  ,data = avg,colour = "#CC0033",size = 0.8,linetype = 6)
    }
    p <- p+ggtitle(paste("Leads Generated by",input$LeadsGen))+
      xlab(xlabtxt)+
      ylab("No of Leads")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
     
  })
  output$SalesPlot <- renderPlot({
    sales = subset(sales(),as.Date(as.character(Date.Created)) >= input$dateRange_sale[1]&as.Date(as.character(Date.Created)) <= input$dateRange[2])
    data = subset(sales,trimws(Sales.Rep.1,"both")%in%trimws(input$sales_rep,"both"))
    sales.day <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$Date.Created),FUN = sum)
    data$week <- week(data$Date.Created)
    sales.week <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$week),FUN = sum)
    sales_avg_day <- subset(sales_avg_day(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
    sales_avg_week <- subset(sales_avg_week(),trimws(Segment,"both") %in% trimws(input$segment_sale,"both"))
    
    switch(input$plotty_sale,
           "week" = {aesthetics1 = aes(x=sales.week[,1], y= sales.week[,2],group = "week")
           data = sales.week
           xlabtxt = "Week"
           avg = sales_avg_week
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {aesthetics1 = aes(x=sales.day[,1], y= sales.day[,2],group = "day")
           data = sales.day
           xlabtxt = "Day"
           avg = sales_avg_day
           plotty = geom_line(size = 1.2,colour = "#3399FF")}
           
    )
    p <- ggplot(data,aesthetics1)+
      plotty+
      geom_point(aesthetics1,data,size = 1.5,colour = "#000033")
    
    if(input$avg_line_sale){
      p <- p+geom_line(mapping = aes(x=Date, y=`Sales Amount`,group = "avg")
                       ,data = avg,colour = "#CC0033",size = 0.8,linetype = 6)
    }
    p <- p+ggtitle(paste("Sales Generated by",input$sales_rep))+
      xlab(xlabtxt)+
      ylab("Sales Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return (p)
  })
  output$ContractPlots <- renderPlot({
    data=subset(contract(),trimws(Created.By,"both") %in% trimws(input$Contract_creator,"both"))
    contract_sub = subset(data,as.Date(as.character(Date.Created))>= input$dateRange_con[1]&as.Date(as.character(Date.Created)) <= input$dateRange_con[2])
    contract_sub$week <- week(contract_sub$Date.Created)
    contract.week <- aggregate(contract_sub$Amount..Net.of.Tax., by = list(contract_sub$week),FUN = sum)
    contract.day <- aggregate(contract_sub$Amount..Net.of.Tax.,by = list(contract_sub$Date.Created),FUN=sum)
    names(contract.day) <- c("Date","Amount")
    switch(input$plotty_con,
           "week" = {aesthetics1 = aes(x=contract.week[,1], y=contract.week[,2],group = "week")
           pro_data = contract.week
           xlabtxt = "Week"
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
             aesthetics1 = aes(x=contract.day[,1], y=contract.day[,2],group = "day")
             pro_data = contract.day
             xlabtxt = "Day"
             plotty = geom_line(aesthetics1,data = contract.day,size = 1.2,colour = "#00CCCC")
           })
    p <- ggplot(data = pro_data,mapping = aesthetics1)+
      plotty+
      geom_point(size = 1.5)+
      ggtitle(paste("Contract Created by",input$Proposal_creator))+
      xlab(xlabtxt)+
      ylab("Total Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
  })
  output$ProposalPlots <- renderPlot({
    data=subset(proposal(),trimws(Created.By,"both") %in% trimws(input$Proposal_creator,"both"))
    proposal_sub = subset(data
                          ,as.Date(as.character(Date.Created))>= input$dateRange_pro[1]&as.Date(as.character(Date.Created)) <= input$dateRange_pro[2])
    proposal_sub$week <- week(proposal_sub$Date.Created)
    proposal.week <- aggregate(proposal_sub$Amount..Net.of.Tax., by = list(proposal_sub$week),FUN = sum)
    proposal.day <- aggregate(proposal_sub$Amount..Net.of.Tax.,by = list(proposal_sub$Date.Created),FUN=sum)
    switch(input$plotty_pro,
           "week" = {aesthetics1 = aes(x=proposal.week[,1], y=proposal.week[,2],group = "week")
           pro_data = proposal.week
           xlabtxt = "Week"
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
             aesthetics1 = aes(x=proposal.day[,1], y=proposal.day[,2],group = "day")
             pro_data = proposal.day
             xlabtxt = "Day"
             plotty = geom_line(aesthetics1,data = proposal.day,size = 1.2,colour = "#00CCCC")
           })
    p <- ggplot(data = pro_data,mapping = aesthetics1)+
      plotty+geom_point(size = 1.5)+
      ggtitle(paste("Proposal Created by",input$Proposal_creator))+
      xlab(xlabtxt)+
      ylab("Total Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
  })
  
  # Title in the summary part
  output$title <- renderText(
    if (input$reportty=="month" ){
      if(input$level == "segment"){
        paste(paste("Summary Report by",input$level,sep = " "),input$segLevel,sep=":")  
      }
      else if (input$level == "sales rep"){
        paste(paste("Monthly Summary Report by",input$level,sep = " "),input$RepLevel,sep=":")
      }
    }else if (input$reportty =="week"){
      if(input$level == "segment"){
        paste(paste("Weekly Summary Report by",input$level,sep = " "),input$segLevel,sep=":")  
      }
      else if (input$level == "sales rep"){
        paste(paste("Weekly Summary Report by",input$level,sep = " "),input$RepLevel,sep=":")
        
      } 
    }
    )
  output$total_leads <- renderText(
    if(input$level == "segment"){
      paste("Total Leads:",sum(leads1()$`No of Leads`),sep = " ")
      
    }
    else if(input$level == "sales rep"){
      paste("Total Leads:",LeadsTable()$`No of Leads`[LeadsTable()$Sale_rep == input$RepLevel],sep = " ")
      
    }
  )
  output$avgperperson_leads <- renderText(paste("--Average Level Per Person:"
                                                ,round(mean(LeadsTable()$`No of Leads`),2),sep =" "))
  output$avginSeg_leads <- renderText(paste("--Average Level within Segment:"
                                            ,round(mean(leads1()$`No of Leads`),2),sep = ""))
  output$total_contracts <- renderText(
    if(input$level == "segment"){
      paste("Total Contracts sent(Amount):"
            ,round(sum(contract1()$`Total Amount`),2),sep = "")
    }
    else if(input$level == "sales rep"){
      paste("Total Contracts sent(Amount):"
            ,ContractTable()$`Total Amount`[ContractTable()$Created.By ==input$RepLevel],sep = "")
    }
  )
  output$total_contracts_no <- renderText(
    if(input$level == "segment"){
      paste("Total Contracts sent(No):",round(sum(contract1()$No_Of_Contract),2),sep = "")
    }
    else if(input$level == "sales rep"){
      paste("Total Contracts sent(No):",ContractTable()$No_Of_Contract[ContractTable()$Created.By ==input$RepLevel],sep = "")
    }
  )
  output$avgperperson_contracts <- renderText(paste("--Average Amount Per Person:"
                                                    ,round(mean(ContractTable()$`Total Amount`),2),sep = ""))
  output$avginSeg_contracts <- renderText(paste("--Average Amount within Segment:"
                                                ,round(mean(contract1()$`Total Amount`),2),sep = ""))
  output$avgperperson_contracts_no <- renderText(paste("--Average Number Per Person:"
                                                       ,round(mean(ContractTable()$No_Of_Contract),2),sep = ""))
  output$avginSeg_contracts_no <-renderText(paste("--Average Number Per Person within Segment:"
                                                  ,round(mean(contract1()$No_Of_Contract),2),sep = ""))
  output$total_proposals <- renderText(
    if(input$level == "segment"){
      paste("Total proposals sent(Amount):",round(sum(proposal1()$`Total Amount`),2),sep = "")  
    }
    else if(input$level == "sales rep"){
      paste("Total proposals sent(Amount):",ProposalTable()$`Total Amount`[ProposalTable()$Created.By==input$RepLevel],sep = "")
    }
  )
  output$total_proposals_no <- renderText(
    if(input$level == "segment"){
      paste("Total proposals sent(No):",round(sum(proposal1()$No_of_Proposal),2),sep = "") 
    }
    else if(input$level == "sales rep"){
      paste("Total proposals sent(No):",ProposalTable()$No_of_Proposal[ProposalTable()$Created.By==input$RepLevel],sep = "")
    }
  )
  output$avgperperson_proposals <- renderText(paste("--Average Amount Per Person:"
                                                    ,round(mean(ProposalTable()$`Total Amount`),2),sep = ""))
  output$avginSeg_proposals <- renderText(paste("--Average Amount within Segment:"
                                                ,round(mean(proposal1()$`Total Amount`),2),sep = ""))
  output$avgperperson_proposals_no <- renderText(paste("--Average Number Per Person:"
                                                       ,round(mean(ProposalTable()$No_of_Proposal),2),sep = ""))
  output$avginSeg_proposals_no <-renderText(paste("--Average Number Per Person within Segment:"
                                                  ,round(mean(proposal1()$No_of_Proposal),2),sep = ""))
  output$total_SO <- renderText(
    if(input$level == "segment"){
      paste("Total SO (Amount):",round(sum(so1()$Sales_Amount),2),sep = "")  
    }
    else if(input$level == "sales rep"){
      paste("Total SO (Amount):",SalesTable()$Sales_Amount[SalesTable()$Sale_rep==input$RepLevel],sep = "")
    }
  )
  output$total_SO_no <- renderText(
    if(input$level == "segment"){
      paste("Total SO (No):",round(sum(so1()$No_of_Sales),2),sep = "")  
    }
    else if(input$level == "sales rep"){
      paste("Total SO (No):",SalesTable()$No_of_Sales[SalesTable()$Sale_rep==input$RepLevel],sep = "")
    }
  )
  output$avgperperson_SO <- renderText(paste("--Average Amount Per Person:"
                                             ,round(mean(SalesTable()$Sales_Amount),2),sep=""))
  output$avginSeg_SO <- renderText(paste("--Average Amount within Segment:"
                                         ,round(mean(so1()$Sales_Amount),2),sep = ""))
  output$avgperperson_SO_no <- renderText(paste("--Average Number Per Person:"
                                                ,round(mean(SalesTable()$No_of_Sales),2),sep = ""))
  output$avginSeg_SO_no <-renderText(paste("--Average Number Per Person within Segment:"
                                           ,round(mean(so1()$No_of_Sales),2),sep = "")) 
  
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