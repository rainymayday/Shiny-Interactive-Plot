library(shiny)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plyr)
library(scales)

load_df <- function(Rfile){
  df <- source(Rfile)
  df <- as.data.frame(df[1])
  names(df) <- gsub("value.","",names(df))
  return (df)
}

leads <- load_df('Leads.R')
leads$Date.Created <- as.POSIXct(as.Date(leads$Date.Created))
leads_avg_day <- aggregate(leads$freq
                           ,by = list(leads$Date.Created,leads$segment)
                           ,FUN = mean)
names(leads_avg_day) <- c("Date","Segment","Leads")
leads$week <- as.numeric(format(leads$Date.Created+3, "%U"))
leads_avg_week <- aggregate(leads$freq
                            ,by = list(leads$week,leads$segment)
                            ,FUN=mean)
names(leads_avg_week) <- c("Date","Segment","Leads")


sales <- load_df('SO.R')
sales_avg_day <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.
                           ,by = list(sales$Date.Created,sales$segment)
                           ,FUN = mean)
names(sales_avg_day) <- c("Date","Segment","Sales Amount")
sales$week <- as.numeric(format(sales$Date.Created+3, "%U"))
sales_avg_week <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.
                            ,by = list(sales$week,sales$segment)
                            ,FUN = mean)
names(sales_avg_week) <- c("Date","Segment","Sales Amount")


proposal <- load_df('Proposal.R')
contract <- load_df('Contract.R')





shinyServer(function(input, output,session) {
  SalesTable <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.,by = list(sales$Sales.Rep.1,sales$segment),FUN = sum)
  names(SalesTable) <- c("Sale_rep","Segment","Sales_Amount")
  
  LeadsTable <- aggregate(leads$freq,by = list(leads$Lead.Generator,leads$segment),FUN = sum)
  names(LeadsTable) <- c("Sale_rep","segment","No of Leads")
  
  SalesTable_No <- aggregate(sales$Event.Name,by = list(sales$Sales.Rep.1,sales$segment),FUN = length)
  SalesTable$No_of_Sales <- SalesTable_No[,3]
  
  ProposalTable <- aggregate(proposal$Amount..Net.of.Tax.,by = list(proposal$Created.By,proposal$segment),FUN = sum)
  names(ProposalTable) <- c("Created.By","Segment","Total Amount")
  ProposalTable_No <- aggregate(proposal$Amount..Net.of.Tax.,by = list(proposal$Created.By,proposal$segment),FUN = length)
  ProposalTable$No_of_Proposal <- ProposalTable_No[,3]
  
  contractTable <- aggregate(contract$Amount..Net.of.Tax.,by = list(contract$Created.By,contract$segment),FUN = sum)
  names(contractTable) <- c("Created.By","Segment","Total Amount")
  contractTable_No <- aggregate(contract$Amount..Net.of.Tax., by = list(contract$Created.By,contract$segment),FUN=sum)
  contractTable$No_Of_Contract <- contractTable_No[,3]
  
  
  
  
  
   observe({
   updateSelectInput(session,inputId = "LeadsGen" ,label = 'Leads Generator'
                       ,choices = unique(as.character(leads$Lead.Generator[leads$segment==input$segment]))
                       ,selected = "E0063 Rena Wong")
   })
   observe({
     updateSelectInput(session,inputId = "sales_rep" ,label = 'sales_rep'
                       ,choices = unique(as.character(sales$Sales.Rep.1[sales$segment==input$segment_sale])))
   })
   observe({
     updateSelectInput(session,inputId = "Proposal_creator" ,label = 'Created by'
                       ,choices = unique(as.character(proposal$Created.By[proposal$segment==input$segment_pro])))
   })
   observe({
     updateSelectInput(session,inputId = "Contract_creator",label = "Created by",
                       choices = unique(as.character(contract$Created.By[contract$segment == input$segment_con])))
   })
   output$dateRange <- renderUI({
     if(input$plotty == "day"){
       dateRangeInput('dateRange','Choose date range'
                      ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
     }
   })
   
   output$dateRange_con <- renderUI({
     if(input$plotty_con == "day"){
       dateRangeInput("dateRange_con",'Choose date range'
                      ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
     }
   })
  

   
   output$data <- renderTable({
     
     inFile <- input$file1
     
     if (is.null(inFile))
       return(NULL)
     
     read.csv(inFile$datapath, header=input$header, sep=input$sep, 
              quote=input$quote)
   })


   output$dateRange_sale <- renderUI({
     if(input$plotty_sale == "day"){
       dateRangeInput('dateRange_sale','Choose date range'
                      ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
     }
     
     
   })
   
   output$dateRange_pro <- renderUI({
     if(input$plotty_pro == "day"){
       dateRangeInput('dateRange_pro','Choose date range'
                      ,start = as.Date("2015-11-01"), end = as.Date("2015-12-01"))
     }
   })

   output$LeadsPlot <- renderPlot({
    environment<-environment()
    data=subset(leads,trimws(Lead.Generator,"both") %in% trimws(input$LeadsGen,"both"))
    leads_sub = subset(data,as.Date(as.character(Date.Created)) >= input$dateRange[1]&as.Date(as.character(Date.Created)) <= input$dateRange[2])
    
    leads_sub$week <- as.numeric( format(leads_sub$Date.Created+3, "%U"))
    data.week <- aggregate(leads_sub$freq, by = list(leads_sub$week),FUN = sum)
    leads_avg_day <- subset(leads_avg_day,trimws(Segment,"both") == input$segment )
    leads_avg_day <- subset(leads_avg_day
                        ,as.Date(Date) >= input$dateRange[1]&as.Date(Date) <= input$dateRange[2])
    leads_avg_week <- subset(leads_avg_week,trimws(Segment,"both") ==input$segment)

    switch(input$plotty,
           "week" = {aesthetics1 = aes(x=data.week[,1], y=data.week[,2])
           leads_data = data.week
           xlabtxt = "Week"
           avg = leads_avg_week
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
            aesthetics1 = aes(x=Date.Created, y=freq)
            
           leads_data = leads_sub
           xlabtxt = "Day"
           avg = leads_avg_day
           plotty = geom_line(aesthetics1,data = leads_data,size = 1.2,colour = "#00CCCC")
             }
    )
      p <- ggplot(data = leads_data,mapping = aesthetics1,environment = environment)+
        plotty+geom_point(size = 1.5)
      
      if(input$avg_line){
          p <- p+
            geom_line(mapping = aes(x=Date, y=Leads)
                           ,data = avg,colour = "#CC0033",size = 0.8,linetype = 6)
      }
      p <- p+ggtitle(paste("Leads Generated by",input$LeadsGen))+
        xlab(xlabtxt)+
        ylab("No of Leads")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(p)
 
  })
   
   
   output$ProposalPlots <- renderPlot({
     environment<-environment()
     data=subset(proposal,trimws(Created.By,"both") %in% trimws(input$Proposal_creator,"both"))
     proposal_sub = subset(data
                       ,as.Date(as.character(Date.Created))>= input$dateRange_pro[1]&as.Date(as.character(Date.Created)) <= input$dateRange_pro[2])
     proposal_sub$week <- as.numeric( format(proposal_sub$Date.Created+3, "%U"))
     proposal.week <- aggregate(proposal_sub$Amount..Net.of.Tax., by = list(proposal_sub$week),FUN = sum)
     proposal.day <- aggregate(proposal_sub$Amount..Net.of.Tax.,by = list(proposal_sub$Date.Created),FUN=sum)
     switch(input$plotty_pro,
            "week" = {aesthetics1 = aes(x=proposal.week[,1], y=proposal.week[,2])
            pro_data = proposal.week
            xlabtxt = "Week"
            plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
            "day"  = {
              aesthetics1 = aes(x=proposal.day[,1], y=proposal.day[,2])
              
              pro_data = proposal.day
              xlabtxt = "Day"
              plotty = geom_line(aesthetics1,data = proposal.day,size = 1.2,colour = "#00CCCC")
            })
     p <- ggplot(data = pro_data,mapping = aesthetics1,environment = environment)+
       plotty+geom_point(size = 1.5)
     
     
     p <- p+ggtitle(paste("Proposal Created by",input$Proposal_creator))+
       xlab(xlabtxt)+
       ylab("Total Amount")+
       theme_bw()+
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
     return(p)
     
   })
   
  
  output$SalesPlot <- renderPlot({
    environment<-environment()
    sales = subset(sales,as.Date(as.character(Date.Created)) >= input$dateRange_sale[1]&as.Date(as.character(Date.Created)) <= input$dateRange[2])
    data = subset(sales,trimws(Sales.Rep.1,"both")==trimws(input$sales_rep,"both"))
    sales.day <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$Date.Created),FUN = sum)
    
    data$week <- as.numeric( format(data$Date.Created+3, "%U"))
    sales.week <- aggregate(data$Maximum.of.Amount..Net.of.Tax.,by = list(data$week),FUN = sum)
    
    sales_avg_day <- subset(sales_avg_day,trimws(Segment,"both") == input$segment_sale)
    sales_avg_week <- subset(sales_avg_week,trimws(Segment,"both") == input$segment_sale)
    
    switch(input$plotty_sale,
           "week" = {aesthetics1 = aes(x=sales.week[,1], y= sales.week[,2])
           data = sales.week
           xlabtxt = "Week"
           avg = sales_avg_week
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {aesthetics1 = aes(x=sales.day[,1], y= sales.day[,2])
           data = sales.day
           xlabtxt = "Day"
           avg = sales_avg_day
           plotty = geom_line(size = 1.2,colour = "#3399FF")}
           
    )
    p <- ggplot(data,aesthetics1
                ,environment = environment)+
      plotty+
      geom_point(aesthetics1,data,size = 1.5,colour = "#000033")
    
    if(input$avg_line_sale){
      p <- p+geom_line(mapping = aes(x=Date, y=`Sales Amount`)
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
    environment<-environment()
    data=subset(contract,trimws(Created.By,"both") %in% trimws(input$Contract_creator,"both"))
    contract_sub = subset(data
                          ,as.Date(as.character(Date.Created))>= input$dateRange_con[1]&as.Date(as.character(Date.Created)) <= input$dateRange_con[2])
    contract_sub$week <- as.numeric( format(contract_sub$Date.Created+3, "%U"))
    contract.week <- aggregate(contract_sub$Amount..Net.of.Tax., by = list(contract_sub$week),FUN = sum)
    contract.day <- aggregate(contract_sub$Amount..Net.of.Tax.,by = list(contract_sub$Date.Created),FUN=sum)
    switch(input$plotty_con,
           "week" = {aesthetics1 = aes(x=contract.week[,1], y=contract.week[,2])
           pro_data = contract.week
           xlabtxt = "Week"
           plotty= geom_bar(size = 1.2,fill= "#00CCCC",stat="identity")},
           "day"  = {
             aesthetics1 = aes(x=contract.day[,1], y=contract.day[,2])
             
             pro_data = contract.day
             xlabtxt = "Day"
             plotty = geom_line(aesthetics1,data = contract.day,size = 1.2,colour = "#00CCCC")
           })
    p <- ggplot(data = pro_data,mapping = aesthetics1,environment = environment)+
      plotty+geom_point(size = 1.5)
    
    
    p <- p+ggtitle(paste("Contract Created by",input$Proposal_creator))+
      xlab(xlabtxt)+
      ylab("Total Amount")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
  })

  
  output$LeadsTable <- renderDataTable({
    LeadsTable
 })

  output$SalesTable <- renderDataTable({
    SalesTable
  })
  
  output$ProposalTable <- renderDataTable({
    ProposalTable
  })
  output$ContractTable <- renderTable(contractTable)
  

  
  output$downloadSales <- downloadHandler(
    filename = function(){
      paste('sales','.csv',sep='')
      },
    content = function(file){
      write.csv(SalesTable,file)
      }
  )
  output$downloadLeads <- downloadHandler(
    filename = function(){
      paste('Leads','.csv',sep='')
    },
    content = function(file){
      write.csv(LeadsTable,file)
    }
  )
  
  output$downloadProposal <- downloadHandler(
    filename = function(){
      paste('Proposal','.csv',sep='')
    },
    content = function(file){
      write.csv(ProposalTable,file)
    }
  )
  
 })