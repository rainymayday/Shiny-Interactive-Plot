setwd('C:/Users/Sunny/Desktop/Recent/Internship/Dynamic Plot/')
proposal <- read.csv('Proposal.csv')
# remove duplicates
proposal <- proposal[!duplicated(proposal),]
# No of customers
cols <- c("Date.Created","Name","Created.By"
          ,"Amount..Net.of.Tax.")
proposal <- proposal[,cols]
sales_rep <- read.csv('sales_rep.csv')

factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
  
}
proposal$Amount..Net.of.Tax. <- factor2numeric(proposal$Amount..Net.of.Tax.)
proposal <- subset(proposal,proposal$Created.By %in% as.character(sales_rep$sale_rep))
proposal$Date.Created <- as.Date(strptime(as.character(proposal$Date.Created), "%d/%m/%Y"))
proposal <- merge(proposal, sales_rep,by.x= "Created.By",by.y = "sale_rep")

