setwd('C:/Users/Sunny/Desktop/Recent/Internship/Dynamic Plot/')
sales <- read.csv('Sales Order.csv')
sales_rep <- read.csv('sales_rep.csv')
sales <- subset(sales,sales$Sales.Rep.1 %in% as.character(sales_rep$sale_rep))
col <- c("Date.Created","Name","Sales.Rep.1","Status","Event.Name"
         ,"Maximum.of.Amount","Maximum.of.Amount..Net.of.Tax."
         ,"Opp...Event.Start.Date","Opp...Event.End.Date")
sales <- sales[,col]
# convert factor with comma to number
factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
  
}
sales$Maximum.of.Amount <- factor2numeric(sales$Maximum.of.Amount)
sales$Maximum.of.Amount..Net.of.Tax. <- factor2numeric(sales$Maximum.of.Amount..Net.of.Tax.)
sales <- merge(sales, sales_rep,by.x= "Sales.Rep.1",by.y = "sale_rep")
sales$Date.Created <- as.Date(strptime(as.character(sales$Date.Created), "%d/%m/%Y"))


sales
