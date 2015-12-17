setwd('C:/Users/Sunny/Desktop/Recent/Internship/Dynamic Plot/')
contract <- read.csv('Contract.csv')
sales_rep <- read.csv('sales_rep.csv')
contract <- subset(contract,contract$Created.By %in% as.character(sales_rep$sale_rep))
col <- c("Date.Created","Created.By","Name","Amount..Net.of.Tax.")
contract <- contract[,col]
factor2numeric <- function(x){
  x.new <- as.numeric(gsub(",","",as.character(x)))
  return (x.new)
  
}

contract$Amount..Net.of.Tax. <- factor2numeric(contract$Amount..Net.of.Tax.)
contract$Date.Created <- as.Date(strptime(as.character(contract$Date.Created), "%d/%m/%Y"))
contract <- merge(contract, sales_rep,by.x= "Created.By",by.y = "sale_rep")





