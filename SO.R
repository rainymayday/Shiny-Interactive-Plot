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
# sales amount by sales rep
Total_Sales = aggregate(sales$Maximum.of.Amount..Net.of.Tax.
                        ,by = list(sales$Sales.Rep.1),FUN = sum)

#sales amount by segment
Sales_by_seg <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.
                          ,by = list(sales$segment),FUN = sum)
Sales_by_seg_count <- aggregate(sales$Maximum.of.Amount..Net.of.Tax.
                                ,by = list(sales$segment),FUN = length)
# bar chart in qplot
qplot(x = Total_Sales[,1],y = Total_Sales[,2]
      ,fill=factor(Total_Sales[,1]), geom="bar", stat="identity"
      , position="dodge", alpha=I(1/5), data=Total_Sales, xlab="Sales Person"
      , ylab="Total Amount (SGD)", main="Sales Amount by Sales Rep") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) + 
  guides(fill=guide_legend(title="Sale_Rep", reverse=FALSE))
# bar chart in qplot
qplot(x = Sales_by_seg[,1],y = Sales_by_seg[,2]
      ,fill=Sales_by_seg[,2], geom="bar", stat="identity"
      , position="dodge", alpha=I(1/5), data=Sales_by_seg, xlab="Sales Person"
      , ylab="Total Amount (SGD)", main="Sales Amount by Segment") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) + 
  guides(fill=guide_legend(title="Sale_Rep", reverse=FALSE))

# bar chart in ggplot2
p <- ggplot(Total_Sales,aes(x=Total_Sales[,1], y= Total_Sales[,2]
                            ,fill = Total_Sales[,1]))
p <- p+ geom_bar(stat = "identity")+
  ggtitle(paste("Sales Amount by Sales Rep "))+
  ylab("Sales Amount")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p
# bar chart using ggplot
p <- ggplot(Sales_by_seg,aes(x=Sales_by_seg[,1], y= Sales_by_seg[,2]
                             ,fill = Sales_by_seg[,1]))
p <- p+ geom_bar(stat = "identity")+
  ggtitle(paste("Sales Amount by Sales Rep "))+
  ylab("Sales Amount")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p


sales
