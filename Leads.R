library(plyr)
setwd('C:/Users/Sunny/Desktop/Recent/Internship/Dynamic Plot/')
sales_rep <- read.csv('sales_rep.csv')

leads <- read.csv('Leads.csv')

leads$Date.Created <- strptime(as.character(leads$Date.Created), "%d/%m/%Y")
leads_date_generator <- count(leads,c("Date.Created","Lead.Generator","segment"))
leads_date_generator$Lead.Generator <- as.factor(leads_date_generator$Lead.Generator)
leads$Date.Created <- as.Date(leads$Date.Created)

leads_date_generator
