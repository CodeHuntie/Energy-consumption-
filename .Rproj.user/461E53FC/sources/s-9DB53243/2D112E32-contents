library(readr);library(arules); library(arulesViz); library(RColorBrewer); library(ggplot2)

setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C2Task4-Market basket Analysis")
Sales<- read.transactions("ElectronidexTransactions2017.csv", sep = ",")
#Sales<- read.transactions("ProductTypeT.csv", sep = ",")
# duplicate removed when 2 itmes are in 1 basket 
inspect(Sales)
length(Sales);size(Sales);LIST(Sales)
itemLabels(Sales)

#plotting #pastel color brewer can check colors 
itemFrequencyPlot(Sales, topN= 10, type= "relative")
image(sample(Sales, 1200))
itemFrequencyPlot(Sales,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Eletronidex Sales")

# apriori to run Rules # high conf gets no rules, play with parameter 
Sales.Rules<- apriori (Sales, parameter = list(supp = 0.002, conf = 0.8, minlen= 2))

# Sorting measurement, or by support and confidence
inspect(sort(Sales.Rules, by = "support"))
inspect(head(Sales.Rules, 10))
# another way 
#top.support <- sort.list(Sales.Rules, decreasing = TRUE, na.last = NA, by = "support")

# Mass data with brand names 
# Appearance is another way to subset by controlling rhs 
imac.Rules<- apriori (Sales, parameter = list(supp = 0.002, conf = 0.8, minlen= 2), appearance = list(default= "lhs", rhs="iMac"))
inspect(imac.Rules); plot(imac.Rules)
plot(imac.Rules, method = "grouped", control= list(k=3))
plot(sort(Sales.Rules, by= "lift"), method = "graph", engine= "htmlwidget") #interactive plots

################### Product type data 
pSales<- read.transactions("ElectronidexTransactions2017final.csv", sep = ";")
pSales.Rules<- apriori (pSales, parameter = list(supp = 0.03, conf = 0.8, minlen= 2))
inspect(pSales.Rules) # strong corelation of popular product iMac 
Keyboard.Rules <- subset(pSales.Rules, items %in% "Keyboard")
summary(Keyboard.Rules); plot(Keyboard.Rules)

# find your redundant rules, how to combine them? 
is.redundant(pSales.Rules, measure= "lift" )
inspect(pSales.Rules[is.redundant(pSales.Rules)])

# plot them
plot(pSales.Rules, jitter= 2)
plot(pSales.Rules[1:iMac.Rules], method="graph", control=list(type="items")) 
plot(pSales.Rules, method = "paracoord") #not easy to explain 
plot(pSales.Rules, method = "two-key plot") # shows orders (set of items)
plot(sort(pSales.Rules, by= "lift"), method = "graph", engine= "htmlwidget") #interactive plots
plot(pSales.Rules, method = "grouped", control= list(k=5)) #grouped matrix, better use on the product type 
itemFrequencyPlot(pSales,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Product type ranking")
