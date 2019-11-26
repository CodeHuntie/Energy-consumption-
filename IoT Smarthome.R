library(RMySQL); library(dplyr)

setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C3Task1-IoT")
# imoport txt file, later will use SQL to import  #Smarthome <-read.delim("household_power_consumption.txt")
## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)
## Lists attributes contained in a table
dbListFields(con,'yr_2006')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
## 
yr2006.SELECT <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
