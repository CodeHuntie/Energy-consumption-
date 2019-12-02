library(RMySQL); library(dplyr); library(lubridate); library(chron); library(ggplot2)
summary(energy79)

setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C3Task1-IoT smarthome\\IoT Smarthome")
# imoport txt file, later will use SQL to import  #Smarthome <-read.delim("household_power_consumption.txt")
## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)
## Lists attributes contained in a table
dbListFields(con,'yr_2006')
## Use asterisk to specify all attributes for download
yr2006ALL <- dbGetQuery(con)
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
## richer selection
yr2006<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr2007<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr2008<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr2009<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr2010<- dbGetQuery(con, "SELECT Global_active_power, Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

# Exploring new data frames - repeat 2007-2010 
str(yr2006)
summary(yr2006)
head(yr2006)
tail(yr2006)

# 2006 Dec 16 - Nov 26  only 
## Combine tables into one dataframe using dplyr
energy79 <- bind_rows(yr2007, yr2008, yr2009)
str(energy79)
summary(energy79)
head(energy79)
tail(energy79)

#rename variable - decide if want to change all below 
names(energy79)[names(energy79) == 'Sub_metering_1'] <- 'Kitchen'
names(energy79)[names(energy79) == 'Sub_metering_2'] <- 'Laundry'
names(energy79)[names(energy79) == 'Sub_metering_3'] <- 'Temp'
# rename with dplyr
#energy79 %>% 
 # rename(    Kitchen = Sub_metering_1,   Laundry = Sub_metering_2,    Temp = Sub_metering_3  )

## Combine Date and Time attribute values in a new attribute column
energy79<-cbind(energy79,paste(energy79$Date,energy79$Time), stringsAsFactors=FALSE)
## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(energy79)[7] <-"DateTime"

# energy79[energy79 == 'Sub_metering_1'] <- 'Kitchen'
## Move the DateTime attribute within the dataset
energy79 <- energy79[,c(ncol(energy79), 1:(ncol(energy79)-1))]
## Convert DateTime from POSIXlt to POSIXct 
#energy79$DateTime <- as.POSIXct(energy79$DateTime, "%Y/%m/%d %H:%M:%S")
energy79$DateTime <- as.POSIXct(energy79$DateTime, " %Y-%m-%d %H:%M:%S", tz = "CET")
## Inspect the data types
str(energy79)

#try total, can calculate the % of each submeter 
energy79 <- mutate(energy79, TotalSub= Kitchen+ Laundry+ Temp)
#energy79[energy79 == "TotalSub"] <- "TotalSub"
#names(energy79)[names(energy79) == 'TotalSub'] <- 'TotalSub'

plot(energy79$DateTime, energy79$TotalSub)

## Create "year" attribute with (library) lubridate
energy79$year <- year(energy79$DateTime)
energy79$quarter <- quarter(energy79$DateTime)
energy79$month   <- month(energy79$DateTime)
energy79$week    <- week(energy79$DateTime)
energy79$wday    <- wday(energy79$DateTime) 
energy79$day     <- day(energy79$DateTime)
energy79$hour    <- hour(energy79$DateTime)
energy79$minute  <- minute(energy79$DateTime)

# set Weeday Attributes
energy79$wday <- weekdays(energy79$DateTime)
energy79$wday <- factor(energy79$wday, 
  levels = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
summary(energy79$wday)
#############
#let's play #
#############

#exploring and plotting 
hist(energy79$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
hist(energy79$TotalSub, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
plot(energy79$TotalSub ~ energy79$DateTime, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")
# 2008 Aug-Sep got a big gap 
filter(energy79, energy79$DateTime >= "2008-01-01", energy79$DateTime <= "2008-12-31")
plot(energy79$TotalSub ~ energy79$DateTime >= "2008-01-01" & energy79$DateTime <= "2008-12-31")

ggplot(energy79,aes(y=TotalSub,x=DateTime))+
  geom_point() # total plot 
ggplot(energy79,aes(y=TotalSub,x=year))+
  geom_point() # total plot 

################### pure exploration, with subset examples 
######## Month plots
monthly <- energy79 %>% group_by(month) %>% summarise(TotalSub = sum(TotalSub), submeter1=sum(), submeter2=sum(Laundry), submeter3=sum(Temp))
Dec.Month <- subset(energy79, Date >= "2007-12-01" & Date <= "2007-12-31")
plot(Dec.Month$DateTime, Dec.Month$TotalSub)
ggplot(Dec.Month,aes(y=TotalSub,x=DateTime))+
  geom_point()
ggplot(Dec.Month,aes(y=Kitchen,x=DateTime))+
  geom_point()
ggplot(Dec.Month,aes(y=Laundry,x=DateTime))+
  geom_point()
ggplot(Dec.Month,aes(y=Temp,x=DateTime))+
  geom_point()

# filter 2008 
f2008 <-  filter(energy79, year== "2008")
summary(f2008)
plot(f2008$TotalSub ~ f2008$DateTime, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")
# look closer 
fm1<-ggplot(f2008, aes(month,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per weekday
fm2<-ggplot(f2008, aes(month,Kitchen))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter1
fm3<-ggplot(f2008, aes(month,Laundry))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter2
fm4<-ggplot(f2008, aes(month,Temp))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter3
ggarrange(fm1, fm2, fm3, fm4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")

# filter 2008 Aug 
f2008.8 <-  filter(energy79, year== "2008", month== "8")
summary(f2008.8)
plot(f2008$TotalSub ~ f2008$DateTime, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")
# filter 2007 Apr28
f2007.0428 <-  filter(energy79, year== "2007", month== "4", day== "28")
plot(f2007.0428$TotalSub ~ f2007.0428$DateTime)
# look closer 
fm1<-ggplot(f2008.8, aes(day,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per weekday
fm2<-ggplot(f2008.8, aes(day,Kitchen))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter1
fm3<-ggplot(f2008.8, aes(day,Laundry))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter2
fm4<-ggplot(f2008.8, aes(day,Temp))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter3
ggarrange(fm1, fm2, fm3, fm4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")

# year comparison in week days 
f2007 <-  filter(energy79, year== "2007")
f2008 <-  filter(energy79, year== "2008")
f2009 <-  filter(energy79, year== "2009")
plot(f2008$TotalSub ~ f2008$DateTime, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")

summarise(f2007, mean(Global_active_power), mean(TotalSub), mean(Kitchen), mean(Laundry), mean(Temp))
summarise(f2008, mean(Global_active_power), mean(TotalSub), mean(Kitchen), mean(Laundry), mean(Temp))
summarise(f2009, mean(Global_active_power), mean(TotalSub), mean(Kitchen), mean(Laundry), mean(Temp))
summarise(energy79, mean(Global_active_power))

# TotalSub over year 
to7<-ggplot(f2007, aes(month,TotalSub))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,6e+05) # per weekday
to8<-ggplot(f2008, aes(month,TotalSub))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,6e+05) # per weekday
to9<-ggplot(f2009, aes(month,TotalSub))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,6e+05) # per weekday
ggarrange(to7, to8, to9,  
          labels = c("2007", "2008","2009"),
          ncol = 3, nrow = 1, align = "h")
#Kitchen over year 
ki7<-ggplot(f2007, aes(month,Kitchen))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,80000) # per weekday for submeter1
ki8<-ggplot(f2008, aes(month,Kitchen))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,80000) # per weekday for submeter1
ki9<-ggplot(f2009, aes(month,Kitchen))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,80000) # per weekday for submeter1
ggarrange(ki7, ki8, ki9,  
          labels = c("2007", "2008","2009"),
          ncol = 3, nrow = 1, align = "hv")
#Laundry over year 
ld7<-ggplot(f2007, aes(month,Laundry))+
  geom_bar(stat="identity")+theme_classic()+
#  geom_hline(aes(yintercept = mean(Laundry)),col='red',size=2)+
  ylim(0,110000)
ld8<-ggplot(f2008, aes(month,Laundry))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,110000) # per weekday for submeter2
ld9<-ggplot(f2009, aes(month,Laundry))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,110000) # per weekday for submeter2
ggarrange(ld7, ld8, ld9,  
          labels = c("2007", "2008","2009"),
          ncol = 3, nrow = 1, align = "hv")
#Temp over year 
tm7<-ggplot(f2007, aes(day,Temp))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,150000) # per weekday for submeter3
tm8<-ggplot(f2008, aes(day,Temp))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,150000) # per weekday for submeter3
tm9<-ggplot(f2009, aes(day,Temp))+
  geom_bar(stat="identity")+theme_classic()+
  ylim(0,150000) # per weekday for submeter3
ggarrange(tm7, tm8, tm9,  
          labels = c("2007", "2008","2009"),
          ncol = 3, nrow = 1, align = "hv")

# Week plots
Random.week <- subset(energy79, Date >= "2007-04-01" & Date <= "2007-04-07")
plot(Random.week$DateTime, Random.week$TotalSub) 
plot(Random.week$DateTime, Random.week$Kitchen)
plot(Random.week$DateTime, Random.week$Laundry)
plot(Random.week$DateTime, Random.week$Temp)

# Day plots # careful with == double 
Random.day <- subset(energy79, Date =="2009-02-01")
plot(Random.day$DateTime, Random.day$TotalSub)
plot(Random.day$DateTime, Random.day$Kitchen)
plot(Random.day$DateTime, Random.day$Laundry)
plot(Random.day$DateTime, Random.day$Temp)

####################################thiago 
######## Hour plot in a day 
hourly <- energy79 %>% group_by(hour) %>% summarise(TotalSub = sum(TotalSub), Kitchen=sum(Kitchen),
                                                    Laundry=sum(Laundry), Temp=sum(Temp))
h1<- ggplot(hourly, aes(hour,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per hour
h2<- ggplot(hourly, aes(hour,Kitchen))+
  geom_bar(stat="identity")+theme_classic() 
h3<- ggplot(hourly, aes(hour,Laundry))+
  geom_bar(stat="identity")+theme_classic() 
h4<- ggplot(hourly, aes(hour,Temp))+
  geom_bar(stat="identity")+theme_classic() 

library(ggpubr) # Temp is dominant to TotalSub 
ggarrange(h1, h2, h3, h4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")
plot(hourly$TotalSub ~ hourly$hour, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")

#### laundry hours weird
# sum(energy79$Laundry)/1000/3
mean(energy79$Kitchen)

######## Weekday plots in 1 week, 7 and 1 is weekend 
weekdaily <- energy79 %>% group_by(wday) %>% summarise(TotalSub = sum(TotalSub), Kitchen=sum(Kitchen),
                                                       Laundry=sum(Laundry), Temp=sum(Temp))
summary(energy79$wday)
w1<-ggplot(weekdaily, aes(wday,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per weekday
w2<-ggplot(weekdaily, aes(wday,Kitchen))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter1
w3<-ggplot(weekdaily, aes(wday,Laundry))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter2
w4<-ggplot(weekdaily, aes(wday,Temp))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter3
ggarrange(w1, w2, w3, w4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")
plot(weekdaily$TotalSub ~ weekdaily$wday, ylab = "Total House Consumption(watt/h)", xlab = "", type = "l")

lines(weekdaily$Kitchen ~ weekdaily$wday, col = 'Red')
lines(weekdaily$Laundry ~ weekdaily$wday, col = 'Blue')
lines(weekdaily$Temp ~ weekdaily$wday, col = 'Yellow')
legend( col = c("black", "red", "blue", "yellow"), legend = c("Total", "Kitchen", "Laundry", "Temp"), lwd = 1)

############### Day plots in 1 month 
daily <- energy79 %>% group_by(day) %>% summarise(TotalSub = sum(TotalSub), Kitchen=sum(),
                                                       Laundry=sum(Laundry), Temp=sum(Temp))
d1<-ggplot(daily, aes(day,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per weekday
d2<-ggplot(daily, aes(day,Kitchen))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter1
d3<-ggplot(daily, aes(day,Laundry))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter2
d4<-ggplot(daily, aes(day,Temp))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter3
ggarrange(d1, d2, d3, d4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")

############### Month plots in 1 year 
monthly <- energy79 %>% group_by(month) %>% summarise(TotalSub = sum(TotalSub), Kitchen=sum(Kitchen),
                                                  Laundry=sum(Laundry), Temp=sum(Temp))
m1<-ggplot(monthly, aes(month,TotalSub))+
  geom_bar(stat="identity")+theme_classic() # per weekday
m2<-ggplot(monthly, aes(month,Kitchen))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter1
m3<-ggplot(monthly, aes(month,Laundry))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter2
m4<-ggplot(monthly, aes(month,Temp))+
  geom_bar(stat="identity")+theme_classic() # per weekday for submeter3
ggarrange(m1, m2, m3, m4,  
          labels = c("All", "Kitchen","Laundry","Temp"),
          ncol = 2, nrow = 2, align = "h")

############ Scatter plots 
hour_weekday <- energy79 %>% group_by(hour, wday) %>% summarise(TotalSub = sum(TotalSub), Kitchen=sum(),
                                                       Laundry=sum(Laundry), Temp=sum(Temp))
hour_month <- energy79 %>% group_by(hour, month) %>% summarise(TotalSub = sum(TotalSub), submeter1=sum(),
                                                               submeter2=sum(Laundry), submeter3=sum(Temp))
hour_quarter <- energy79 %>% group_by(hour, quarter) %>% summarise(TotalSub = sum(TotalSub), submeter1=sum(),
                                                                   submeter2=sum(Laundry), submeter3=sum(Temp))
ggplot(data=hour_weekday) + 
  geom_point(mapping = aes(x = hour, y = wday, size = TotalSub))+theme_classic()
ggplot(data=hour_month) + 
  geom_point(mapping = aes(x = hour, y = month, size = TotalSub))+theme_classic()
ggplot(data=hour_quarter) + 
  geom_point(mapping = aes(x = hour, y = quarter, size = TotalSub))+theme_classic()

