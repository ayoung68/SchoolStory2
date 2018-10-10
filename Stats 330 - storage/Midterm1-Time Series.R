# webscraper for Q1 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q1 box office url
q1boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1")
# read webpage and store in memory
q1boxoffice.webpage<-htmlParse(q1boxoffice.url)
# create R dataset from webpage contents
q1boxoffice<-readHTMLTable(q1boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q1boxoffice<-q1boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q1boxoffice)<-c("year","gross")

# create the variable qtr
q1boxoffice$qtr<-1
q2boxoffice$qtr<-2
q3boxoffice$qtr<-3
q4boxoffice$qtr<-4

# stack the dataframes
boxoffice<-rbind(q1boxoffice,q2boxoffice,q3boxoffice,q4boxoffice)

# webscraper for Q2 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q2 box office url
q2boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2")
# read webpage and store in memory
q2boxoffice.webpage<-htmlParse(q2boxoffice.url)
# create R dataset from webpage contents
q2boxoffice<-readHTMLTable(q2boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q2boxoffice<-q2boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q2boxoffice)<-c("year","gross")


# webscraper for Q3 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q3 box office url
q3boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3")
# read webpage and store in memory
q3boxoffice.webpage<-htmlParse(q3boxoffice.url)
# create R dataset from webpage contents
q3boxoffice<-readHTMLTable(q3boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q3boxoffice<-q3boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q3boxoffice)<-c("year","gross")


# webscraper for Q4 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q4 box office url
q4boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4")
# read webpage and store in memory
q4boxoffice.webpage<-htmlParse(q4boxoffice.url)
# create R dataset from webpage contents
q4boxoffice<-readHTMLTable(q4boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q4boxoffice<-q4boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q4boxoffice)<-c("year","gross")

# sort the data by both year and qtr
boxoffice<-boxoffice[order(boxoffice$year,boxoffice$qtr),]

# remove the data for current quarter(2018, 1) since it is 'gross to date.'
boxoffice<-(boxoffice[-145,])
tail(boxoffice)

boxoffice$seq<-(1:144)
plot(gross~seq, data=boxoffice, type='b')

#2 After observing the the quarterly total gross box office revenue, it appears there is a quarterly trend.
# The revenue was highest during the second and fourth quarters. One likely business/economic cause
# for this effect is the fact that many viewers choose to spend more time watching movies in the 
# theater around the holiday season (October-December). Movie producers likely put the biggest movies
# out when families are together and spend money in the theater. May is also a common month for movies
# to come out.

#3 Filtering to the recent past or a transformation is not necessary because there appears to be
# addivity in the dataset. There is no evidence of a multiplicative effect, so the data seems 
# reasonable as is.

#Filter the data 
boxoffice.filter<-subset(boxoffice, year >= 2000)

#Analysis
library(astsa)

#Estimate the Arima (1,1,1)x(1,1,1)_12
box.out<-sarima(boxoffice.filter$gross,1,1,1,1,1,1,4)

#table of estimates
box.out$ttable

#Predictions for next 3 YEARS
box.future<-sarima.for(boxoffice.filter$gross,n.ahead=12,1,1,1,1,1,1,4)

#Compute 95% prediction interval
L<-box.future$pred - 2 * box.future$se
U<-box.future$pred + 2 * box.future$se

#table of predictions
cbind(box.future$pred,L,U)

plot(gross~seq, data = boxoffice, type = "b",
     main = "Quarterly Box Office Revenue and Prediction",
     ylab = "Gross Income (in $ millions)", xlab = "Quarters", xlim=c(80,160), ylim=c(1500,4000))
lines(145:156, box.future$pred ,col="darkorange2",type = "b",pch = 19)
lines(145:156, (L),col="darkorange2",lty =2) 
lines(145:156, (U),col="darkorange2",lty =2)  
