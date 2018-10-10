#Forecast/predict monthly US residential energy consumption for the next 2 years.
  
#Data: from the US Department of Energy
#Create a dataframe named energy
#Energy: 
#Retrieved on Mon Jan 15, 2018
#US residential energy consumption
#http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption

data1<-read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")
  
#subset to TERCBUS Total Energy Consumed by the Residential Sector
data2<-subset(data1,MSN=="TERCBUS")
  
#subset to "your lifetime"
data3<-subset(data2,data2$YYYYMM>199607)
  
#remove yearly total (coded "month 13", every 13th obs)
data4<-subset(data3,data3$YYYYMM%%100 !=13)
  
energy<-data4$Value
  
#Plot the time series for US residential energy consumption
plot(Value~MSN, data=energy, type='b')
tail(energy)
  
#There does appear to be a monthly pattern. This likely means that more energy is used
#during certain times of the year. During the cold temperatures in the winter, heat is needed
#and used more often.
  
#Fit ARIMA model and report the parameter estimates and report the parameter estimates
#and standard errors.
  
#Graphics
#Notice that in addition to an increasing trend, there is also a seasonal trend (12 month cycle)
  
plot(1:T, energy, type="b",
       ylab="US Residential Energy(trillion Btu)",
       xlab="Month")
       
#Analysis
library(astsa)
       
#Estimate the Arima (1,1,1)x(1,1,1)_12
energy.out<-sarima(energy,1,1,1,1,1,1,12)
       
#table of estimates
energy.out$ttable
  
#Predictions for next 2 YEARS
energy.future<-sarima.for(energy,n.ahead=27,1,1,1,1,1,1,12)
  
#Computer 95% prediction interval
L<-energy.future$pred - 2 * energy.future$se
U<-energy.future$pred + 2 * energy.future$se
                                                                        
#table of predictions
cbind(energy.future$pred,L,U)
  
#Graphic
plot(Value~MSN, data = energy, type = "b",
    ylab = "US Residential Energy(trillion Btu)")
lines(2017:2019, exp(energy.future$pred),col="darkorange2",type = "b",pch = 19)
lines(2017:2019, exp(L),col="darkorange2",lty =2) 
lines(2017:2019, exp(U),col="darkorange2",lty =2)                                                                                   
  
#Research task:
#Predict monthly US residential energy consumption for the next 2 years.

#data features: Time series, correlation from past appears will continue in the next months.
#One of the main strengths of the research and analysis was that the data was mostly linear, or additive.
#This means that the data had no strong outliers, so a filtration and log transformation were not necessary.
#Though we did subset the data for our lifetimes, it still included most of the data initially procured since
#1991. Also, the data accounted for energy from every major appliance, such as TV, kitchen utilities, and dryers.

#Analysis Weaknesses
#May not have calculated for lurking variables - recessions, increase in technologic consumption,
#more buildings being built.

#Challenge:
#Pollution measurement: predict how much harmful pollution will be contributed to the United States 
#for the next two years as a result of emissions testing data.
#Data starting in 1990 (https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/publications/national_transportation_statistics/html/table_04_43.html).

