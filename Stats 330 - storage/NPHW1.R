#Forecast/predict annual visitors to Washington Monument, Jefferson Memorial, Wright Bros Memorial, and Acadia.
# Data
# create a dataframe called Washington Monument
#»¿Title,Bookmark
#WashingtonMonument,Bookmark this report: https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Annual%20Park%20Recreation%20Visitation%20(1904%20-%20Last%20Calendar%20Year)
# retrieved on Mon Jan 13 2018

washington<-read.csv("C:/Users/mike1/Downloads/Annual Park Recreation Visitation %281904 - Last Calendar Year%29 (3).csv") 
washington

#remove the commas from the numbers
washington$RecreationVisitors<-as.numeric(gsub(',','',washington$RecreationVisitors))
mean(washington$RecreationVisitors)

#change to "Visitors (in millions)"
washington$RecreationVisitors<-washington$RecreationVisitors/10^6

#Plot time series for annual visitors
plot(RecreationVisitors~Year, data = washington, type = 'b',
     ylab = "Washington Monument NP Annual Visitors(in millions)")

plot(RecreationVisitors~Year, data = zion, type='b',
     ylab = "Zion NP Annual Visitors (in millions)")
tail(zion)


#C:/Users/mike1/Downloads/Annual Park Recreation Visitation %281904 - Last Calendar Year%29 (3).csv