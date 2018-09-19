# Project Title: Hotel Room Pricing in Indian Market
# NAME: Sai Gopi Krishna Govindarajula
# EMAIL: saigopikrishna.g@gmail.com
# COLLEGE: BITS PILANI

##setting the directory and assigning a variabel to the data frame
setwd("E:/Studies/Sai Gopi Krishna Govindarajula/Udemy/project/Hotel Analysis/Sai Gopi Krishna Govindarajula Project")

#Reading the dataset and creating a data frame
hotelData.df<-read.csv(paste("Cities42.csv",sep = ""))

#Viewing the data
View(hotelData.df)

#Adjusting the dates for consistency using gsub command

hotelData.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("4-Jan-17", "Jan 04 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("4-Jan-16", "Jan 04 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("8-Jan-16", "Jan 08 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("8-Jan-17", "Jan 08 2017", hotelData.df$Date)
hotelData.df$Date<-gsub("18-Dec-16", "Dec 18 2016", hotelData.df$Date)
hotelData.df$Date<-gsub("21-Dec-16", "Dec 21 2016", hotelData.df$Date)
hotelData.df$Date<-gsub("24-Dec-16", "Dec 24 2016", hotelData.df$Date)
hotelData.df$Date<-gsub("25-Dec-16", "Dec 25 2016", hotelData.df$Date)
hotelData.df$Date<-gsub("28-Dec-16", "Dec 28 2016", hotelData.df$Date)
hotelData.df$Date<-gsub("31-Dec-16", "Dec 31 2016", hotelData.df$Date)

#Changing dates to factors for labelling 

hotelData.df$Date<-factor(hotelData.df$Date)
is.factor(hotelData.df$Date)

#Checking the labelling
levels(hotelData.df$Date)

#Analyzing the summary of the data and describing the variables

library(psych)
describe(hotelData.df)

summary(hotelData.df)

#Identifying the most relevent predictor variables by  correlation corrgram
#Taking Y = RoomRent
library(corrgram)

corrgram(hotelData.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of hotelData")
##We can see that HasSwimming, StarRating, HotelCapacity are very well 
                  #correlated to RoomRent. Hence considering them as predictor variables


##Visualizing data for Y as Room rent and X1,X2,X3 as HasSwimmingPool, StarRating and HotelCapacity respectively

#Table for HasSwimmingPool
table(hotelData.df$HasSwimmingPool)
Swim<-table(hotelData.df$HasSwimmingPool)
barplot(Swim,main="Barrplot of Hotel Swimming Pool")

#Table for StarRating
table(hotelData.df$StarRating)
starRating<-table(hotelData.df$StarRating)
barplot(starRating,main = "Barrplot for Star Rating")

#BoxPlot for HotelCapacity
boxplot(hotelData.df$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)

#Scatterplot pair wise for predictor variable

library(car)
#StarRating Vs RoomRent

scatterplot(hotelData.df$StarRating,hotelData.df$RoomRent,main="RoomRent of Hotels  with StarRating",ylab = "RoomRent in INR", xlab="Star rating out of 5",cex=1.1)

#RoomRent Vs HotelCapacity

scatterplot(hotelData.df$HotelCapacity,hotelData.df$RoomRent,main="RoomRent of Hotels  with Hotel capacity",ylab ="RoomRent in INR",xlab="Hotel Capacity in rooms",cex=1.1)

#RoomRent Vs HasSwimmingPool

plot(jitter(hotelData.df$RoomRent),jitter(hotelData.df$HasSwimmingPool),main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)
library(lattice)
bwplot(HasSwimmingPool~RoomRent, data = hotelData.df,main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent" )

#Scatterplot matrix

scatterplotMatrix(
  hotelData.df[
    ,c("RoomRent","HasSwimmingPool","StarRating", "HotelCapacity")], 
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")


#Corrgram of Y, x1, x2, x3

library(corrgram)

xyz<-data.frame(hotelData.df$RoomRent, hotelData.df$HasSwimmingPool, hotelData.df$HotelCapacity, hotelData.df$StarRating)
corrgram(xyz, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel Prices In India")

#Variance-Covariance Matrix for Y, x1, x2, x3

x<-hotelData.df[,c("HasSwimmingPool","StarRating", "HotelCapacity")]
y<-hotelData.df[,c("RoomRent")]
cor(x,y)
cov(x,y)
var(x,y)
#Forming a variable which is having RoomRent less than 1 lakh because the outliers effect the average
RoomRent1.df <-hotelData.df[which(hotelData.df$RoomRent<100000),]

#Comparing other factors and their pattern using other trends with roomrent

#Analyzing IsWeekeng effect on RoomRent
table(hotelData.df$IsWeekend)

table1<-table(hotelData.df$IsWeekend)
barplot(table1, main="Distribution of Weekend", xlab="Not weekend(0)         Weekend(1)", col="orange")

#Effect of Isweekend on RoomRent
iw= aggregate(RoomRent ~ IsWeekend, data=hotelData.df,mean)
iw

boxplot(RoomRent~IsWeekend,data=hotelData.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)
#Without extreme outliers 
boxplot(RoomRent~IsWeekend,data=RoomRent1.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)

#Comapring RoomRent on different dates
table(hotelData.df$Date)

library(lattice)
histogram(~Date, data = hotelData.df, main="Distribution of Dates", xlab = "Differnt of Dates", col="Blue")


#Effect of different dates on RoomRent

d = aggregate(RoomRent ~ Date, data = hotelData.df,mean)
d

scatterplot(d$Date,d$RoomRent, main="Scatterplot between Date and RoomRent", xlab="Date", ylab = "Room Rent in Rupees")


boxplot(RoomRent~Date,data=hotelData.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
##Without extreme outliers
boxplot(RoomRent~Date,data=RoomRent1.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))

#Analyzing IsMetroCity effect on RoomRent
table(hotelData.df$IsMetroCity)

table1<-table(hotelData.df$IsMetroCity)
barplot(table1, main="Distribution of IsMetroCity", xlab="Not a Metro city(0)         Is a Metro City(1)", col="blue")

#Effect of IsMetroCity on RoomRent
imc = aggregate(RoomRent ~ IsMetroCity, data = hotelData.df, mean)
imc

boxplot(RoomRent~IsMetroCity,data=hotelData.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers
boxplot(RoomRent~IsMetroCity,data=RoomRent1.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)


#Analyzing IsTouristDestination effect on RoomRent
table(hotelData.df$IsTouristDestination)

table1<-table(hotelData.df$IsTouristDestination)
barplot(table1, main="Distribution of IsToursitDestination", xlab="Not a Tourist Destination(0)         Is a Tourist Destination(1)", col="yellow")

#Effect of IsTouristDestination on RoomRent
itd = aggregate(RoomRent ~ IsTouristDestination, data = hotelData.df, mean)
itd

boxplot(RoomRent~IsTouristDestination,data=hotelData.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers
boxplot(RoomRent~IsTouristDestination,data=RoomRent1.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#Analyzing FreeWifi Vs RoomRent
table(hotelData.df$FreeWifi)
fw<-table(hotelData.df$FreeWifi)
barplot(fw, main="Borplot of FreeWifi",xlab= "FreeWifi" ,col="red")

#Effect of FreeWifi on RoomRent
fw = aggregate(RoomRent ~ FreeWifi, data = hotelData.df, mean)
fw

##With extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=hotelData.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=RoomRent1.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#Analyzing FreeBreakfast Vs RoomRent
table(hotelData.df$FreeWifi)
fw<-table(hotelData.df$FreeBreakfast)
barplot(fw, main="Borplot of FreeBreakfast",xlab= "FreeWifi" ,col="red")


#Effect of FreeBreakfast on RoomRent
fb = aggregate(RoomRent ~ FreeBreakfast, data =hotelData.df, mean)
fb1  = aggregate(RoomRent ~ FreeBreakfast, data =RoomRent1.df, mean)
##Aggregate are affected by outliers a lot in the case of FreeBreakfast on RoomRent
fb
fb1

##With extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=hotelData.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=RoomRent1.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)


#Analyzing Airport distance from hotel effects in what way on RoomRent
summary(hotelData.df$Airport)

boxplot(hotelData.df$Airport, main="Boxplot of Airport",xlab= "Distance of airport from hotel(Km)" ,col="green",horizontal = TRUE)

#Effect of Airport distance on RoomRent

scatterplot(hotelData.df$Airport,hotelData.df$RoomRent, main="Room rent vs. Airport distance", xlab="Airport distance(km)", ylab="Room Rent in rupees ",cex=1.1)

##Hypothesis

#1.Average RoomRent in hotels having swimming pool is more than that which don't have.
t.test(RoomRent~HasSwimmingPool,data = hotelData.df, alternative="less")

#2.Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
t.test(hotelData.df$RoomRent,hotelData.df$StarRating)

#3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide.
t.test(RoomRent~FreeBreakfast, data = hotelData.df, alternative="less")

#4.Average RoomRent in metro cities hotels is more than that of non metro cities.
t.test(RoomRent~IsMetroCity, data = hotelData.df, alternative="less")

#5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
t.test(hotelData.df$RoomRent,hotelData.df$HotelCapacity)

#Generating a multiple linear regression model for RoomRent
#1.
fit1<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity, data = hotelData.df)
summary(fit1)

#Coefficents of the model
fit1$coefficients

#Fitted residuals and values  are checked and the deviation was around 1000 , because of 
#large data points it's not suitable to show those in the output file.

#  Model1:   Hotel Rent = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity
#   b0 = -6896.154,  b1 =  3597.322, b2=2528.885, b3= -15.558



#2.
fit2<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = hotelData.df)
summary(fit2)

#Coefficents of the model
fit2$coefficients


#Model2:    Rent= b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*IsWeekend + b5*IsTouristDestination
#b0 = -1(assumption),  b1 = 1258.9558 , b2=3670.2511, b3= -6.1769 , b4= -509.6479, b5=1053.0394 



#3.
fit3<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport, data = hotelData.df)
summary(fit3)
#Coefficents of the model
fit3$coefficients
# Model3:    Rent = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*Airport
#   b0 = -7288.04830,  b1 =  3522.99002 , b2=2708.40013, b3= -14.77562, b4= 25.34377


