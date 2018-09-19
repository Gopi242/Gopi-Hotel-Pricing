#Using Boruta
library(Boruta)
hotel1.df<-na.omit(hotel.df)
xvar<-Boruta(RoomRent~CityName + Population + IsMetroCity + IsTouristDestination + IsWeekend +
               IsNewYearEve + Date + HotelName + StarRating + Airport + HotelAddress + HotelPincode
             + HotelDescription + FreeWifi + FreeBreakfast + HasSwimmingPool, data= hotel1.df)
print(xvar)

xvar<-Boruta(RoomRent~.-X - CityName - HotelAddress - HotelDescription, data= hotel1.df, doTrace = 2)
print(xvar)