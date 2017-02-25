# Set working directory
setwd('/Users/Ameet/Ameet/Classes/R/traffic')

# Read Traffic collisions CSV file
traffic=read.csv('NYPD_Motor_Vehicle_Collisions.csv')


# Review the file structure
str(traffic)

# View a few rows
View(head(traffic))

# Since the major contributing factors are specified in Column CONTIRBUTING.FACTOR.1 we can take 
# this as the reason for the accident.
traffic$CONTRIBUTING.FACTOR=traffic$CONTRIBUTING.FACTOR.VEHICLE.1

# Check the different type of vehicles that are involved in an accident
x=table(traffic$VEHICLE.TYPE.CODE.1)

# Combine the vehicles to a smaller set of categories
# Two wheeler	<- BICYCLE,	MOTORCYCLE,	SCOOTER,	PEDICAB
# Emergency vehicles <-	AMBULANCE,	FIRE TRUCK 
# Commerical Vehicles <-	LARGE COM VEH(6 OR MORE TIRES), SMALL COM VEH(4 TIRES),	DLIVERY VEHICLE,	VAN
# Taxi <-	TAXI
# Passenger vehicle<-	PASSENGER VEHICLE,	SPORT UTILITY / STATION WAGON,	PICK-UP TRUCK 
# Public Transport <-	BUS
# Other <-	OTHER	
# Unkown <- UNKNOWN, ""

levels(traffic$VEHICLE.TYPE.CODE.1)
levels(traffic$VEHICLE.TYPE.CODE.1)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.2)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.3)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.4)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.5)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')

# Get approximate lon and lat info based on street details
library('ggmap')




sort(table(traffic$LATITUDE),decreasing = TRUE)
sort(table(traffic$LONGITUDE),decreasing = TRUE)

traffic$CONTRIBUTING.FACTOR[traffic$CONTRIBUTING.FACTOR %in% "Unspecified" & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.2  %in% 'Unspecified')]<-traffic$CONTRIBUTING.FACTOR.VEHICLE.2
traffic$CONTRIBUTING.FACTOR[traffic$CONTRIBUTING.FACTOR %in% "Unspecified" & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.3  %in% 'Unspecified')]<-traffic$CONTRIBUTING.FACTOR.VEHICLE.2
traffic$CONTRIBUTING.FACTOR[traffic$CONTRIBUTING.FACTOR %in% "Unspecified" & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.4  %in% 'Unspecified')]<-traffic$CONTRIBUTING.FACTOR.VEHICLE.2
traffic$CONTRIBUTING.FACTOR[traffic$CONTRIBUTING.FACTOR %in% "Unspecified" & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.5  %in% 'Unspecified')]<-traffic$CONTRIBUTING.FACTOR.VEHICLE.2



View(traffic)

str(traffic)



traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1=='Unspecified'&& traffic$CONTRIBUTING.FACTOR.VEHICLE.2!='Unspecified',17]

sort(table(traffic$LONGITUDE,traffic$LATITUDE),decreasing = TRUE)

sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.1),decreasing = TRUE)

sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.2),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.3),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.4),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.5),decreasing = TRUE)


trafficTemp=traffic[0,]
trafficTemp$CONTRIBUTING.FACTOR.VEHICLE=traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1==traffic$CONTRIBUTING.FACTOR.VEHICLE.2,19]

traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1==traffic$CONTRIBUTING.FACTOR.VEHICLE.2,19]<-
