# Set working directory
setwd('/Users/Ameet/Box Sync/Ameet/GitHub/R-Projects/NY Traffic')

# Read Traffic collisions CSV file
traffic=read.csv('/Users/Ameet/Box Sync/Ameet/Classes/R/traffic/NYPD_Motor_Vehicle_Collisions.csv')


# Review the file structure
str(traffic)

# View a few rows
View(head(traffic))

# Since the major contributing factors are specified in Column CONTIRBUTING.FACTOR.1 we can take 
# this as the reason for the accident.
traffic$CONTRIBUTING.FACTOR=traffic$CONTRIBUTING.FACTOR.VEHICLE.1


# Check the different type of vehicles that are involved in an accident
table(traffic$VEHICLE.TYPE.CODE.1)

# Combine the vehicles to a smaller set of categories
# 1. Two wheeler	<- BICYCLE,	MOTORCYCLE,	SCOOTER,	PEDICAB
# 2. Emergency vehicles <-	AMBULANCE,	FIRE TRUCK 
# 3. Commerical Vehicles <-	LARGE COM VEH(6 OR MORE TIRES), SMALL COM VEH(4 TIRES),	DLIVERY VEHICLE,	VAN
# 4. Taxi <-	TAXI
# 5. Passenger vehicle<-	PASSENGER VEHICLE,	SPORT UTILITY / STATION WAGON,	PICK-UP TRUCK 
# 6. Public Transport <-	BUS
# 7. Other <-	OTHER	
# 8. Unkown <- UNKNOWN, ""

levels(traffic$VEHICLE.TYPE.CODE.5)
levels(traffic$VEHICLE.TYPE.CODE.1)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.2)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.3)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.4)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')
levels(traffic$VEHICLE.TYPE.CODE.5)=c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')

# Review changes
levels(traffic$VEHICLE.TYPE.CODE.1)

# A few of the longitude and latitude values are missing. However the street names , cross street names are provided. 
# Using ggmap get approximate longitude and lattitude information based on street details
library('ggmap')


sort(table(traffic$LATITUDE),decreasing = TRUE)
sort(table(traffic$LONGITUDE),decreasing = TRUE)

# Looking at the data, the CONTRIBUTING.FACTOR.VEHICLE.1 seems to be the dominant reason for accident.
# Narowing down this to a more generalized category.
levels(traffic$CONTRIBUTING.FACTOR.VEHICLE.1)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.1))
View(traffic)

# Check to see if secondary reason is specified if the primary is missing. This shows that is the primary reason is
# either blank or "Unspecified" then the secondary ( or further) columns are also blank
parameters=c("","Unspecified","Other Vehicular")
#########  View(traffic[(traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular' & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.2 %in% parameters)),])
NROW(traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular',]$CONTRIBUTING.FACTOR.VEHICLE.1)

traffic[(traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular' & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.2 %in% parameters)),]$CONTRIBUTING.FACTOR.VEHICLE.1=traffic[(traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular' & !(traffic$CONTRIBUTING.FACTOR.VEHICLE.2 %in% parameters)),]$CONTRIBUTING.FACTOR.VEHICLE.2

NROW(traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular',]$CONTRIBUTING.FACTOR.VEHICLE.1)

# Narrow the accident causes to a smaller group
# “Unsafe driving” -  "Aggressive Driving/Road Rage”, "Following Too Closely”, "Unsafe Speed”, "Unsafe Lane Changing”,"Backing Unsafely"
# “Dizzy Driving” - "Drugs (Illegal)”, "Alcohol Involvement”,"Prescription Medication”
# “Driver distraction” - "Fell Asleep" , "Fatigued/Drowsy”, "Driver Inattention/Distraction”,  "Lost Consciousness”, "Illness” , "Outside Car Distraction”, "Passenger Distraction”,"Reaction to Other Uninvolved Vehicle"
# “Use of electronic devices” - "Cell Phone (hands-free)”, "Other Electronic Device”, "Cell Phone (hand-held)"
# “Rules violations” - "Traffic Control Disregarded”,  "Turning Improperly”, "Failure to Yield Right-of-Way”, "Failure to Keep Right”, "Passing or Lane Usage Improper"
# “Mechanical Failure” - "Accelerator Defective”, "Brakes Defective”, "Tire Failure/Inadequate”, "Steering Failure”, "Headlights Defective”, "Tow Hitch Defective" 
# “Unspecified” - “”, "Unspecified”,  
# “NY Infrastructure issues” - "Shoulders Defective/Improper”, "Traffic Control Device Improper/Non-Working”, "Obstruction/Debris”, "Other Lighting Defects”, "Lane Marking Improper/Inadequate”, "Pavement Slippery”, "Pavement Defective"
# “Miscellaneous” -"Other Vehicular”, "Oversized Vehicle”,"Pedestrian/Bicyclist/Other Pedestrian Error/Confusion”,"Physical Disability"
# “Restricted view” - "View Obstructed/Limited”,"Windshield Inadequate”, "Glare”

levels(traffic$CONTRIBUTING.FACTOR.VEHICLE.1)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.1))
View(traffic[(traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Vehicular'),])
View(traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Other Electronic Device',])
View(traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1 %in% 'Physical Disability',])

CONTRIBUTING.FACTOR.LEVELS= c('Unknown','Emergency Vehicles','Two Wheelers','Public Transport','Emergency Vehicles','Commerical Vehicles','Commerical Vehicles','Two Wheelers','Other','Passenger Vehicles','Two Wheelers','Passenger Vehicles','Two Wheelers','Commerical Vehicles','Passenger Vehicles','Taxi','Unknown','Commerical Vehicles')

levels(traffic$CONTRIBUTING.FACTOR.VEHICLE.1)=



View(traffic)

str(traffic)

levels(traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1=='Unspecified',]$CONTRIBUTING.FACTOR.VEHICLE.2)

traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1=='Unspecified'&& traffic$CONTRIBUTING.FACTOR.VEHICLE.2!='Unspecified',17]

sort(table(traffic$LONGITUDE,traffic$LATITUDE),decreasing = TRUE)

sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.1),decreasing = TRUE)

sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.2),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.3),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.4),decreasing = TRUE)
sort(table(traffic$CONTRIBUTING.FACTOR.VEHICLE.5),decreasing = TRUE)


trafficTemp=traffic[0,]
trafficTemp$CONTRIBUTING.FACTOR.VEHICLE=traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1==traffic$CONTRIBUTING.FACTOR.VEHICLE.2,19]

#traffic[traffic$CONTRIBUTING.FACTOR.VEHICLE.1==traffic$CONTRIBUTING.FACTOR.VEHICLE.2,19]<-
