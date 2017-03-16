readDissFromFile <- function(){
##  con <- gzfile("diss.rds")
  tra <- readRDS("diss.rds")
  tra
}


readDataFrame <- function(){
  raw = read_csv("Accidents_2015.csv" )
  raw
}

dataFrameToTransaction <- function(raw){
### Data Cleaning
##raw$Accident_Index <- factor(raw$Accident_Index)
raw$Accident_Index <- NULL

##raw$Location_Easting_OSGR <- as.factor(as.numeric(raw$Location_Easting_OSGR))
##raw$Location_Northing_OSGR <- as.factor(as.numeric(raw$Location_Northing_OSGR))
raw$Location_Easting_OSGR <- NULL
raw$Location_Northing_OSGR <- NULL

raw$Longitude <- discretize(raw$Longitude ,categories = 50, onlycuts=FALSE)
raw$Latitude <- discretize(raw$Latitude ,categories = 50, onlycuts=FALSE)

raw$Police_Force <- discretize(raw$Police_Force ,categories=4  )
raw$Police_Force <- as.factor(as.numeric(raw$Police_Force))


raw$Accident_Severity <- factor(raw$Accident_Severity,labels=c("Fatal","Serious","Slight"))

##raw$Date <- as.Date(sprintf("%s", raw$Date), format ="%Y%m%d")
raw$Date <- NULL
raw$Time <- NULL
raw$Number_of_Vehicles[raw$Number_of_Vehicles<0 | raw$Number_of_Vehicles > 6 ] <- NA
raw$Number_of_Vehicles <- as.factor(raw$Number_of_Vehicles)

raw$Number_of_Casualties [raw$Number_of_Casualties<0 | raw$Number_of_Casualties>38]
raw$Number_of_Casualties <- as.factor(raw$Number_of_Casualties)

##raw$Day_of_Week[raw$Day_of_Week <1 | raw$Day_of_Week>7] <- NA
##raw$Day_of_Week <- as.factor(raw$Day_of_Week)
raw$Day_of_Week <- NULL

raw$'Local_Authority_(District)' <- NULL
raw$'Local_Authority_(Highway)'<- NULL
raw$'1st_Road_Class' <- as.factor(raw$'1st_Road_Class')
raw$'1st_Road_Number' <- NULL
raw$`2nd_Road_Class` <- as.factor(raw$`2nd_Road_Class`)
raw$`2nd_Road_Number` <- NULL
raw$`Pedestrian_Crossing-Human_Control` <- NULL
raw$`Pedestrian_Crossing-Physical_Facilities` <- NULL


raw$Junction_Detail <- NULL
raw$Junction_Control <- NULL

raw$Road_Type <- as.factor(raw$Road_Type)
raw$Speed_limit <- as.factor(raw$Speed_limit)



##raw$Urban_or_Rural_Area[raw$Urban_or_Rural_Area < 0 | raw$Urban_or_Rural_Area > 2] <- NA
##raw$Urban_or_Rural_Area <- factor(raw$Urban_or_Rural_Area,labels=c("Urban","Rural"))
raw$Urban_or_Rural_Area <- NULL

raw$Weather_Conditions[raw$Weather_Conditions < 1 | raw$Weather_Conditions > 7] <- NA
raw$Weather_Conditions <- factor(raw$Weather_Conditions,labels=c("Fine no high winds","Raining no high winds","Snowing no high winds","Fine + high winds","Raining + high winds","Snowing + high winds","Fog or mist"))

raw$Light_Conditions <- as.factor(raw$Light_Conditions)
raw$Road_Surface_Conditions <- as.factor(raw$Road_Surface_Conditions)

##raw$Special_Conditions_at_Site <- as.factor(raw$Special_Conditions_at_Site)
raw$Special_Conditions_at_Site <- NULL
raw$Carriageway_Hazards <- NULL
raw$Did_Police_Officer_Attend_Scene_of_Accident <- NULL


##raw$LSOA_of_Accident_Location <- as.factor(raw$LSOA_of_Accident_Location)
raw$LSOA_of_Accident_Location <- NULL

prepared <- raw[complete.cases(raw),]
trans = as(prepared, "transactions")
return (trans)
}

