
library(dplyr)

# # read and clean current year crime data
# nyc_crime_current <- read.csv(file="NYPD_Complaint_Data_Current__Year_To_Date_.csv", header=TRUE, sep=",")
# nyc_crime_current <- subset(nyc_crime_current, select = c(OFNS_DESC, LAW_CAT_CD, CMPLNT_FR_DT, Latitude, Longitude, X_COORD_CD, Y_COORD_CD))
# nyc_crime_current <- nyc_crime_current[as.Date(nyc_crime_current$CMPLNT_FR_DT, format = "%m/%d/%Y") >= as.Date("01/01/2019", format = "%m/%d/%Y"),]
# 
# # read and clean historic crime data for 2015-2018
# nyc_crime_historic <- read.csv(file="NYPD_Complaint_Data_Historic.csv", header=TRUE, sep=",")
# nyc_crime_historic <- subset(nyc_crime_historic, select = c(OFNS_DESC, LAW_CAT_CD, CMPLNT_FR_DT, Latitude, Longitude, X_COORD_CD, Y_COORD_CD))
# nyc_crime_historic <- nyc_crime_historic[(as.Date(nyc_crime_historic$CMPLNT_FR_DT, format = "%m/%d/%Y") >= as.Date("01/01/2015", format = "%m/%d/%Y")) & (as.Date(nyc_crime_historic$CMPLNT_FR_DT, format = "%m/%d/%Y") < as.Date("01/01/2019", format = "%m/%d/%Y")),]
# 
# # merge current year and historic crime data
# nyc_crime <- rbind(nyc_crime_current, nyc_crime_historic)
# 
# # clean NA
# unique(nyc_crime[is.na(nyc_crime$LAW_CAT_CD),]) # Where LAW_CAT_CD is NA, all other features are NA
# nyc_crime <- nyc_crime[complete.cases(nyc_crime),]
# 
# # save as csv file
# write.csv(nyc_crime, "nyc_crime.csv", row.names=F)

nyc_crime <- read.csv(file = "nyc_crime.csv", header = TRUE, sep = ",")
nrow(nyc_crime)
head(nyc_crime)

# felony <- filter(nyc_crime, LAW_CAT_CD == "FELONY")
# misdemeanor <- filter(nyc_crime, LAW_CAT_CD == "MISDEMEANOR")
# violation <- filter(nyc_crime, LAW_CAT_CD == "VIOLATION")
# 
# write.csv(felony, "felony.csv", row.names=F)
# write.csv(misdemeanor, "misdemeanor.csv", row.names=F)
# write.csv(violation, "violation.csv", row.names=F)

felony <- read.csv(file = "felony.csv", header = TRUE, sep = ",")
misdemeanor <- read.csv(file = "misdemeanor.csv", header = TRUE, sep = ",")
violation <- read.csv(file = "violation.csv", header = TRUE, sep = ",")

# # read and clean current year shooting data
# nyc_shooting_current <- read.csv(file="NYPD_Shooting_Incident_Data__Year_To_Date_.csv", header=TRUE, sep=",")
# nyc_shooting_current <- subset(nyc_shooting_current, select = c(OCCUR_DATE, Latitude, Longitude, X_COORD_CD, Y_COORD_CD))
# nyc_shooting_current <- nyc_shooting_current[as.Date(nyc_shooting_current$OCCUR_DATE, format = "%m/%d/%Y") >= as.Date("01/01/2019", format = "%m/%d/%Y"),]
# 
# # read and clean historic shooting data for 2015-2018
# nyc_shooting_historic <- read.csv(file="NYPD_Shooting_Incident_Data__Historic_.csv", header=TRUE, sep=",")
# nyc_shooting_historic <- subset(nyc_shooting_historic, select = c(OCCUR_DATE, Latitude, Longitude, X_COORD_CD, Y_COORD_CD))
# nyc_shooting_historic <- nyc_shooting_historic[(as.Date(nyc_shooting_historic$OCCUR_DATE, format = "%m/%d/%Y") >= as.Date("01/01/2015", format = "%m/%d/%Y")) & (as.Date(nyc_shooting_historic$OCCUR_DATE, format = "%m/%d/%Y") < as.Date("01/01/2019", format = "%m/%d/%Y")),]
# 
# # merge current year and historic shooting data
# nyc_shooting <- rbind(nyc_shooting_current, nyc_shooting_historic)
# nyc_shooting <- nyc_shooting[complete.cases(nyc_shooting),]
# 
# # # save as csv file
# write.csv(nyc_shooting, "nyc_shooting.csv", row.names=F)

nyc_shooting <- read.csv(file="nyc_shooting.csv", header=TRUE, sep=",")