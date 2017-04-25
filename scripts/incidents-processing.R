library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Incidents
# Created by Jenna Daly
# On 04/24/17
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
all_state_csvs <- dir(path_to_raw_data, recursive=T, pattern = "ct.csv") 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

incidents_dist <- data.frame(stringsAsFactors = F)
incidents_dist_noTrend <- grep("trend", all_dist_csvs, value=T, invert=T)
for (i in 1:length(incidents_dist_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", incidents_dist_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:3),]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(incidents_dist_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  incidents_dist <- rbind(incidents_dist, current_file)
}

#Add statewide data
incidents_state <- data.frame(stringsAsFactors = F)
incidents_state_noTrend <- grep("trend", all_state_csvs, value=T, invert=T)
for (i in 1:length(incidents_state_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", incidents_state_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:3),]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  names(current_file)[names(current_file) == 'State'] <- 'District'
  current_file$District <- "Connecticut"
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(incidents_state_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  incidents_state <- rbind(incidents_dist, current_file)
}

#Combine district and state
incidents <- rbind(incidents_dist, incidents_state)

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

incidents_fips <- merge(incidents, districts, by.x = "District", by.y = "District", all=T)

incidents_fips$District <- NULL

incidents_fips<-incidents_fips[!duplicated(incidents_fips), ]

#backfill year
years <- c("2009-2010", 
           "2010-2011",
           "2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years 
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_incidents <- merge(incidents_fips, backfill_years, all=T)

#remove duplicated Year rows
complete_incidents <- complete_incidents[!with(complete_incidents, is.na(complete_incidents$Year)),]

#return blank in FIPS if not reported
complete_incidents$FIPS <- as.character(complete_incidents$FIPS)
complete_incidents[["FIPS"]][is.na(complete_incidents[["FIPS"]])] <- ""

#recode missing data with -6666
complete_incidents[is.na(complete_incidents)] <- -6666

#recode suppressed data with -9999
complete_incidents[complete_incidents == "*"]<- -9999

#reshape from wide to long format
cols_to_stack <- c("Violent Crimes Against Persons",   
                   "School Policy Violations",          
                   "Sexually Related Behavior",         
                   "Personally Threatening Behavior",  
                   "Theft Related Behaviors",           
                   "Physical and Verbal Confrontation", 
                   "Fighting and Battery",             
                   "Property Damage",                   
                   "Weapons",                           
                   "Drugs, Alcohol, Tobacco")

long_row_count = nrow(complete_incidents) * length(cols_to_stack)

complete_incidents_long <- reshape(complete_incidents,
                                        varying = cols_to_stack,
                                        v.names = "Value",
                                        timevar = "Incident Type",
                                        times = cols_to_stack,
                                        new.row.names = 1:long_row_count,
                                        direction = "long"
)

#Rename FixedDistrict to District
names(complete_incidents_long)[names(complete_incidents_long) == 'FixedDistrict'] <- 'District'

#reorder columns and remove ID column
complete_incidents_long <- complete_incidents_long[order(complete_incidents_long$District, complete_incidents_long$Year),]
complete_incidents_long$id <- NULL

#Add Measure Type
complete_incidents_long$`Measure Type` <- "Number"

#Rename Variable columns
complete_incidents_long$`Variable` <- "Incidents"


#Order columns
complete_incidents_long <- complete_incidents_long %>% 
  select(`District`, `FIPS`, `Year`, `Incident Type`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entires for a given district
# test <- complete_incidents_long[,c("District", "Year", "Incident Type")]
# test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_incidents_long,
  file.path(getwd(), "data", "incidents_2010-2016.csv"),
  sep = ",",
  row.names = F
)

