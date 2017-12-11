library(tidyverse)
library(readxl)
library(dplyr)
library(DT)
library(janitor)
library(stringr)
library(lubridate)

GHProject_Dataset <- read_excel("GHProject_Dataset.xlsx") %>% clean_names() #load data

n_occur <- data.frame(table(patientid))
#check the duplicated patients
revisit_patient <- GHProject_Dataset[GHProject_Dataset$patientid %in% n_occur$Var1[n_occur$Freq > 1],]

#remove duplicated patients
ghproject_remove <- GHProject_Dataset[!duplicated(GHProject_Dataset[c('patientid')]),]

##recode gender
ghproject_tidy <- ghproject_remove %>% 
  mutate(gender = ifelse(gender == "Male", "1","0" )) %>% #recode male
  mutate(maritalstatus = ifelse(maritalstatus == "Married","1","0"), #recode marital status
         cindex = recode(cindex, #recode cindex
                         "0" = "0",
                         "1" = "0",
                         "2" = "1",
                         "3" = "1",
                         "4" = "2",
                         "5" = "2"),
         insurancetype = recode(insurancetype, #recode insurance type
                                "NA" = "0",
                                "Private" = "1",
                                "Medicare" = "2",
                                "Medicaid" = "3"),
         mews = recode(mews, #recode mews
                       "NA" = "0",
                       "0" = "1",
                       "1" = "1",
                       "2" = "2",
                       "3" = "2",
                       "4" = "3",
                       "5" = "3",
                       "6"= "4",
                       "7"= "4",
                       "8"= "4",
                       "9"= "4",
                       "10"= "4",
                       "11"= "4",
                       "12"= "4",
                       "13"= "4",
                       "14" = "4"),
         religion = ifelse(religion == "No Affiliation", "0", "1"), #recode religion
         losdays2_log = log(losdays2),
         id = 1:nrow(ghproject_remove)) %>% 
  separate(admitdtm, into= c("day", "month", "year"), sep = ", ", convert = TRUE) %>% #recode admission date to month
  mutate(month = str_replace(month, "[0-9]", ""))%>% 
  mutate(month = str_replace(month, "[0-9]", ""))%>%
  mutate(month = str_replace(month, " ", ""))%>%
  mutate(month = recode(month,
                        "January" = "1",
                        "February" = "2",
                        "March" = "3",
                        "April" = "4",
                        "May" = "5",
                        "June" = "6",
                        "July" = "7",
                        "August" = "8",
                        "September" = "9",
                        "October" = "10",
                        "November" = "11",
                        "December" = "12")) %>% 
  select(-c(day, year, patientid, visitid, postalcode, facilityname, facilityzip, race)) %>% #remove unneeded variables
    na.omit() 

ghproject_tidy$mews <- as.integer(as.character(ghproject_tidy$mews))

ghproject_tidy$cindex <- as.integer(as.character(ghproject_tidy$cindex))

ghproject_tidy$month <- as.integer(as.character(ghproject_tidy$month))

ghproject_tidy$gender <- as.integer(as.character(ghproject_tidy$gender))

ghproject_tidy$religion <- as.integer(as.character(ghproject_tidy$religion))

ghproject_tidy$maritalstatus <- as.integer(as.character(ghproject_tidy$maritalstatus))

ghproject_tidy$insurancetype <- as.integer(as.character(ghproject_tidy$insurancetype))


###

#install.packages("GGally")
library(GGally)
# correlation matrix for numeric variables
ggpairs(ghproject_tidy, columns = c("month", "ageyear", "bmi", "bpsystolic","o2sat","temperature", "heartrate","respirationrate", "bpdiastolic","losdays2_log"))


###

write_csv(ghproject_tidy, "ghproject_saturated.csv")