library(readxl)
GHProject_Dataset <- read_excel("GHProject_Dataset.xlsx") #load data
View(GHProject_Dataset)

attach(GHProject_Dataset)

summary(GHProject_Dataset) #summary for each column
names(GHProject_Dataset) #column names
sapply(GHProject_Dataset, class) #class for each column

#histograms for numeric variables
hist(LOSHours)
hist(LOSDays2)
hist(Is30DayReadmit)
hist(MEWS)
hist(Cindex)
hist(Evisit)
hist(ICU_Flag)
hist(AgeYear)
hist(BMI)
hist(BPSYSTOLIC)
hist(O2SAT)
hist(TEMPERATURE)
hist(HEARTRATE)
hist(RESPIRATIONRATE)
hist(BPDIASTOLIC)
hist(FacilityZip)


#what are the inputs of char variables?
unique(Race)
unique(Religion)
unique(PostalCode)
unique(MaritalStatus)
unique(FacilityName)
unique(InsuranceType)
