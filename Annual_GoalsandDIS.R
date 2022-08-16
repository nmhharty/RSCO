#creating tables for annual G&O from SPARS and the Diversity Impact Statement (DIS) goals
library(data.table)

#create data table with annual SPARS goals and objectives data
#need to include a table with goal, actual, percent of goal in annual report

##UPDATE THIS SCRIPT TO CHANGE GOALS WHEN/IF ANNAUL G&O CHANGE!!


#creating separate data tables for each year, can append together for grant-to-date summary

#FISCAL YEAR 2019
SPARS_GoalsFY19 = data.table(
  "SPARS Indicator" = c("WD2","T3","S1","R1","AC1"),
  "Goal" = c(50,25,100,40,.75),
  ##update "Actual" based upon SPARS calculation
  "Actual" = c(233,34,351,103,.72)
)
SPARS_GoalsFY19 <- SPARS_GoalsFY19 %>%
  mutate("Percent of Goal Reached" = scales::percent(Actual/Goal), Year = "FY 2019") %>%
  select(Year, "SPARS Indicator", Goal, Actual, "Percent of Goal Reached")

#FISCAL YEAR 2020
SPARS_GoalsFY20 = data.table(
  "SPARS Indicator" = c("WD2","T3","S1","R1","AC1"),
  "Goal" = c(250,50,275,23,.65),
  ##update "Actual" based upon SPARS calculation
  #APRIL 2020 - current through Q2 reporting
  "Actual" = c(145,52,219,19,.86)
)
SPARS_GoalsFY20 <- SPARS_GoalsFY20 %>%
  mutate("Percent of Goal Reached" = scales::percent(Actual/Goal), Year = "FY 2020") %>%
  select(Year, "SPARS Indicator", Goal, Actual, "Percent of Goal Reached")

#GRANT TO DATE
SPARS_GoalstoDate <- SPARS_GoalsFY19 %>%
  bind_rows(SPARS_GoalsFY20)

#create data table with DIS goals and data
#need to include a table with goal, actual, percent of goal in annual report

##UPDATE THIS SCRIPT TO CHANGE GOALS WHEN/IF ANNAUL G&O CHANGE!!

#creating separate data tables for each year, can append together for grant-to-date summary

#workforce development race/ethnicity table
#pull workforce demographics for actuals data
source('Initial_Training_Data_PrepAnalysis.R')

#join KP participant demographics with all other demographics
#just need participantID, Ethnicity, Gender from RegistDemo and ParticipantID, Ethnicity, Gender from KPparticipants
DISworkforceDemo <- RegistDemo %>%
  filter(Attended=="Yes") %>%
  select(ParticipantId, FirstName, LastName, Ethnicity, EthnicityOE, Gender, GenderOE, FiscalYear) %>%
  left_join(KPparticipants, by = c("ParticipantId" = "ParticipantID", "FiscalYear"="FiscalYear")) %>%
  select(ParticipantId, `FirstName.x`, LastName, FiscalYear, `Ethnicity.x`, EthnicityOE, Gender, GenderOE, `FirstName.y`,
         "Last Name", `Ethnicity.y`)

DISworkforceDemo %>%
#  filter(FiscalYear!=FY20) %>%
  group_by(FiscalYear, Ethnicity.x, Ethnicity.y) %>%
  count()
#reporting grouped categories:
#Hispanic/Latinx = 17
#Black/African American = 10
#White = 124+16 = 140
#Other/Two or More Races = 1+9+1+1+9 = 21
#Ethnicity Unavailable = 3+1 = 4

DISworkforceDemo %>%
#  filter(FiscalYear!=FY20) %>%
  group_by(FiscalYear, Gender) %>%
  count()
#reporting grouped categories:
#Male = 20
#Female = 171
#Other Gender Identity = 1
#Gender Unavailable = 0
#"Ethnicity Unavailable" or "Gender Unavailable" means preferred not to respond or missing data

library(data.table)
DIS_workforceFY19 = data.table(
  "Subpopulation" = c("Total","Hispanic/Latinx","Black/African American","White","Other/Two or More Races", "Ethnicity Unavailable",
                      "Male","Female","Other Gender Identity", "Gender Unavailable"),
  "Goal" = c(50,11,3,32,4,0,15,35,0,0),
  ##update "Actual" based upon training data
  "Actual" = c(192,17,10,140,21,4,20,171,1,0)
)


DIS_workforceFY19 <- DIS_workforceFY19 %>%
  mutate("Percent of Goal Reached" = ifelse(Actual/Goal>=0,(Actual/Goal),NA)) %>%
  mutate("Percent of Goal Reached" = scales::percent(ifelse(`Percent of Goal Reached`=="Inf",NA,`Percent of Goal Reached`)))


#we are missing demographics for a few people because the total number included here is a little smaller than
#the total number for WD2 in SPARS, due mostly to the implementation of our training participant tracking
#system in Q2, after the first training offered



#clinical services race/ethnicity data

#pull Race/Ethnicity data for Enrolled Consumer IDs plus caregivers (other than Self)
source('Initial_Clinical_Data_PrepAnalysis.R')

ClinicalDIStable <- demosEnrolledFamily %>%
  select(`Enrollment Consumer ID`, Enrollment_FYstart, Enrollment_FYend, Ethnicity_Summary, Gender,
         PrimaryLanguage, ServiceLanguage, Relationship, CaregiverName,
         Caregiver_RaceEthnicity, date_start) %>%
  filter(Relationship!="Self")
#***Update to get only those enrolled in current FY!!****

#race
ClinicalDIStable %>%
  filter(Enrollment_FYstart=="FY19") %>%
  group_by(Ethnicity_Summary) %>%
  summarise(Enrolled_Count = n_distinct(`Enrollment Consumer ID`))
ClinicalDIStable %>%
  filter(date_start>"2019-06-30") %>%
  group_by(Caregiver_RaceEthnicity) %>%
  summarise(Caregiver_Count = n())

#reporting grouped categories:
#Hispanic/Latinx = 18+9 = 27
#Black/African American =  4+1 = 5
#White = 5+4 = 9
#Other/Two or More Races = 1+2+1 = 4
#Ethnicity Unavailable = 0

#gender
ClinicalDIStable %>%
  group_by(Gender) %>%
  summarise(Count = n_distinct(`Enrollment Consumer ID`))

levels(ClinicalDIStable$Relationship)
ClinicalDIStable %>%
  group_by(Relationship) %>%
  summarise(Caregiver_Count = n())
#reporting grouped categories
#Female = female enrolled ID + mother/sister/niece+nephew/adoptive parents/foster parents
# 11 + 17 + 1 + 1 + 2 + 1 = 33
#Male = male enrolled ID + father/brother/aunt+uncle/child/grandparents
# 20 + 5 + 1 + 2 + 4 + 4 = 36

#language
ClinicalDIStable %>%
  group_by(PrimaryLanguage) %>%
  count()
#English = 23, Spanish = 16

#create table

DIS_clinicalFY19 = data.table(
  "Subpopulation" = c("Total","Hispanic/Latinx","Black/African American","White","Other/Two or More Races",
                      "Ethnicity Unavailable", "Male","Female", "Gender Unavailable", "English","Spanish"),
  "Goal" = c(30,16,3,6,5,0,17,13,0,20,10),
  ##update "Actual" based upon enrollment data (need to pull from consumer summary AND caregiver form)
  "Actual" = c(35,27,5,9,4,0,36,33,0,23,16)
)

DIS_clinicalFY19 <- DIS_clinicalFY19 %>%
  mutate("Percent of Goal Reached" = scales::percent(ifelse((Actual/Goal)=="Inf","",
                                            ifelse(is.na(Actual/Goal),NA,(Actual/Goal)))))



# #create data table for each year, then to put together for grant to date, append columns from each year, add column groupings in kable formatting
# 
# #kable formatting code
# # library(kableExtra)
# # DIS_clinicalFY19 %>%
# #   kable(caption = "Home-Based Clinical Services Diversity Impact") %>%
# #   kable_styling("striped", full_width = F) %>%
# #   pack_rows("Race/Ethnicity", 2, 6) %>%
# #   pack_rows("Gender",7,9) %>%
# #   pack_rows("Language",10,11) 
# # 
# # DIS_workforceFY19 %>%
# #   kable(caption = "Workforce Development Activities Diversity Impact") %>%
# #   kable_styling("striped", full_width = F) %>%
# #   pack_rows("Race/Ethnicity", 2,6) %>%
# #   pack_rows("Gender",7,10)
