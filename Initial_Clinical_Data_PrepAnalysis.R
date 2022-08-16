#Script to pull clinical assessments data for Right Start for Colorado
#First authored Spring 2019, Nicole Harty
#Last update, July 7 2020, Nicole Harty

library(RODBC)
library(tidyverse)
library(lubridate)
library(readxl)

##Connect to BI01 server and retrieve data
BI01 <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")

#bring in SQL tables
RSCOsp <- sqlQuery(BI01, 'SELECT * FROM Construct.[dbo].[Right Start Grant Dashboard]')
ASQ <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[NH].[ASQ3InformationSummary]')
PSI <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[RightStart].[ParentingStressIndex]')
#3/17/2020 added CBCL, Edinburgh, and SCL90 data pulls
CBCL <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[RightStart].[CBCLScoreReport]')
Edin <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[RightStart].[Edinburgh]')
SCL <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[RightStart].[SCL90]')

##Update this to be a query that excludes the columns I don't need
# CBCL <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[RightStart].[CBCLScoreReport]')

#read in SQL queries and load queries
RaceLangQuery <- read_file("SQL Queries/RaceLanguageQuery.sql")
CaregiversQuery <- read_file("SQL Queries/CaregiversQuery.sql")
ReferralsQuery <- read_file("SQL Queries/ReferralsQuery.sql")
EBPsQuery <- read_file("SQL Queries/ReferralsQuery.sql")

RaceLang <- sqlQuery(BI01, RaceLangQuery)
Caregivers <- sqlQuery(BI01, CaregiversQuery)
Referrals <- sqlQuery(BI01, ReferralsQuery)
EBPs <- sqlQuery(BI01, EBPsQuery)

#6/3/2020: change RSCOsp$EnrollmentConsumerID to factor not integer so joins correctly for twin's screeners (where ID is consumerid_number) 
RSCOsp$`Enrollment Consumer ID` <- as.factor(RSCOsp$`Enrollment Consumer ID`)
#CBCL for twins not yet entered so need to change te AssessedPersonId to factor as well (may pull in as factor once twins entered)
CBCL$AssessedPersonId <- as.factor(CBCL$AssessedPersonId)
#need to change parent assessments to be factor so will join to RSCOsp correctly
Edin$`Consumer ID` <- as.factor(Edin$`Consumer ID`)
SCL$`Consumer ID` <- as.factor(SCL$`Consumer ID`)
#Need to change all other Consumer ID columns to join to be factors
RaceLang$`Master Consumer ID` <- as.factor(RaceLang$`Master Consumer ID`)
Caregivers$`Enrollment Consumer ID` <- as.factor(Caregivers$`Enrollment Consumer ID`)
Referrals$`Enrollment Consumer ID` <- as.factor(Referrals$`Enrollment Consumer ID`)
EBPs$`Enrollment Consumer ID` <- as.factor(EBPs$`Enrollment Consumer ID`)

#clean up the screener datasets - column names for consumer ID vary and aren't appropriately descriptive
#Add Enrolled Consumer ID column for ASQ to account for twins; change Baby_ID
ASQ <- ASQ %>%
  rename("Child Consumer ID" = "Baby_ID") %>%
  mutate("Enrolled Consumer ID" = str_sub(`Child Consumer ID`,1,6)) 


# PSI ---------------------------------------------------------------------


#PSI: add score categories based upon norms (for SCORES not Percentiles)
#PD Score Categories: <38 = Normal, 38-39 = Borderline, 40+ = Clinical
#DC Score Categories: <38 = Normal, 38-39 = Borderline, 40+ = Clinical
#P-CDI Score Categories: <34 = Normal, 34-35 = Borderline, 36+ = Clinical
#Total Stress Score Categories: <110 = Normal, 110-113 = Borderline, 114+ = Clinical
###**1/8/2020: noticed code here was wrong and updated to correctly categorize scores
PSI <- PSI %>%
  mutate("PD Score Category" = ifelse(`PD Score` <38,"Normal",
                ifelse(`PD Score` >37 & `PD Score`<40 ,"Borderline",
                       ifelse(`PD Score`>=40,"Clinical",NA)))
  ) %>%
  mutate("DC Score Category" = ifelse(`DC Score` <38,"Normal",
                                      ifelse(`DC Score` >37 & `DC Score`<40 ,"Borderline",
                                             ifelse(`DC Score`>=40,"Clinical",NA)))
  ) %>%
  mutate("P-CDI Score Category" = ifelse(`P_CDI Score` <34,"Normal",
                                      ifelse(`P_CDI Score` >33 & `P_CDI Score`<36 ,"Borderline",
                                             ifelse(`P_CDI Score`>=36,"Clinical",NA)))
  ) %>%
  mutate("Total Stress Score Category" = ifelse(`Total Stress Score` <110,"Normal",
                                      ifelse(`Total Stress Score` >109 & `Total Stress Score`<114 ,"Borderline",
                                             ifelse(`Total Stress Score`>=114,"Clinical",NA)))
  )
#change new columns to factors
colnames(PSI)
PSI[,24:27] <- lapply(PSI[,24:27], factor)

#3/25/2020 update 
##adding separate PSI dfs for homebased and outpatient to allow for comparisons
##adding additional columns in from raw PSI data: defensive responding, caregiver relationships and names (to match for pre/post exclusion)

#creating second column with "Enrolled Consumer ID" for joining to RSCOsp to pull demographic/clinical info
##*Need to use Child Consumer ID for analysis because that's the Unique ID per twin
#create PSIhb for home-based PSI scores, PSIopt for outpatient Right Start
PSI <- PSI %>%
  filter(!is.na(`Child Consumer ID`)) %>%
  mutate("Enrolled Consumer ID" = str_sub(`Child Consumer ID`,1,6)) 
PSIhb <- PSI %>%
  rename("Respondent Relationship" = "Caretaker Relationship", "Respondent Last Name" = "Care Taker Last Name",
         "Respondent First Name" = "Caretaker First Name") %>%
  select(`Enrolled Consumer ID`, `Child Consumer ID`, `TimePoint Label`, Date, `Child First Name`, `Child  Last Name`, `Defensive Responding?`, "Respondent Last Name", 
         "Respondent First Name", `Respondent Relationship`, "PD Score", `P_CDI Score`, "DC Score", "Total Stress Score", 
         "PD Percent", "P_CDI Percent", "DC Percent", "Total Percent", "PD Score Category", "DC Score Category", 
         "P-CDI Score Category", `Total Stress Score Category`) %>%
  semi_join(RSCOsp, by =  c("Enrolled Consumer ID" = "Enrollment Consumer ID")) 
RSCOspEnrollmentIDs <- RSCOsp %>%
  filter(!is.na(`Enrollment Consumer ID`))
PSIopt <- PSI %>%
  rename("Respondent Relationship" = "Caretaker Relationship", "Respondent Last Name" = "Care Taker Last Name",
         "Respondent First Name" = "Caretaker First Name") %>%
  select(`Enrolled Consumer ID`, `TimePoint Label`, Date, `Child First Name`, `Child  Last Name`, `Defensive Responding?`, "Respondent Last Name", 
         "Respondent First Name", `Respondent Relationship`, "PD Score", `P_CDI Score`, "DC Score", "Total Stress Score", 
         "PD Percent", "P_CDI Percent", "DC Percent", "Total Percent", "PD Score Category", "DC Score Category", 
         "P-CDI Score Category", `Total Stress Score Category`) %>%
  anti_join(RSCOspEnrollmentIDs, by =  c("Enrolled Consumer ID" = "Enrollment Consumer ID")) 



# CBCL ---------------------------------------------------------------------

#Change CBCL column names to be more meaningful
#only need TScores columns (not "Total" or "Percentile")

##***JULY 7 2020 - NEED TO EDIT COLUMN NAMES WITH NEW CBCL TABLE THAT INCLUDES 6-18YO ASSESSMENT**

CBCL <- CBCL %>%
  rename("Enrolled Consumer ID" = "AssessedPersonId", "Timepoint" = "EvaluationId", "Date" = "DateOnForm") %>%
  select("Enrolled Consumer ID", Timepoint, Date, Gender, Age, Relationship, Emotionally_Reactive_TScore, Anxious__Depressed_TScore, Somatic_Complaints_TScore, 
         Withdrawn_TScore, Sleep_Problems_TScore, Attention_Problems_TScore, Aggressive_Behavior_TScore, Internalizing_Problems_TScore, Externalizing_Problems_TScore,
         Total_Problems_TScore, Stress_Problems_TScore, Depressive_Problems_TScore, Anxiety_Problems_TScore, Autism_Spectrum_Problems_TScore, 
         Attention_Deficit__Hyperactivity_Problems_TScore, Oppositional_Defiant_Problems_TScore)
# #make Enrolled Consumer ID int variable
# CBCL$`Enrolled Consumer ID` <- as.integer(CBCL$`Enrolled Consumer ID`)

##Create TScore Category variables
# <65 = Normal, 65-69 = Borderline, 70+ = Clinical: Emotional Reaction, Anxious/Depressed, Somatic Complaints, Withdrawn, 
#     Internalizing Problems, Attention Problems, Aggressive Behaviors, Anxiety Problems, ADHD Problems, Autism Spectrum Problems,
#      Oppositional Defiance, Sleep Problems
#Emotional Reaction + Anxious/Depressed + Somatic Complaints + Withdrawn = Internalizing Problems
#Attention Problems + Aggressive Behaviors = Externalizing Problems
# <60 = Normal, 60-63 = Borderline, 64+ = Clinical: Internalizing Problems, Externalizing Problems, Total Problems
#Total Problems = all scores together: <60 = Normal, 60-63 = Borderline, 64+ = Clinical
CBCL <- CBCL %>%
  mutate("Emotional Reaction" = ifelse(Emotionally_Reactive_TScore <65,"Normal",
                                      ifelse(Emotionally_Reactive_TScore<70 ,"Borderline",
                                             ifelse(Emotionally_Reactive_TScore>=70,"Clinical",NA))),
         "Anxious/Depressed" = ifelse(Anxious__Depressed_TScore <65,"Normal",
                                      ifelse(Anxious__Depressed_TScore<70 ,"Borderline",
                                             ifelse(Anxious__Depressed_TScore>=70,"Clinical",NA))),
         "Somatic Complaints" = ifelse(Somatic_Complaints_TScore <65,"Normal",
                                       ifelse(Somatic_Complaints_TScore<70 ,"Borderline",
                                              ifelse(Somatic_Complaints_TScore>=70,"Clinical",NA))),
         "Withdrawn" = ifelse(Withdrawn_TScore <65,"Normal",
                              ifelse(Withdrawn_TScore<70 ,"Borderline",
                                     ifelse(Withdrawn_TScore>=70,"Clinical",NA))),
         "Attention Problems" = ifelse(Attention_Problems_TScore <65,"Normal",
                                       ifelse(Attention_Problems_TScore<70 ,"Borderline",
                                              ifelse(Attention_Problems_TScore>=70,"Clinical",NA))),
         "ADHD Problems" = ifelse(Attention_Deficit__Hyperactivity_Problems_TScore <65,"Normal",
                                       ifelse(Attention_Deficit__Hyperactivity_Problems_TScore<70 ,"Borderline",
                                              ifelse(Attention_Deficit__Hyperactivity_Problems_TScore>=70,"Clinical",NA))),
         "Aggressive Behaviors" = ifelse(Aggressive_Behavior_TScore <65,"Normal",
                                         ifelse(Aggressive_Behavior_TScore<70 ,"Borderline",
                                                ifelse(Aggressive_Behavior_TScore>=70,"Clinical",NA))),
         "Anxiety Problems" = ifelse(Anxiety_Problems_TScore <65,"Normal",
                                     ifelse(Anxiety_Problems_TScore<70 ,"Borderline",
                                            ifelse(Anxiety_Problems_TScore>=70,"Clinical",NA))),
         "Autism Spectrum Problems" = ifelse(Autism_Spectrum_Problems_TScore <65,"Normal",
                                     ifelse(Autism_Spectrum_Problems_TScore<70 ,"Borderline",
                                            ifelse(Autism_Spectrum_Problems_TScore>=70,"Clinical",NA))),
         "Oppositional Defiance" = ifelse(Oppositional_Defiant_Problems_TScore <65,"Normal",
                                             ifelse(Oppositional_Defiant_Problems_TScore<70 ,"Borderline",
                                                    ifelse(Oppositional_Defiant_Problems_TScore>=70,"Clinical",NA))),
         "Sleep Problems" = ifelse(Sleep_Problems_TScore <65,"Normal",
                                          ifelse(Sleep_Problems_TScore<70 ,"Borderline",
                                                 ifelse(Sleep_Problems_TScore>=70,"Clinical",NA))),
         "Stress Problems" = ifelse(Stress_Problems_TScore <65,"Normal",
                                          ifelse(Stress_Problems_TScore<70 ,"Borderline",
                                                 ifelse(Stress_Problems_TScore>=70,"Clinical",NA))),
         "Depressive Problems" = ifelse(Depressive_Problems_TScore <65,"Normal",
                                          ifelse(Depressive_Problems_TScore<70 ,"Borderline",
                                                 ifelse(Depressive_Problems_TScore>=70,"Clinical",NA))),
         "Internalizing Problems" = ifelse(Internalizing_Problems_TScore <60,"Normal",
                                            ifelse(Internalizing_Problems_TScore<64 ,"Borderline",
                                                   ifelse(Internalizing_Problems_TScore>=64,"Clinical",NA))),
         "Externalizing Problems" = ifelse(Externalizing_Problems_TScore <60,"Normal",
                                            ifelse(Externalizing_Problems_TScore<64 ,"Borderline",
                                                   ifelse(Externalizing_Problems_TScore>=64,"Clinical",NA))),
         "Total Problems" = ifelse(Total_Problems_TScore <60,"Normal",
                                            ifelse(Total_Problems_TScore<64 ,"Borderline",
                                                   ifelse(Total_Problems_TScore>=64,"Clinical",NA)))
         )
#change new columns to factors

##***JULY 7 2020 - NEED TO EDIT INDEX REFERENCES WITH NEW CBCL TABLE THAT INCLUDES 6-18YO ASSESSMENT**
colnames(CBCL)
CBCL[,24:38] <- lapply(CBCL[,24:38], factor)


# Edinburgh ---------------------------------------------------------------------

#Edinburgh data clean up
##change column names, calculate categorical fields, assign factors
Edin <- Edin %>%
  rename("Enrolled Consumer ID" = "Consumer ID") %>%
  mutate(ScoreCat = ifelse(`Total Score`>=10,"Possible Depression","Normal"))
Edin$`TimePoint Label` <- as.factor(Edin$`TimePoint Label`)
Edin$ScoreCat <- as.factor(Edin$ScoreCat)


# SCL90 ---------------------------------------------------------------------

#SCL90 data clean up
SCL <- SCL %>%
  rename("Enrolled Consumer ID" = "Consumer ID")
SCL$`TimePoint Label` <- as.factor(SCL$`TimePoint Label`)


# Demographics Prep ---------------------------------------------------------------------

##I have a lot of the clinical data already working in Tableau - all the reporting data works
##If writing the full report in R, I need to do all that in R --> create the visualizations, etc.
##But, the SPARS data isn't necessary to visulaize
##Sreeners data and services data is more valuable to visualize; that hasn't been done in Tableau

##Clinical Data to include: SPARS Metrics, Caregiver and Referrals Information, Screener Results & Analysis
###SPARS Metrics = individuals screened, referred, receiving services, percent accessing services who were referred: this is included in a manual table in report
###Screener Results & Analysis = Baseline ASQ results, 6-mo change in ASQ, Baseline PSI results, 6-mo change in PSI, CBCL results,
###6-mo change in CBCL, Baseline SCL, 6-mo change in SCL

#SPARS Metrics - pulled from quarterly report summary file (excel) and manually create table in report

#Add FY variable to enrollments
#Add FY column variable based upon training date (add to Evals and RegistDemo)
FY19 <- interval("2018-10-01", "2019-09-30")
FY20 <- interval("2019-10-01", "2020-09-30")
FY21 <- interval("2020-10-01", "2021-09-30")
FY22 <- interval("2021-10-01", "2022-09-30")
FY23 <- interval("2022-10-01", "2023-09-30")

RSCOsp$Enrollment_FYstart <- with(
  RSCOsp, ifelse(EnrollmentStart %within% FY19, 'FY19',
                      ifelse(EnrollmentStart %within% FY20, 'FY20',
                             ifelse(EnrollmentStart %within% FY21, 'FY21',
                                    ifelse(EnrollmentStart %within% FY22, 'FY22',
                                           ifelse(EnrollmentStart %within% FY23, 'FY23',"N/A")))))
)
RSCOsp$Enrollment_FYstart <- as.factor(RSCOsp$Enrollment_FYstart)

RSCOsp$Enrollment_FYend <- with(
  RSCOsp, ifelse(EnrollmentEnd %within% FY19, 'FY19',
                    ifelse(EnrollmentEnd %within% FY20, 'FY20',
                           ifelse(EnrollmentEnd %within% FY21, 'FY21',
                                  ifelse(EnrollmentEnd %within% FY22, 'FY22',
                                         ifelse(EnrollmentEnd %within% FY23, 'FY23',"N/A")))))
)
RSCOsp$Enrollment_FYend <- as.factor(RSCOsp$Enrollment_FYend)

#calculate enrollment duration and imput a duration for those still enrolled (using today's date)
#first, input today's date for EnrollmentEnd for those who have an enrollment start
RSCOsp$EnrollmentEnd <- as_date(ifelse(is.na(RSCOsp$EnrollmentStart),NA,
                                  ifelse(is.na(RSCOsp$EnrollmentEnd),Sys.Date(),RSCOsp$EnrollmentEnd)))
#then create new column
RSCOsp <- RSCOsp %>%
  mutate(EnrollmentDuration = EnrollmentEnd - EnrollmentStart)

#Demographics of ALL those served: combine those receiving BHAs with those enrolled in home-based services with caregivers
demographicsAll <- RaceLang %>%
  left_join(Caregivers, by = c("Master Consumer ID" = "Enrollment Consumer ID")) %>%
#remove duplicated columns from join
  select(-c(`Enrollment Active?.y`, `EnrollmentStart.y`, `Intake?.y`)) %>%
  rename("EnrollmentActive" = `Enrollment Active?.x`, EnrollmentStart = `EnrollmentStart.x`,
         "Intake" = `Intake?.x`)
#calculate enrollment duration for demographics df as doen with allEnroll data fram
#first, input today's date for EnrollmentEnd for those who have an enrollment start
demographicsAll$EnrollmentEnd <- as_date(ifelse(is.na(demographicsAll$EnrollmentStart),NA,
                                          ifelse(is.na(demographicsAll$EnrollmentEnd),Sys.Date(),demographicsAll$EnrollmentEnd)))
#then create new column
demographicsAll <- demographicsAll %>%
  mutate(EnrollmentDuration = EnrollmentEnd - EnrollmentStart)

#Demographics of just those enrolled in Home-based Services
demosEnrolledIDs <- RSCOsp %>%
  filter(!is.na(`Enrollment Consumer ID`)) %>%
  left_join(RaceLang, by = c("Enrollment Consumer ID" = "Master Consumer ID")) %>%
  select(`Enrollment Consumer ID`, `Enrollment Consumer`, `EnrollmentStart.x`, EnrollmentEnd, `Most Recent BHA Date.x`,
         `BHA Staff`, `Enrollment Staff`, `Enrollment Active?.x`, `Earliest Intake Date`, `Earliest Therapy Date`, 
         `Receive Services?.x`, Enrollment_FYstart, Enrollment_FYend, EnrollmentDuration, `Date of Birth`, 
         `Race and Ethnicity Summary`, Gender, `Primary Language`, `Service Language`, `Last CCAR Discharge Date`, `CCAR Reason For Discharge`) %>%
#rename columns with ".x"
  rename("Most Recent BHA" = `Most Recent BHA Date.x`, "EnrollmentActive" = `Enrollment Active?.x`, 
         EnrollmentStart = `EnrollmentStart.x`, "Receive Services" = `Receive Services?.x`, "Ethnicity_Summary" = `Race and Ethnicity Summary`,
         "PrimaryLanguage" = `Primary Language`, "ServiceLanguage" = `Service Language`, "DOB" = `Date of Birth`)
demosEnrolledIDs <- demosEnrolledIDs %>%
  mutate(Enrolled_AgeCat= ifelse((difftime(Sys.Date(), demosEnrolledIDs$DOB,
                                    units = "weeks")>=938.571),"Adult","Child"))


#Demographics of those enrolled in Home-based Services AND their caregivers
demosEnrolledFamily <- demosEnrolledIDs %>%
  left_join(Caregivers, by = "Enrollment Consumer ID") %>%
  select(`Enrollment Consumer ID`, `Enrollment Consumer.x`, Enrolled_AgeCat, `EnrollmentStart.x`, `EnrollmentEnd.x`, `Most Recent BHA`,
         `BHA Staff`, `Enrollment Staff.x`, EnrollmentActive, `Earliest Intake Date.x`, `Earliest Therapy Date`,
         `Receive Services`, Enrollment_FYstart, Enrollment_FYend, EnrollmentDuration, DOB, Ethnicity_Summary, Gender, PrimaryLanguage, 
         ServiceLanguage, `Relationship to ConsumerID`, `Caregiver Name`,
         date_start, Caregiver_RaceEthnicity) %>%
#rename columns with ".x"
  rename("Enrollment Consumer" = `Enrollment Consumer.x`, "EnrollmentStart" = `EnrollmentStart.x`, 
        "EnrollmentEnd" = `EnrollmentEnd.x`, "Enrollment Staff" = `Enrollment Staff.x`, 
        "Earliest Intake Date" = `Earliest Intake Date.x`, "Relationship" = `Relationship to ConsumerID`, "CaregiverName" = `Caregiver Name`)


# Add Demos to Screeners ---------------------------------------------------------------------

#DO I WANT JUST DEMOS FOR THE ENROLLED CONSUMER OR THE KIDDO? for now (11/18/19), just enrolled consumer ID
ASQdemos <- ASQ %>%
  left_join(demosEnrolledIDs, by = c("Enrolled Consumer ID" = "Enrollment Consumer ID")) %>%
  select(Date_Completed, `Enrolled Consumer ID`, Tx_Time, Communication_Score, Gross_Motor_Score, Fine_Motor_Score, 
         Problem_Solving_Score, PersonalSocial_Score, Communication_Category, Gross_Motor_Category, Fine_Motor_Category, 
         Problem_Solving_Category, PersonalSocial_Category, Refer_for_Hear_Vision_Behav, Refer_for_Primary_Care_Comm, 
         Refer_for_EI_Spec_Ed, `Enrollment Consumer`, `Most Recent BHA`, EnrollmentActive, EnrollmentStart, EnrollmentEnd, 
         Enrollment_FYstart, Enrollment_FYend, `Earliest Intake Date`, `Receive Services`, EnrollmentDuration, 
         DOB, Ethnicity_Summary, PrimaryLanguage, ServiceLanguage)
#add FY variable for date ASQ completed 
ASQdemos$FiscalYear <- with(
    ASQdemos, ifelse(Date_Completed %within% FY19, 'FY19',
                   ifelse(Date_Completed %within% FY20, 'FY20',
                          ifelse(Date_Completed %within% FY21, 'FY21',
                                 ifelse(Date_Completed %within% FY22, 'FY22',
                                        ifelse(Date_Completed %within% FY23, 'FY23',"N/A")))))
  )
ASQdemos$FiscalYear <- as.factor(ASQdemos$FiscalYear)

PSIdemos <- PSIhb %>%
  left_join(demosEnrolledIDs, by = c("Enrolled Consumer ID" = "Enrollment Consumer ID")) %>%
  select(`Enrolled Consumer ID`, `TimePoint Label`, Date, `Child First Name`, `Child  Last Name`, "PD Score", `P_CDI Score`, "DC Score",
         "Total Stress Score", "PD Percent", "P_CDI Percent", "DC Percent", "Total Percent", "PD Score Category", "DC Score Category",
         "P-CDI Score Category", "Total Stress Score Category", `Enrollment Consumer`, `Most Recent BHA`, EnrollmentActive, 
         EnrollmentStart, EnrollmentEnd, Enrollment_FYstart, Enrollment_FYend, `Earliest Intake Date`, `Receive Services`, 
         EnrollmentDuration, DOB, Ethnicity_Summary, PrimaryLanguage, ServiceLanguage)
#add FY variable to PSI
PSIdemos$FiscalYear <- with(
  PSIdemos, ifelse(Date %within% FY19, 'FY19',
                   ifelse(Date %within% FY20, 'FY20',
                          ifelse(Date %within% FY21, 'FY21',
                                 ifelse(Date %within% FY22, 'FY22',
                                        ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
PSIdemos$FiscalYear <- as.factor(PSIdemos$FiscalYear)


##***JULY 7 2020 - NEED TO EDIT COLUMN NAMES WITH NEW CBCL TABLE THAT INCLUDES 6-18YO ASSESSMENT**

colnames(CBCL)
CBCLdemos <- CBCL %>%
  left_join(demosEnrolledIDs, by = c("Enrolled Consumer ID" = "Enrollment Consumer ID")) %>%
  select("Enrolled Consumer ID", Timepoint, Date, `Gender.x`, Age, Relationship, Emotionally_Reactive_TScore, Anxious__Depressed_TScore, Somatic_Complaints_TScore, 
         Withdrawn_TScore, Sleep_Problems_TScore, Attention_Problems_TScore, Aggressive_Behavior_TScore, Internalizing_Problems_TScore, Externalizing_Problems_TScore,
         Total_Problems_TScore, Stress_Problems_TScore, Depressive_Problems_TScore, Anxiety_Problems_TScore, Autism_Spectrum_Problems_TScore, 
         Attention_Deficit__Hyperactivity_Problems_TScore, Oppositional_Defiant_Problems_TScore, "Emotional Reaction", "Somatic Complaints", "Withdrawn", 
         "Attention Problems", "ADHD Problems", "Aggressive Behaviors", "Anxiety Problems", "Autism Spectrum Problems", "Oppositional Defiance", "Sleep Problems", 
         "Stress Problems", "Depressive Problems", "Internalizing Problems", "Externalizing Problems", "Total Problems", `Enrollment Consumer`, `Most Recent BHA`, 
         EnrollmentActive, EnrollmentStart, EnrollmentEnd, Enrollment_FYstart, Enrollment_FYend, `Earliest Intake Date`, `Receive Services`, EnrollmentDuration, 
         DOB, Ethnicity_Summary, PrimaryLanguage, ServiceLanguage)
#add FY variable to CBCL data
CBCLdemos$Date <- as.Date(CBCLdemos$Date, "%m/%d/%Y")
CBCLdemos$FiscalYear <- with(
  CBCLdemos, ifelse(Date %within% FY19, 'FY19',
                   ifelse(Date %within% FY20, 'FY20',
                          ifelse(Date %within% FY21, 'FY21',
                                 ifelse(Date %within% FY22, 'FY22',
                                        ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
CBCLdemos$FiscalYear <- as.factor(CBCLdemos$FiscalYear)

colnames(Edin)
EdinDemos <- Edin %>%
  left_join(demosEnrolledIDs, by = c("Enrolled Consumer ID" = "Enrollment Consumer ID")) %>%
  select("Enrolled Consumer ID", Date, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, "Total Score", "ScoreCat", "TimePoint Label", 
         `Enrollment Consumer`, `Most Recent BHA`, EnrollmentActive, EnrollmentStart, EnrollmentEnd, Enrollment_FYstart, 
         Enrollment_FYend, `Earliest Intake Date`, `Receive Services`, EnrollmentDuration, `DOB.y`, Ethnicity_Summary, 
         PrimaryLanguage, ServiceLanguage)
#add FY variable to CBCL data
EdinDemos$Date <- as.Date(EdinDemos$Date, "%m/%d/%Y")
EdinDemos$FiscalYear <- with(
  EdinDemos, ifelse(Date %within% FY19, 'FY19',
                    ifelse(Date %within% FY20, 'FY20',
                           ifelse(Date %within% FY21, 'FY21',
                                  ifelse(Date %within% FY22, 'FY22',
                                         ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
EdinDemos$FiscalYear <- as.factor(EdinDemos$FiscalYear)

