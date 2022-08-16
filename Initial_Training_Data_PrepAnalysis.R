#Script to pull all Right Start for Colorado training data together for reporting and analysis
#First authored Spring 2019, Nicole Harty
#Last update, Sept 2 2020, Nicole Harty

library(RODBC)
library(tidyverse)
library(lubridate)

##Connect to BI01 server and retrieve data
source('../BI01_RODBC.R')


##because of missing ParticipantID and mismatched training/event name - bring in each table from SQL and do all prep in R
RegistDemo <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TrainingRegistrationsFinal')
Evals <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TrainingEvaluation')
TrainSummary <- sqlQuery(BI01, 'SELECT * FROM [DataEntry].[RightStart].[TrainingSummary]')
DC05prepost <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.DC05PrePostTest')
TenetsEval <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TenetsEval')

##****KP Loose Change training is NOT in SQL:
library(readxl)
KPeval <- read_excel("YR1 Indiv Training Files/KP Loose Change TrainingEvalResults.xlsx")
#need to add KP registrations to TrainAttend - use demographics from Constant Contact and have NA for those who only attended KP
KPparticipants <- read_excel("YR1 Indiv Training Files/KP Loose Change TrainingParticipantDemographics.xlsx")
KPparticipants$FiscalYear <- "FY19"

#bring electronic evals in from Qualtrics
#data prep completed in separate script so load that script.
source('QualtricsTrainingEvals.R')


# Registration Data Manipulation and Prep ---------------------------------

#get rid of registrations from testing (registrations that were cancelled or for Theresa or me)
RegistDemo <- RegistDemo %>%
  filter(RegistrationStatus!='CANCELLED', Email!='theresa.diggs@mhcd.org', Email!='nicole.harty@mhcd.org',
         Email!='nicole.m.harty@gmail.com')

##removing columns I don't need
#April 2020: updating based upon new ETL table. Some new columns added that aren't needed here; keeping RegistrationStatus so can keep
#track of the cancelled trainings due to COVID and get accurate participation rates
colnames(RegistDemo)
RegistDemo <- RegistDemo %>%
  select(-c("ImportedDateTime", "EventStatus", "EventLocation", "EventAddress", "EventDescription", "EventCreatedDateTime", 
            "TotalRegistrationCount", "EventActivatedDateTime", "RegistrationId", "GuestCount", "PaymentStatus",
            "RegistrationDateTime", "RegistrationUpdatedDateTime", "First Name:", "Last Name:",
            "Secondary Language Set- $100\r\n(Cannot be the same language as the Primary Language Set)",
            "Primary Language Set- $0.00\r\n(Included with purchase)",
            "You are required to bring a DC: 0-5 manual in order to participate in this training. We will alert you at least a month befor...",
            "City:", "State:", "You are required to bring a DC: 0-5 manual to participate in this training.",
            "You are required to bring a DC: 0-5 manual in order to participate in this training.",
            "Please let us know how you heard about DC:0-5 Training.",
            "Please check if you are in need of any of the following dietary accommodations. (Check all that apply)",
            "Email Address:","Address 1:", "ZIP Code:"))

#renaming RegistDemo columns
colnames(RegistDemo)
RegistDemo <- RegistDemo %>% 
  rename('Reason for Attending' = `What is the main reason you are participating in this training? (Please check one)`,
         'Alternate Email' = `Please provide us with an alternate email address:`,
         'UniqueID' = `Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...`,
         Gender1 = `Gender (Please check one):`, Gender2 = `Gender (Please check one)`, 
         'Clinical?' = `Do you provide ongoing mental health psychotherapy services to children 0-5 and their families?`, 
         GenderOE = `If you chose \"Prefer to Self-Describe\" please feel free to do so here.`, Ethnicity = `Ethnicity (Please check one)`, 
         EthnicityOE2 = `If you chose \"Prefer to Self-Describe\", please feel free to do so here.`,
         EthnicityOE1 = `If you chose \"Prefer to Self-Describe\", feel free to do so here.`, 
         Organization1 = `What is the name of your organization/primary employer?`, 
         Organization2 = `Organization:`, Language = `What primary language/s do you use professionally? (Select all that apply)`, 
         County1 = `What county/counties do you predominantly serve? (Check all that apply)`, 
         Education = `What is the highest level of education you have completed? (Please check one)`,
         Experience = `How many years of experience do you have working with infants, toddlers, and children ages 0-5 years?`, 
         County2 = `What county/counties do you predominantly serve?`, 
         Role1 = `What category best defines your role in the workplace?`, Role2 = `Role in the Workplace:`,
         Age = `Age (Please check one)`, StartDate = EventStartDateTime, EndDate = EventEndDateTime)
#concatenate the two County columns; can use just unite with na.rm=true when tidyr gets updated (but check if still need to change to character rather than factor)
#RegistDemo %>% unite(County, c("County1", "County2"), na.rm = TRUE)
RegistDemo$County1 <- as.character(RegistDemo$County1)
RegistDemo$County2 <- as.character(RegistDemo$County2)
RegistDemo$Gender1 <- as.character(RegistDemo$Gender1)
RegistDemo$Gender2 <- as.character(RegistDemo$Gender2)
RegistDemo$EthnicityOE1 <- as.character(RegistDemo$EthnicityOE1)
RegistDemo$EthnicityOE2 <- as.character(RegistDemo$EthnicityOE2)
RegistDemo$Organization1 <- as.character(RegistDemo$Organization1)
RegistDemo$Organization2 <- as.character(RegistDemo$Organization2)
RegistDemo$Role1 <- as.character(RegistDemo$Role1)
RegistDemo$Role2 <- as.character(RegistDemo$Role2)

RegistDemo <- RegistDemo %>%  
  replace_na(list(County1 = "", County2 = "", EthnicityOE1 = "", EthnicityOE2 = "", Gender1 = "", Gender2 = "", Organization1 = "", Organization2 = "",
                  Role1 = "", Role2 = "")) %>% 
  unite(County, County1, County2, remove = TRUE, sep = "") %>%
#April 2020: Adding in uniting of Ethnicity OE and Gender columns because there were additional phrasing used (see renaming above)
  unite(Gender, Gender1, Gender2, remove = TRUE, sep = "") %>%
  unite(EthnicityOE, EthnicityOE1, EthnicityOE2, remove = TRUE, sep = "") %>%
#May 2020: Adding in uniting of Organization and Role because there were additional phrasing used (see renaming above)
  unite(Organization, Organization1, Organization2, remove = TRUE, sep = "") %>%
#Aug 2020: Role 2 is an open-ended question - registrants were allowed to write whatever they wanted. Don't concatenate
  # unite(Role, Role1, Role2, remove = TRUE, sep = "")
  mutate(Role=Role1) %>%
  select(-Role2, -Role1)
RegistDemo$County <- as.factor(RegistDemo$County) 
RegistDemo$Gender <- as.factor(RegistDemo$Gender) 
RegistDemo$EthnicityOE <- as.factor(RegistDemo$EthnicityOE) 
RegistDemo$Organization <- as.factor(RegistDemo$Organization) 
RegistDemo$Role <- as.factor(RegistDemo$Role) 

#set factor levels
levels(RegistDemo$Experience)
RegistDemo$Experience <- RegistDemo$Experience %>%
  fct_relevel("11+", after = 4)

# APRIL 2020: This Attendance file was only for year 1 annual report because ETL was broken. Checking it can be deleted.
# #9.6.19 SQL TrainingRegistrationsFinal doesn't include attendance data correctly, so bringing in Attendance excel file
# TrainAttend <- read_excel("Attendance_TrainingParticipants10.17.19.xlsx")
# TrainAttend$ParticipantId <- as.numeric(TrainAttend$ParticipantId)

#SQL TrainingRegistrationsFinal doesn't have all allied/clinical data (some was collected after the fact), so bringing that in
ClinAllied <- read_excel("ClinicalAlliedCategorization8.19.19.xlsx")

#Join TrainSummary to RegistDemo to get the "Final" training names
#also join to TrainAttend to get attendance (becasue the Attendance ETL not working 9/6/19)
#use fuzzy join
library(fuzzyjoin)

#APRIL 2020: Fuzzy Join doesn't always work - there's a spacing difference in the preregistrants vs regular event for Jan 27-28 DC05
##Need to hard code this update before joining to TrainSummary
RegistDemo$EventName <- as.character(RegistDemo$EventName)
RegistDemo[RegistDemo$EventName == "RS-CO Jan 27,28 DC:0-5 late registrant","EventName"] <- "RS-CO Jan 27, 28 DC:0-5 Training"
RegistDemo$EventName <- as.factor(RegistDemo$EventName)


RegistDemo <- RegistDemo %>%
  regex_full_join(TrainSummary, by = c(EventName = "TrainingName")) %>%
  # colnames(RegistDemo) RegistDemo <- RegistDemo %>%
  select(-c("LearningObjective1", "LearningObjective2", "LearningObjective3", "LearningObjective4")) %>%
  #  full_join(TrainAttend, by = c("ParticipantId", "EventName")) %>%
  #Clean up data frame ##colnames(RegistDemo)
  ##RegistDemo <- RegistDemo %>%
  select(-c("EventId", "EventName", "StartDate.x", "EndDate.x")) %>%
  ##RegistDemo <- RegistDemo %>%
  left_join(ClinAllied, by = c("ParticipantId" = "ParticipantID"))
#Jan 27-28 2020 DC:05 registrations don't join up correctly - need to fix.
##DON'T PUT EACH REGISTRATION IN TRAINING SUMMARY!!!!!


#concatenate Clinical?.x and Clinical?.y
##DO NOT CONCATENATE - DO THIS PROGRAMMATICALLY BECAUSE EACH COLUMN IS DIFFERENT (y/n vs clinical/allied)
#Clinical?.x is factor and Clinical?.y is character. Make both character then change to factor at end
RegistDemo$`Clinical?.x` <- as.character(RegistDemo$`Clinical?.x`)

##****CHECK TO SEE IF NAMING COLUMNS WITH "" RATHER THAN `` WORKS!!!! - THIS WON'T EVALUATE IN MARKDOWN!**##
RegistDemo$ClinicalProfessional <- with(
  RegistDemo, if_else(is.na(`Clinical?.x`),`Clinical?.y`,`Clinical?.x`))
RegistDemo$ClinicalProfessional <- with(
  RegistDemo, if_else(ClinicalProfessional=='Clinical','Yes',
                      if_else(ClinicalProfessional=='Allied','No',ClinicalProfessional)))
#remove old Clinical columns and rename columns appended with x and y
RegistDemo <- RegistDemo %>%
  select(-c("Clinical?.x", "Clinical?.y")) %>%
  rename(StartDate = `StartDate.y`, EndDate = `EndDate.y`)

# Master Evaluation df Manipulation ---------------------------

#need to add column with training name and end date for joining to Evals table and Training Summary
##use a date range filter/ifelse on the recorded date of the survey to determine which offering for evals used for multiple trainings (i.e. Foundations)
##hard code for one time evals
QevalsMaster$EvalName <- as.factor(QevalsMaster$EvalName)
colnames(QevalsMaster)
colnames(Evals)
colnames(TrainSummary)
levels(TrainSummary$TrainingName)
levels(QevalsMaster$EvalName)

#******ADD TO THIS WHEN NEW TRAININGS ADDED!!********

QevalsMaster <- QevalsMaster %>%
  mutate(TrainingName = ifelse(EvalName=="RS-CO April 2020 Self-Care Webinar"&RecordedDate<"2020-04-28","RS-CO April 22 Inhale to Exhale Self-care webinar",
                               ifelse(EvalName=="RS-CO April 2020 Self-Care Webinar","RS-CO April 28 Inhale to Exhale Self-care webinar",
                                      ifelse(EvalName=="RS-CO April DDHS COVID-19 Response Training Evaluation","RS-CO April 10 DHS Training Series",
                                             ifelse(EvalName=="RS-CO April DDHS Evaluation","RS-CO April 16 DHS",
                                                    ifelse(EvalName=="RS-CO Foundations Post Evaluation"&RecordedDate<"2020-05-25","RS-CO Feb. 26, 2020 Foundations",
                                                           ifelse(EvalName=="RS-CO March DDHS Evaluation","RS-CO Mar. 26 DHS Training",
                                                                  ifelse(EvalName=="RS-CO July DDHS Evaluation","RS-CO 7/23/2020 DHS Foster Support Workers",
                                                                         ifelse(EvalName=="RS-CO July IECMH Overview Evaluation","RS-CO 7/21/2020 Montrose Training",NA))))))))) %>%
  rename("OverallValue"=OverallRating)

#REMOVE BAD FOUNDATIONS DATA FROM FEB 26 2020 TRAINING
## Foundations post eval from May 2020 (first eval in Qualtrics) had Relationships and Learning Objectives Likert scales backwards (ie SA -> SD rather than SD -> SA)
##very likely leading to inaccurate responses. Exclude this data
QevalsMaster <- QevalsMaster %>%
  mutate(Relationships1=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,Relationships1),
         Relationships2=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,Relationships2),
         Relationships3=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,Relationships3),
         Relationships4=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,Relationships4),
         LearningObj1=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,LearningObj1),
         LearningObj2=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,LearningObj2),
         LearningObj3=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,LearningObj3),
         LearningObj4=ifelse(TrainingName=="RS-CO Feb. 26, 2020 Foundations",NA,LearningObj4)) 
QevalsMaster[,17:24] <- lapply(QevalsMaster[,17:24], as.factor)


#APPEND TENETS EVALS TO REGULAR EVALS
#TenetsEval columns to bring: LO1-LO4, Relationships1-4
colnames(TenetsEval)
TenetsAppend <- TenetsEval %>%
  select(c(1:9, 13:16)) %>%
  filter(ParticipantID!="NA")
Evals <- Evals %>%
  bind_rows(TenetsAppend)

#change all likert reponses to factors
Evals[,6:31] <- lapply(Evals[,6:31], as.factor)
Evals[,6:31] <- Evals[,6:31] %>%
  fct_unify() 
Evals[,6:31] <- lapply(Evals[,6:31], fct_relevel, "1")


#Merge Foundations exports (this is static, as of May 2020) with Evals df
load("CoAIMHpostEval.Rdata") 
colnames(CoAIMHpostEval)
colnames(Evals)

Evals <- Evals %>%
  bind_rows(CoAIMHpostEval %>%
              rename(OverallValue = OverallRating))

#**MERGE QUALTRICS SURVEYS WITH Evals df 
colnames(QevalsMaster)
colnames(Evals)
#need to add in TrainingTopic, StartDate, EndDate for QevalsMaster trainings
QevalsMaster <- QevalsMaster %>%
  left_join(TrainSummary, by = "TrainingName") %>%
  select(-c(Location, Participants, LearningObjective1, LearningObjective2, LearningObjective3, LearningObjective4)) 

Evals <- Evals %>%
  bind_rows(QevalsMaster %>%
              rename(ParticipantID = ExternalReference) %>%
              select(-c(RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail)))


# Adding Demographics Info ------------------------------------------------


##I just want the demographics for all evals for which I have participant ID, then can stratify eval results, but will do demographic descriptions from RegistDemo
#need new ID column for evals for which I don't have a participant
#need to chage ID to numeric
Evals$ID <- as.numeric(Evals$ID)
Evals <- Evals %>%
  mutate(EvalPartID = coalesce(ParticipantID, ID))
EvalsWdemos <- Evals %>%
  left_join(RegistDemo, by = c("EvalPartID" = "ParticipantId", "TrainingName" = "TrainingName",
                               "StartDate" = "StartDate", "EndDate" = "EndDate")) %>%
  select(-c("TrainingTopic.y", "Location", "Participants")) %>%
  regex_left_join(TrainSummary, by = c("TrainingName" = "TrainingName")) %>%
  select(-c("TrainingTopic.x", "TrainingName.y", "StartDate.y", "EndDate.y", "Participants", "Location", "LearningObjective1", "LearningObjective2", "LearningObjective3", 
            "LearningObjective4")) %>%
  rename(TrainingName = `TrainingName.x`, StartDate = `StartDate.x`, EndDate = `EndDate.x`)


#Add FY column variable based upon training date (add to Evals, RegistDemo, and TrainSummary)
FY19 <- interval("2018-10-01", "2019-09-30")
FY20 <- interval("2019-10-01", "2020-09-30")
FY21 <- interval("2020-10-01", "2021-09-30")
FY22 <- interval("2021-10-01", "2022-09-30")
FY23 <- interval("2022-10-01", "2023-09-30")

EvalsWdemos$FiscalYear <- with(
  EvalsWdemos, ifelse(EndDate %within% FY19, 'FY19',
             ifelse(EndDate %within% FY20, 'FY20',
                    ifelse(EndDate %within% FY21, 'FY21',
                           ifelse(EndDate %within% FY22, 'FY22',
                                  ifelse(EndDate %within% FY23, 'FY23',"N/A")))))
)
EvalsWdemos$FiscalYear <- as.factor(EvalsWdemos$FiscalYear)


RegistDemo$FiscalYear <- with(
  RegistDemo, ifelse(EndDate %within% FY19, 'FY19',
                      ifelse(EndDate %within% FY20, 'FY20',
                             ifelse(EndDate %within% FY21, 'FY21',
                                    ifelse(EndDate %within% FY22, 'FY22',
                                           ifelse(EndDate %within% FY23, 'FY23',"N/A")))))
)
RegistDemo$FiscalYear <- as.factor(RegistDemo$FiscalYear)

TrainSummary$FiscalYear <- with(
  TrainSummary, ifelse(EndDate %within% FY19, 'FY19',
                     ifelse(EndDate %within% FY20, 'FY20',
                            ifelse(EndDate %within% FY21, 'FY21',
                                   ifelse(EndDate %within% FY22, 'FY22',
                                          ifelse(EndDate %within% FY23, 'FY23',"N/A")))))
)
TrainSummary$FiscalYear <- as.factor(TrainSummary$FiscalYear)


#Add demographics to Tenets Evals
TenetswDemos <- TenetsEval %>%
  left_join(RegistDemo, by = c("ParticipantID" = "ParticipantId", "TrainingName" = "TrainingName",
                               "TrainingTopic" = "TrainingTopic", "StartDate" = "StartDate", "EndDate" = "EndDate"))


##prep for DC05 training results
#get DC05 demographics for joining, remove columns that will be duplicated by DC05prepost
DC05demos <- RegistDemo %>%
  filter(TrainingTopic=='DC:0-5') %>%
  select(-TrainingTopic, -Location)


#add demos to training results:
DC05prepost <- DC05prepost %>%
  left_join(DC05demos, by = c("ParticipantIdNew" = "ParticipantId", "StartDate" = "StartDate")) %>%
  select(-ParticipantID) %>%
  rename("ParticipantID" = ParticipantIdNew)


# One Week Follow-Up ------------------------------------------------------


##1wk follow-up question
#excel files only have first name, last name, email, so need to join to participant demographics using first name + last name to get the participant ID
#with participant ID, can then link responses back to eval results or pre/post (for trainings where that would be relevant)
#example code to bring in all 1wk follow-up files 
OneWeekFU <- 
  list.files(path = "/home/Public/Nicole/1wkFollowUps", pattern = "*.csv", full.names = T) %>%
  map_df(~read.csv(.))

#DOWNLOAD NEW FILES (current through 6/8/2020)

#need to append a column to each file with the training type (get rest of info once join to demographics)
##***UPDATE THIS!! (current through 6/8/2020)
OneWeekFU$Campaign.Name <- as.factor(OneWeekFU$Campaign.Name)
levels(OneWeekFU$Campaign.Name)
levels(RegistDemo$TrainingName)
OneWeekFU <- OneWeekFU %>%
  mutate(TrainingName = ifelse(Campaign.Name=="RS-CO Aug 16 DHS follow up","RS-CO Aug 16 DHS",
                               ifelse(Campaign.Name=="RS-CO Aug 9 Tenets Follow up","RS-CO August 9 Tenets",
                                       ifelse(Campaign.Name=="RS-CO Jan. 13,14 DC05 Follow up","RS-CO Jan 13,14 2020 DC:0-5 Training",
                                               ifelse(Campaign.Name=="RS-CO Jan. 27,28 DC05 Follow up","RS-CO Jan 27, 28 DC:0-5 Training",
                                                       ifelse(Campaign.Name=="RS-CO July 8,9 DC05 Follow up","RS-CO July 8,9 DC 0-5 Training",
                                                               ifelse(Campaign.Name=="RS-CO Nov. 14,15 DC05 Follow up","RS-CO Nov 14,15 DC:0-5 Training",
                                                                      ifelse(Campaign.Name=="RS-CO Nov. 18 DHS Follow-up","RS-CO Nov. 18 DHS Training",
                                                                             ifelse(Campaign.Name=="RS-CO Sept. 20 DHS Follow-up","RS-CO Sept 20 DHS Training",NA)))))))))
OneWeekFU <- OneWeekFU %>%
  left_join(RegistDemo, by = c("First.Name" = "FirstName", "Last.Name" = "LastName", "Email.Address" = "Email", "TrainingName"))





# 6 Month Follow-Up Surveys:Quant and Qual -----------------------------------------------
##all done in Qualtrics
#Individual evaluations have questions specific to that training
#general open-ended responses for all trainings are in SixMoFUqual
SixMoFUqual



# Pivoting and Subsetting Data --------------------------------------------

#need to turn "NA" into a factor level for all factor data
RegistDemo <- RegistDemo %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "Missing")
EvalsWdemos <- EvalsWdemos %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "Missing")
  

#want eval response data in long form for analysis, but keeping  all demographic info in wide form
#this keeps only respnses from people who attended the training (which serves to limit the number of missing data points)
#do the same thing ^^ with DC0-5 data (and will need to do same for other pre/posts as we get them)
colnames(EvalsWdemos)
EvalsDemoQuant <- EvalsWdemos %>%
  gather("Question", "Response", 5:30) %>%
  select(-contains("OpenEnded"))


#using wideform, Adding new columns for average score for each domain
colnames(EvalsWdemos)
EvalsWdemos[,5:30] <- lapply(EvalsWdemos[,5:30], as.numeric)
EvalsWdemos$Satisfaction <- rowMeans(EvalsWdemos[5:7], na.rm = TRUE)
EvalsWdemos$Instruction <- rowMeans(EvalsWdemos[8:21], na.rm = TRUE)
EvalsWdemos$Relationships <- rowMeans(EvalsWdemos[22:25], na.rm = TRUE)
EvalsWdemos$LearningObj <- rowMeans(EvalsWdemos[26:29], na.rm = TRUE)

#recoding to factors
colnames(EvalsWdemos)
EvalsWdemos[,5:30] <- lapply(EvalsWdemos[,5:30], as.character)
EvalsWdemos[,5:30] <- lapply(EvalsWdemos[,5:30], factor, levels=c("1", "2", "3", "4", "5"))
EvalsWdemos[,c(2,64)] <- lapply(EvalsWdemos[,c(2,64)], as.factor)
EvalsWdemos[,67:70] <- lapply(EvalsWdemos[,67:70], as.numeric)


#split domain scores columns into new dataframe
Evaldomains <- EvalsWdemos %>%
  select(EvalPartID, TrainingName, TrainingTopic, StartDate, EndDate, Satisfaction, Instruction, Relationships, LearningObj, OverallValue,
         `Reason for Attending`, Gender, Ethnicity, Organization, Language, County, Education, Experience, Role, Age, Attended, ClinicalProfessional,
         FiscalYear)
Evaldomains[6:9] <-  round(Evaldomains[6:9], digits = 0)


#recode responses back to factors with phrases
Evaldomains[,6:9] <- lapply(Evaldomains[,6:9], factor, levels=c("1", "2", "3", "4", "5"))
Evaldomains <- Evaldomains %>%
  mutate_at(vars(6:9),
            .funs = forcats::fct_recode,
            "Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5") 

levels(Evaldomains$Satisfaction)
#Check that NaN evaluates to NA
is.na(Evaldomains$Satisfaction)
#doesn't evaluate to NA in Phrase factors, need to convert
Evaldomains[,6:9] <- na_if(Evaldomains[,6:9], "NaN")

Evaldomains <- Evaldomains %>%
  rename("Learning Objectives" = LearningObj)

#need responses as numeric too (domains) 
EvaldomainsNum <- Evaldomains
EvaldomainsNum[,6:10] <- lapply(EvaldomainsNum[,6:10], as.numeric)

#set factor/Likert responses as phrases
colnames(EvalsWdemos)
EvalsWdemos <- EvalsWdemos %>%
  mutate_at(vars(5:29),
            .funs = forcats::fct_recode,
            "Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5") 



# Clean Up Factor Levels, etc for Better Plots ----------------------------

# % agree or strongly agree to Satisfaction, Instruction, Relationships, Learning Objectives, Overall on eval
# compare % agree or strongly agree across trainings

#currently looks at ALL, not separated by fiscal year. NEED TO UPDATE THIS if want to compare across years *****

#reorder factor levels for better plots - reorder for both Evaldomains and EvalsWdemos dfs
#set both "" and "Missing" values to be "Missing"
colnames(EvalsWdemos)
Evaldomains[,11:19] <- lapply(Evaldomains[,11:19], fct_recode, "Missing" = "")
EvalsWdemos[,c(53,55:61,63)] <- lapply(EvalsWdemos[,c(53,55:61,63)], fct_recode, "Missing" = "")

levels(Evaldomains$Role)
Evaldomains$Ethnicity <- Evaldomains$Ethnicity %>%
  fct_relevel("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latinx", "Native Hawaiian or Other Pacific Islander",
              "White/Non-Hispanic", "Other", "Two or More Ethnicities", "Prefer to Self-Describe", "Prefer Not to Answer", "Missing")
Evaldomains$Language <- Evaldomains$Language %>%
  fct_relevel("English", "Spanish")
Evaldomains$Experience <- Evaldomains$Experience %>%
  fct_relevel("11+", after = 4)
Evaldomains$Education <- Evaldomains$Education %>%
  fct_relevel("High School Diploma/High School Equivalency", "Associates Degree", "Some College", "Bachelor's Degree (BA, BS)", "Master's Degree", 
              "Doctoral Degree (PhD, PsyD, EdD, MD, JD)", "Missing")
Evaldomains$Role <- fct_collapse(Evaldomains$Role, "Early Interventionist" = c("Early Intervention Provider (Non-Mental Health)", 
                                                                               "Early Interventionist (e.g., SLP, PT, OT)"))
Evaldomains$Role <- fct_collapse(Evaldomains$Role, "Administrator/Manager/Director (Non-Clinical)" = c("Administrator (Non-Clinical)",
                                                                                                       "Administrator/Manager/Director (Non-Clinical)"))
Evaldomains$Role <- Evaldomains$Role %>%
  fct_relevel("Missing", after = Inf) %>%
  fct_relevel("Other", after = 111)

levels(EvalsWdemos$Role)
EvalsWdemos$Ethnicity <- EvalsWdemos$Ethnicity %>%
  fct_relevel("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latinx", "Native Hawaiian or Other Pacific Islander",
              "White/Non-Hispanic", "Other", "Two or More Ethnicities", "Prefer to Self-Describe", "Prefer Not to Answer", "Missing")
EvalsWdemos$Language <- EvalsWdemos$Language %>%
  fct_relevel("English", "Spanish")
EvalsWdemos$Experience <- EvalsWdemos$Experience %>%
  fct_relevel("11+", after = 4)
EvalsWdemos$Education <- EvalsWdemos$Education %>%
  fct_relevel("High School Diploma/High School Equivalency", "Associates Degree", "Some College", "Bachelor's Degree (BA, BS)", "Master's Degree", 
              "Doctoral Degree (PhD, PsyD, EdD, MD, JD)", "Missing")
EvalsWdemos$Role <- fct_collapse(EvalsWdemos$Role, "Early Interventionist" = c("Early Intervention Provider (Non-Mental Health)", 
                                                                               "Early Interventionist (e.g., SLP, PT, OT)"))
EvalsWdemos$Role <- fct_collapse(EvalsWdemos$Role, "Administrator/Manager/Director (Non-Clinical)" = c("Administrator (Non-Clinical)",
                                                                                                       "Administrator/Manager/Director (Non-Clinical)"))
EvalsWdemos$Role <- EvalsWdemos$Role %>%
  fct_relevel("Missing", after = Inf) %>%
  fct_relevel("Other", after = 11)

#removing TrainingTopic=NA because there's three rows with no data
Evaldomains <- Evaldomains %>%
  filter(!is.na(TrainingTopic), TrainingTopic!="Missing")



#looking at learning objectives for each training - Are respondents consistently "not getting" one of them?
LearningObjectives <- EvalsWdemos %>%
  filter(!is.na(TrainingTopic), TrainingTopic!="Missing") %>%
  select(EvalPartID, TrainingName, TrainingTopic, StartDate, EndDate, LearningObj1, LearningObj2, LearningObj3, LearningObj4, 
         `Reason for Attending`, Gender, Ethnicity, Organization, Language, County, Education, Experience, Role, Age, 
         Attended, ClinicalProfessional, FiscalYear)

Satisfaction <- EvalsWdemos %>%
  filter(!is.na(TrainingTopic), TrainingTopic!="Missing") %>%
  select(EvalPartID, TrainingName, TrainingTopic, StartDate, EndDate, Satisfaction1, Satisfaction2, Satisfaction3, 
         `Reason for Attending`, Gender, Ethnicity, Organization, Language, County, Education, Experience, Role, Age, 
         Attended, ClinicalProfessional, FiscalYear)

Instruction <- EvalsWdemos %>%
  filter(!is.na(TrainingTopic), TrainingTopic!="Missing") %>%
  select(EvalPartID, TrainingName, TrainingTopic, StartDate, EndDate, Instruction1, Instruction2, Instruction3, Instruction4, Instruction5, Instruction6,
         Instruction7, Instruction1b, Instruction2b, Instruction3b, Instruction4b, Instruction5b, Instruction6b, Instruction7b,
         `Reason for Attending`, Gender, Ethnicity, Organization, Language, County, Education, Experience, Role, Age, 
         Attended, ClinicalProfessional, FiscalYear)
#create avg instruction columns ??

Relationships <- EvalsWdemos %>%
  filter(!is.na(TrainingTopic), TrainingTopic!="Missing") %>%
  select(EvalPartID, TrainingName, TrainingTopic, StartDate, EndDate, Relationships1, Relationships2, Relationships3, Relationships4, 
         `Reason for Attending`, Gender, Ethnicity, Organization, Language, County, Education, Experience, Role, Age, 
         Attended, ClinicalProfessional, FiscalYear)

#Check into what might be driving differences in Learning Objectives, Relationships, and Satisfaction
#Could just be due to training topic
##stratifying eval results (each domain) by demographics

###******DO THIS WITH LIKERT PLOTS FOR % BY DEMOGRAPHICS*******

EvalDomainsClinican <- Evaldomains %>%
  filter(!is.na(ClinicalProfessional))

##stratifying eval results for specific trainings by demographics
###this is limited to just a handful of the trainings because we don't have participant IDs for all eval results
###(partially due to developing data collection and management systems while collecting data)

#ANOVA to test differences in survey results by TrainingTopic
#*Don't do ttests for this stuff yet - it's not meaningful
#use EvalDomainsNum

#in future years, when have more data, look at differences by demographics within specific trainings.

#**CHECK LIKERT PLOT RESULTS FOR DETERMINING WHICH PLOTS TO INCLUDE AND WHAT TO WRITE IN NARRATIVE**



# Qualitative Analysis of Post Evals --------------------------------------

#qualitative analysis of open-ended eval responses previously done in NVivo - determining best method to bring that all back here
#August 2020:  loaded paper responses into Qualtrics and coded there

#Reference COVID and Racism survey reports for how to manipulate qualitative data
QevalsMasterQual


# Foundations Data --------------------------------------------------------


#Prep Foundations pre/post test data
##Join excel files and qualtrics files together
##load cleaned excel data
sapply(c('CoAIMHprePostFU.Rdata', 'CoAIMHprePost.Rdata', 'CoAIMHpostFU.Rdata', 'CoAIMHpostFU_IDonly.Rdata', 'CoAIMHprePost_IDonly.Rdata', 'CoAIMHprePostFU_IDonly.Rdata'), 
       load, .GlobalEnv)

#RS qualtrics Foundations df: RS-CO Foundations Post Evaluation, RS-CO Foundations Follow-up
#Qualtrics surveys that contain the pre/post questions are RSCO_FoundationsPostTest and RSCO_FoundationsFUtest
colnames(RSCO_FoundationsPostTest)
colnames(RSCO_FoundationsFUtest)

RSCO_FoundationsPostTest <- RSCO_FoundationsPostTest %>%
  select(-c(RecipientLastName, RecipientFirstName, RecipientEmail)) %>%
  rename(Confidence1 = "Q13_1", Confidence2 = "Q13_2", Confidence3 = "Q13_3", Confidence4 = "Q13_4", Confidence5 = "Q13_5", Confidence6 = "Q13_6", 
         Confidence7 = "Q13_7", Confidence8 = "Q13_8", Confidence9 = "Q13_9", "ParticipantID" = ExternalReference)

RSCO_FoundationsFUtest <- RSCO_FoundationsFUtest %>%
  select(-c(RecipientLastName, RecipientFirstName, RecipientEmail)) %>%
  rename(Confidence1 = "Q2_1", Confidence2 = "Q2_2", Confidence3 = "Q2_3", Confidence4 = "Q2_4", Confidence7 = "Q2_5", Confidence8 = "Q2_6", Confidence9 = "Q2_7",
         "ParticipantID" = ExternalReference)


#start with Qualtrics versions of pre/post/FU as base and add in relevant CoAIMH domains
colnames(CoAIMHpostFU_IDonly)
RSCO_FoundationsPostFinal <- RSCO_FoundationsPostTest %>%
  mutate(Timepoint=2) %>%
  bind_rows(CoAIMHpostFU_IDonly %>%
              filter(Timepoint==2) %>%
              select(1:2,9:17) %>%
              mutate(EvalName="CoAIMH Survey Monkey"))

RSCO_FoundationsFUfinal <- RSCO_FoundationsFUtest %>%
  mutate(Timepoint=3) %>%
  bind_rows(CoAIMHpostFU_IDonly %>%
              filter(Timepoint==3) %>%
              select(1:2,9:17) %>%
              mutate(EvalName="CoAIMH Survey Monkey"))


#Add Qualtrics Foundations Post and FU to appropriate dfs
##CoAIMH pre+post = IMH attitudes + Knowledge (nothing from RSCO versions to add)
##CoAIMH pre+post+FU = IMH attitudes (nothing from RSCO versions to add)
##CoAIMH post+FU = IMH attitudes + Confidence/Skills
#RSCO pre+post+FU = Confidence/Skills
# RSCO_FoundationsFUtest$ParticipantID <- as.character(RSCO_FoundationsFUtest$ParticipantID)
# RSCO_FoundationsPostTest$ParticipantID <- as.character(RSCO_FoundationsPostTest$ParticipantID)

CoAIMHpostFU_IDonly <- CoAIMHpostFU_IDonly %>%
  bind_rows(RSCO_FoundationsPostTest %>%
               select(3:12) %>%
              mutate(Timepoint=2)) %>%
  bind_rows(RSCO_FoundationsFUtest %>%
               select(3:10)%>%
              mutate(Timepoint=3))


#ensure columns are factors for likert analysis
# colnames(RSCO_FoundationsPostFinal)
RSCO_FoundationsPostFinal[,4:12] <- lapply(RSCO_FoundationsPostFinal[,4:12], factor, levels=c("Not at all Confident", "Somewhat Confident", "Confident", "Very Confident"))

# colnames(RSCO_FoundationsFUfinal)
#need to reorder columns
RSCO_FoundationsFUfinal <- RSCO_FoundationsFUfinal %>%
  select(1:7,13,14,8:12)

#determine sets of variables to convert
# colnames(RSCO_FoundationsFUfinal)
RSCO_FoundationsFUfinal[,4:12] <- lapply(RSCO_FoundationsFUfinal[,4:12], factor, levels=c("Not at all Confident", "Somewhat Confident", "Confident", "Very Confident"))

# colnames(CoAIMHprePostFU)
CoAIMHprePostFU[,4:7] <- lapply(CoAIMHprePostFU[,4:7], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHprePostFU$Timepoint <- as.factor(CoAIMHprePostFU$Timepoint)

# colnames(CoAIMHpostFU)
CoAIMHpostFU[,3:8] <- lapply(CoAIMHpostFU[,3:8], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHpostFU[,9:17] <- lapply(CoAIMHpostFU[,9:17], factor, levels=c("Not at all Confident", "Somewhat Confident", "Confident", "Very Confident"))
CoAIMHpostFU$Timepoint <- as.factor(CoAIMHpostFU$Timepoint)

# colnames(CoAIMHprePost)
CoAIMHprePost[,3:6] <- lapply(CoAIMHprePost[,3:6], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHprePost$Timepoint <- as.factor(CoAIMHprePost$Timepoint)


# colnames(CoAIMHprePostFU_IDonly)
CoAIMHprePostFU_IDonly[,4:7] <- lapply(CoAIMHprePostFU_IDonly[,4:7], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHprePostFU_IDonly$Timepoint <- as.factor(CoAIMHprePostFU_IDonly$Timepoint)

# colnames(CoAIMHpostFU_IDonly)
CoAIMHpostFU_IDonly[,3:8] <- lapply(CoAIMHpostFU_IDonly[,3:8], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHpostFU_IDonly[,9:17] <- lapply(CoAIMHpostFU_IDonly[,9:17], factor, levels=c("Not at all Confident", "Somewhat Confident", "Confident", "Very Confident"))
CoAIMHpostFU_IDonly$Timepoint <- as.factor(CoAIMHpostFU_IDonly$Timepoint)

# colnames(CoAIMHprePost_IDonly)
CoAIMHprePost_IDonly[,3:6] <- lapply(CoAIMHprePost_IDonly[,3:6], factor, levels=c("Stongly Disagree", "Disagree", "Agree", "Strongly Agree"))
CoAIMHprePost_IDonly$Timepoint <- as.factor(CoAIMHprePost_IDonly$Timepoint)

#join Foundations dfs to registdemos
##Need to be sure only joining the relevant RegistDemo row!!
RSCO_FoundationsPostFinal <- RSCO_FoundationsPostFinal %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
RSCO_FoundationsFUfinal <- RSCO_FoundationsFUfinal %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
CoAIMHprePostFU <- CoAIMHprePostFU %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
CoAIMHpostFU <- CoAIMHpostFU %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
CoAIMHprePost <- CoAIMHprePost %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))


CoAIMHprePostFU_IDonly <- CoAIMHprePostFU_IDonly %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
CoAIMHpostFU_IDonly <- CoAIMHpostFU_IDonly %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))
CoAIMHprePost_IDonly <- CoAIMHprePost_IDonly %>%
  left_join((RegistDemo %>%
               filter(TrainingTopic=="Foundations")), by = c("ParticipantID" = "ParticipantId"))

#recode timepoint factors as words
CoAIMHprePostFU_IDonly$Timepoint <- fct_recode(CoAIMHprePostFU_IDonly$Timepoint, "Pre" = "1", "Post" = "2", "FollowUp" = "3")
CoAIMHprePost_IDonly$Timepoint <- fct_recode(CoAIMHprePost_IDonly$Timepoint, "Pre" = "1", "Post" = "2")
CoAIMHpostFU_IDonly$Timepoint <- fct_recode(CoAIMHpostFU_IDonly$Timepoint, "Post" = "2", "FollowUp" = "3")

#bind RSCO_FoundationsPost and RSCO_FoundationsFU together
RSCO_FoundationsPostFUfinal <- RSCO_FoundationsPostFinal %>%
  rbind(RSCO_FoundationsFUfinal)
RSCO_FoundationsPostFUfinal$Timepoint <- as.character(RSCO_FoundationsPostFUfinal$Timepoint)
RSCO_FoundationsPostFUfinal$Timepoint <- fct_recode(RSCO_FoundationsPostFUfinal$Timepoint, "Post" = "2", "FollowUp" = "3")


#in reports, use CoAIMH____ dfs for data including ALL Foundations pre/post from CoAIMH and RS
##Start with _IDonly dfs for grouping by participantID and excluding NA participantID
#use RSCO_Found___ dfs for data that's focused on the RSCO interests (confidence in skills)

# #need to account for missing data - some people only have data for one or two timepoints
# CoAIMHprePostFU_IDonly %>%
#   group_by(Timepoint) %>%
#   summarise(Count = n_distinct(ParticipantID))
