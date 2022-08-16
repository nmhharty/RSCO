#Script for pulling closed Right Start for Colorado training surveys from Qualtrics, restructuring and pulling together for use in QualtricsTrainingEvals.R
#Initial creation: 8/4/2020
#Updated: 8/4/2020

library(qualtRics)
library(tidyverse)
library(fuzzyjoin)

#connect to qualtrics
source('../QualtricsConnection_nharty.R')
readRenviron("~/.Renviron")


# Load Surveys ------------------------------------------------------------

QsurveysCLOSED <- all_surveys() %>%
  filter(str_detect(name,"RS-CO"), isActive=="FALSE")

#load closed surveys
QsurveysCLOSED$name

ClosedSurveyIDlist <- (QsurveysCLOSED$id)
for(i in ClosedSurveyIDlist) {
  surveyname <- QsurveysCLOSED %>%
    filter(id==i) %>%
    select(name) %>%
    pull()
  df <- fetch_survey(i, start_date = "2019-01-01",
                     label = TRUE, force_request = TRUE) %>%
    #remove unnecessary columns from each survey df (in bulk, then edit for each survey specifically)
    filter(DistributionChannel!="preview") %>%
    select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>%
    mutate(EvalName=(QsurveysCLOSED %>%
                       filter(id==i) %>%
                       select(name) %>%
                       pull()))
  assign(surveyname,df)
}

QsurveysCLOSED$name


# DDHS --------------------------------------------------------------------
##DDHS evals are the same, except for learning objectives. They basically mirror the standard eval, but ask for unique identifiers

#surveys that need to connect to registrations for participantID: all DDHS trainings (Q11)
source('../BI01_RODBC.R')
DDHSidetifiers <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TrainingRegistrationsFinal') %>%
  filter(str_detect(EventName,"DHS"), EventStartDateTime>"2020-01-31") %>%
  select(Attended, EventName, EventStartDateTime, ParticipantId, 
         "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...") %>%
  rename(UniqueID = "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...")


`RS-CO April DDHS Evaluation` <-  DDHSidetifiers %>%
  filter(EventName=="RS-CO April 16 DHS Training") %>%
  regex_right_join(`RS-CO April DDHS Evaluation`, by = c("UniqueID" = "Q11"), ignore_case = TRUE) %>%
  #adding in Q12 for "overall rating" that was missing in this eval
  mutate(Q12 = NA)

`RS-CO March DDHS Evaluation` <-  DDHSidetifiers %>%
  filter(EventName=="RS-CO Mar. 26 DHS Training") %>%
  regex_right_join(`RS-CO March DDHS Evaluation`, by = c("UniqueID" = "Q11"), ignore_case = TRUE)%>%
  #adding in Q12 for "overall rating" that was missing in this eval
  mutate(Q12 = NA)

# `RS-CO July DDHS Evaluation` <-  DDHSidetifiers %>%
#   filter(EventName=="RS-CO 7/23/2020 DHS Foster Support Workers") %>%
#   regex_right_join(`RS-CO July DDHS Evaluation`, by = c("UniqueID" = "Q11"), ignore_case = TRUE)


#***ADD IN ADDITIONAL MONTHLY DDHS SERIES EVALS HERE


MonthlyChildWelfare <- `RS-CO April DDHS Evaluation` %>%
  rbind(`RS-CO March DDHS Evaluation`) %>%
  # rbind(`RS-CO July DDHS Evaluation`) %>%
  mutate(ExternalReference = ParticipantId)

colnames(MonthlyChildWelfare)
MonthlyChildWelfare <- MonthlyChildWelfare %>%
  rename(Suggestions = "Q9 - Topics", MostValuable = "Q6 - Topics", ApplyTraining = "Q7 - Topics", FollowUpSupport = "Q8 - Topics", Other = "Q10 - Topics") %>%
  select(-c(38,40,42,44,46))

#Rename columns to be meaningful - link back to original eval form
##DDHS Monthly Trainings - basically match paper form so use this as the base (but need to add in the second set of instruction items for when 2 instructors)
###Q2_ are Satisfaction (3)
###Q3_ are Instruction (7)
###Q4 are Relationships (4)
###Q5 are LOs (4)
###Q6 = most valuable lesson
###Q7 = apply training to daily work
###Q8 = follow-up/support
###Q9 = suggestions
###Q20 = other comments
#**Evals pre 5/6/2020 are missing overall rating question
colnames(MonthlyChildWelfare)
MonthlyChildWelfare <- MonthlyChildWelfare %>%
  select(-c(Attended, EventName, EventStartDateTime, UniqueID, Q11, `Duration (in seconds)`, ParticipantId)) %>%
  mutate(OverallRating = Q12, Instruction1b = as.character(NA), Instruction2b = as.character(NA), Instruction3b = as.character(NA), 
         Instruction4b = as.character(NA), Instruction5b = as.character(NA), Instruction6b = as.character(NA), Instruction7b = as.character(NA)) %>%
  select(-Q12)
MonthlyChildWelfare[,3:5] <- lapply(MonthlyChildWelfare[,3:5], as.character)
MonthlyChildWelfare$OverallRating <- as.character(MonthlyChildWelfare$OverallRating)

# Other Trainings ---------------------------------------------------------

OTHERidetifiers <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TrainingRegistrationsFinal') %>%
  filter(EventStartDateTime>"2020-01-31") %>%
  select(Attended, EventName, EventStartDateTime, ParticipantId, 
         "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...") %>%
  rename(UniqueID = "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...")

#surveys that have ParticipantID: April 2020 Self-Care Webinar

# "RS-CO April 2020 Self-Care Webinar" Instruction excludes items 3, 4, 5 **ADDS** Relationships item 1
#Q2 matches, Q3_1 and Q3_2 match
#Q3_3 is actually Relationships - matches Q4_1 in DDHS
#Only 2 LOs
#Add in columns for missing questions; rename existing questions as needed
`RS-CO April 2020 Self-Care Webinar` <- `RS-CO April 2020 Self-Care Webinar` %>%
  rename("Q4_1" = Q3_3,
         "Suggestions" = "Q9 - Topics", "MostValuable" = "Q6 - Topics", "ApplyTraining" = "Q7 - Topics", "FollowUpSupport" = "Q8 - Topics", "Other" = "Q10 - Topics") %>%
  mutate(Q3_3 = as.character(NA), Q3_4 = as.character(NA), Q3_5 = as.character(NA), Q3_6 = as.character(NA), Q3_7 = as.character(NA), Q4_2 = as.character(NA), 
         Q4_3 = as.character(NA), Q4_4 = as.character(NA), Q5_3 = as.character(NA), Q5_4 = as.character(NA),
         OverallRating = as.character(NA), Instruction1b = as.character(NA), Instruction2b = as.character(NA), Instruction3b = as.character(NA), 
         Instruction4b = as.character(NA), Instruction5b = as.character(NA), Instruction6b = as.character(NA), Instruction7b = as.character(NA)) %>%
  select(-c(`Q9 - Parent Topics`, `Q6 - Parent Topics`, `Q7 - Parent Topics`, `Q8 - Parent Topics`, `Q10 - Parent Topics`, `Duration (in seconds)`))


# "RS-CO April DDHS COVID-19 Response Training Evaluation" excludes Instruction items 6, 7; Relationships item2; Only 2 learning objectives instead of 4
#Q2, Q3 match DDHS (Instruction items skipped are last items, so Q3_1 through Q3_5 match)
#Q4 Relationships: Q4_1 matches, but others are off
#Add in columns for missing questions; rename existing questions as needed
`RS-CO April DDHS COVID-19 Response Training Evaluation` <-  DDHSidetifiers %>%
  filter(EventName=="RS-CO April 10 DHS Training Series") %>%
  regex_right_join(`RS-CO April DDHS COVID-19 Response Training Evaluation`, by = c("UniqueID" = "Q11"), ignore_case = TRUE) %>%
  mutate(ExternalReference = ParticipantId, Other = as.character(NA))

`RS-CO April DDHS COVID-19 Response Training Evaluation` <- `RS-CO April DDHS COVID-19 Response Training Evaluation` %>%
  select(-c(32,33,35,37,39,41)) %>%
  rename(Suggestions = "Q9 - Topics", MostValuable = "Q6 - Topics", ApplyTraining = "Q7 - Topics", FollowUpSupport = "Q8 - Topics")

`RS-CO April DDHS COVID-19 Response Training Evaluation` <- `RS-CO April DDHS COVID-19 Response Training Evaluation` %>%
  rename("Q4_4" = Q4_3, "Q4_3" = Q4_2) %>%
  mutate(Q3_6 = as.character(NA), Q3_7 = as.character(NA), Q4_2 = as.character(NA), Q5_3 = as.character(NA), Q5_4 = as.character(NA),
         OverallRating = as.character(NA), Instruction1b = as.character(NA), Instruction2b = as.character(NA), Instruction3b = as.character(NA), 
         Instruction4b = as.character(NA), Instruction5b = as.character(NA), Instruction6b = as.character(NA), Instruction7b = as.character(NA)) %>%
  select(-c(Q11, UniqueID, EventStartDateTime, EventName, Attended, ParticipantId, `Duration (in seconds)`))
`RS-CO April DDHS COVID-19 Response Training Evaluation`[,3:5] <- lapply(`RS-CO April DDHS COVID-19 Response Training Evaluation`[,3:5], as.character)


#July 2020 Montrose River Valley FQHC IECMH Overview
###Q2_ are Satisfaction (3)
###Q3_ are Instruction (7)
###Q4 are Relationships (4)
###Q5 are LOs (4)
###Q6 = most valuable lesson
###Q7 = apply training to daily work
###Q8 = follow-up/support
###Q9 = suggestions
###Q10 = other comments

`RS-CO July IECMH Overview Evaluation` <-  OTHERidetifiers %>%
  filter(EventName=="RS-CO 7/21/2020 Montrose Training") %>%
  regex_right_join(`RS-CO July IECMH Overview Evaluation`, by = c("UniqueID" = "Q11"), ignore_case = TRUE)

`RS-CO July IECMH Overview Evaluation` <- `RS-CO July IECMH Overview Evaluation` %>%
  rename(Suggestions = "Q9 - Topics", MostValuable = "Q6 - Topics", ApplyTraining = "Q7 - Topics", FollowUpSupport = "Q8 - Topics", Other = "Q10 - Topics") %>%
  select(-c(38,41,43,45,47)) %>%
  mutate(ExternalReference=ParticipantId)

`RS-CO July IECMH Overview Evaluation` <- `RS-CO July IECMH Overview Evaluation` %>%
  select(-c(Attended, EventName, EventStartDateTime, UniqueID, Q11, ParticipantId, `Duration (in seconds)`)) %>%
  mutate(OverallRating = Q12, Instruction1b = as.character(NA), Instruction2b = as.character(NA), Instruction3b = as.character(NA), 
         Instruction4b = as.character(NA), Instruction5b = as.character(NA), Instruction6b = as.character(NA), Instruction7b = as.character(NA)) %>%
  select(-Q12)
`RS-CO July IECMH Overview Evaluation`[,3:5] <- lapply(`RS-CO July IECMH Overview Evaluation`[,3:5], as.character)


# `RS-CO Aug 14 DC05 Overview Evaluation`


# SQL Eval Data Entry Qualitative-----------------------------------------------------
`RS-CO SQL Import Evaluation OE Responses` <- fetch_survey("SV_3rRaLlRbCsoCBPT", label = TRUE, force_request = TRUE) %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>%
  mutate(EvalName="RS-CO SQL Import Evaluation OE Responses")


`RS-CO SQL Import Evaluation OE Responses` <- `RS-CO SQL Import Evaluation OE Responses` %>%
  mutate(ExternalReference=Q11) %>%
  rename("TrainingName" = Q13, MostValuableFull = "Q6", ApplyTrainingFull = "Q7", FollowUpSupportFull = "Q8", SuggestionsFull = "Q9", OtherFull = "Q10", 
         Suggestions = "Q9 - Topics", MostValuable = "Q6 - Topics", ApplyTraining = "Q7 - Topics", FollowUpSupport = "Q8 - Topics", Other = "Q10 - Topics") %>%
  select(-c(1,9,16,18,20,22,24))




# Build Combined dfs ------------------------------------------------------

QcolNames <- c("Q2_1","Q2_2","Q2_3","Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q4_1","Q4_2","Q4_3","Q4_4","Q5_1","Q5_2","Q5_3","Q5_4","Q6","Q7","Q8","Q9","Q10")
ShortName <- c("Satisfaction1","Satisfaction2","Satisfaction3","Instruction1","Instruction2","Instruction3","Instruction4","Instruction5","Instruction6","Instruction7",
               "Relationships1","Relationships2","Relationships3","Relationships4","LearningObj1","LearningObj2","LearningObj3","LearningObj4","OpenEnded1","OpenEnded2",
               "OpenEnded3","OpenEnded4","OpenEnded5")

ClosedEvalsMaster <- MonthlyChildWelfare %>%
  dplyr::union(`RS-CO April DDHS COVID-19 Response Training Evaluation`) %>%
  dplyr::union(`RS-CO April 2020 Self-Care Webinar`) %>%
  dplyr::union(`RS-CO July IECMH Overview Evaluation`)

#Rename columns to match Evals df in Initial_Training_Data_Prep_Analysis
##**This is done in BASE R -- figure out how to do this in dplyr with rename_at ?? it's weird because I can't figure out the function to use within replace_at
colnames(ClosedEvalsMaster)[which(colnames(ClosedEvalsMaster) %in% QcolNames)] <- ShortName


#recode respondes in ClosedEvalsMaster to be number factors rather than words
colnames(ClosedEvalsMaster)
ClosedEvalsMaster[,c(7:24,37:43)] <- lapply(ClosedEvalsMaster[,c(7:24,37:43)], factor, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
#used this to check number of responses of each category to make sure recoding worked
# QevalsMaster %>%
#   select(8:25,32:39) %>%
#   gather(Item,Value,1:26) %>%
#   group_by(Value) %>%
#   summarise(n())
# 
# QevalsMaster %>%
#   mutate_at(vars(8:25,32:39),
#             .funs = forcats::fct_recode,
#             "1"="Strongly disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Strongly agree") %>%
#   gather(Item,Value,c(8:25,32:39)) %>%
#   group_by(Value) %>%
#   summarise(n())

ClosedEvalsMaster <- ClosedEvalsMaster %>%
  mutate_at(vars(7:24,37:43),
            .funs = forcats::fct_recode,
            "1"="Strongly disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Strongly agree") 


save(ClosedEvalsMaster,file="ClosedEvalsMaster.Rdata")
