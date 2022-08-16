#Script for pulling all Right Start for Colorado training surveys from Qualtrics, restructuring and pulling together for use in Initial_Training_Data_PrepAnalysis.R
#Initial creation: 5/5/2020
#Updated: 8/3/2020


library(qualtRics)
library(tidyverse)
library(fuzzyjoin)

#connect to qualtrics
source('../QualtricsConnection_nharty.R')
readRenviron("~/.Renviron")

ActiveQsurveys <- all_surveys() %>%
  filter(str_detect(name,"RS-CO"), isActive=="TRUE")



# Load Surveys from Qualtrics ---------------------------------------------


#load in all RSCO surveys - not all are identical
#see notes about eval differences in OneNote: Nicole's personal OneNote > RS-CO > Data Collection, Reporting, and Analysis > *Workforce Data Nuances

#Fist, load Closed Evals (those that are for stand-alone trainings not used repeatedly)

ActiveQsurveys$name

SurveyIDlist <- (ActiveQsurveys$id)
for(i in SurveyIDlist) {
  surveyname <- ActiveQsurveys %>%
    filter(id==i) %>%
    select(name) %>%
    pull()
  df <- fetch_survey(i, start_date = "2019-01-01",
                     label = TRUE, force_request = TRUE) %>%
#remove unnecessary columns from each survey df (in bulk, then edit for each survey specifically)
    filter(DistributionChannel!="preview") %>%
    select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>%
    mutate(EvalName=(ActiveQsurveys %>%
                        filter(id==i) %>%
                        select(name) %>%
                        pull()))
  assign(surveyname,df)
}


# DDHS --------------------------------------------------------------------
##DDHS evals are the same, except for learning objectives. They basically mirror the standard eval, but ask for unique identifiers

#surveys that have ParticipantID: April 2020 Self-Care Webinar, Foundations Post Evaluation ; all 6mo follow-ups
#surveys that need to connect to registrations for participantID: all DDHS trainings (Q11)
# source('../BI01_RODBC.R')
# DDHSidetifiers <- sqlQuery(BI01, 'SELECT * FROM DataEntry.RightStart.TrainingRegistrationsFinal') %>%
#   filter(str_detect(EventName,"DHS"), EventStartDateTime>"2020-01-31") %>%
#   select(Attended, EventName, EventStartDateTime, ParticipantId, 
#          "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...") %>%
#   rename(UniqueID = "Please create a unique identifier in the format of initials and date of birth (first letter of first name + first letter of l...")


# Foundations -------------------------------------------------------------

# "RS-CO Foundations Post Evaluation" CoAIMH Foundations training post eval - slightly different from other evals
#Q2 matches
#Q4 is Instruction and matches Q3 items in DDHS
#Q5 is Relationships and matches Q4 itmes in DDHS
#Q6 is LOs and matches Q5 items in DDHS
#DOES include overall rating - Q7
#Q13 is Applying Skills section from CoAIMH pre/post test
#Q8, Q10, Q11, Q9 match to Q6, Q7, Q8, Q9 of DDHS
colnames(`RS-CO Foundations Post Evaluation`)
#create separate df for CoAIMH Foundations analysis
RSCO_FoundationsPostTest <- `RS-CO Foundations Post Evaluation` %>%
  select(-c(1,8:26,36:51))


`RS-CO Foundations Post Evaluation` <- `RS-CO Foundations Post Evaluation` %>% 
  rename("Q3_1" = Q4_1, "Q3_2" = Q4_2, "Q3_3" = Q4_3, "Q3_4" = Q4_4, "Q3_5" = Q4_5, "Q3_6" = Q4_6, "Q3_7" = Q4_7,
         "Q4_1" = Q5_1, "Q4_2" = Q5_2, "Q4_3" = Q5_3, "Q4_4" = Q5_4,
         "Q5_1" = Q6_1, "Q5_2" = Q6_2, "Q5_3" = Q6_3, "Q5_4" = Q6_4,
         OverallRating = Q7,
         "MostValuableFull" = "Q8", "ApplyTrainingFull" = "Q10", "FollowUpSupportFull" = "Q11", "SuggestionsFull" = "Q9",
         "Suggestions" = "Q9 - Topics", "MostValuable" = "Q8 - Topics", "ApplyTraining" = "Q10 - Topics", "FollowUpSupport" = "Q11 - Topics"
         ) %>%
  mutate(OtherFull = as.character(NA), Other = as.character(NA), Instruction1b = as.character(NA), Instruction2b = as.character(NA), Instruction3b = as.character(NA), 
         Instruction4b = as.character(NA), Instruction5b = as.character(NA), Instruction6b = as.character(NA), Instruction7b = as.character(NA)) %>%
  select(-c(1,27:35,40:42,43,45,47,49))


# "RS-CO Foundations Follow-up" CoAIMH Foundations training FU eval - slightly different from other 6mo evals
#Q2 is the Applying Skills section (only 7 items though - items 5 and 6 from CoAIMH are skipped)
#Q2 is questions to use for prepost analysis
colnames(`RS-CO Foundations Follow-up`)
#create separate DF for CoAIMH Foundations analysis
RSCO_FoundationsFUtest <- `RS-CO Foundations Follow-up` %>%
  select(-c(1,8,16:31))



# DC0-5 -------------------------------------------------------------------
#PRE assessment is just the pre/post test. POST assessment is pre/post test + evaluation

# `RS-CO Pre-Training Assessment: DC:0-5™ Diagnostic System`

# `RS-CO Post-Training Assessment: DC:0-5™ Diagnostic System`


# Create Master Evals df --------------------------------------------------

#put all evals together (union/rbind)
load("ClosedEvalsMaster.Rdata")
QevalsMaster <- `RS-CO Foundations Post Evaluation` # %>%
# dplyr::union(DC05postEval) %>%
# dplyr::union() %>%
# dplyr::union()

#Create mapping df so have Qualtrics Q numbers mapped to Descriptive Names (i.e. Satisfaction1) and full item

QcolNames <- c("Q2_1","Q2_2","Q2_3","Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q4_1","Q4_2","Q4_3","Q4_4","Q5_1","Q5_2","Q5_3","Q5_4","MostValuableFull",
               "ApplyTrainingFull","FollowUpSupportFull","SuggestionsFull","OtherFull")
ShortName <- c("Satisfaction1","Satisfaction2","Satisfaction3","Instruction1","Instruction2","Instruction3","Instruction4","Instruction5","Instruction6","Instruction7",
               "Relationships1","Relationships2","Relationships3","Relationships4","LearningObj1","LearningObj2","LearningObj3","LearningObj4","OpenEnded1","OpenEnded2",
               "OpenEnded3","OpenEnded4","OpenEnded5")
# FullQuestion <- c()

#Rename columns to match Evals df in Initial_Training_Data_Prep_Analysis
##**This is done in BASE R -- figure out how to do this in dplyr with rename_at ?? it's weird because I can't figure out the function to use within replace_at
colnames(QevalsMaster)[which(colnames(QevalsMaster) %in% QcolNames)] <- ShortName

#recode respondes in QevalsMaster to be number factors rather than words
colnames(QevalsMaster)
QevalsMaster[,c(7:24,37:43)] <- lapply(QevalsMaster[,c(7:24,37:43)], factor, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
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

QevalsMaster <- QevalsMaster %>%
  mutate_at(vars(7:24,37:43),
            .funs = forcats::fct_recode,
            "1"="Strongly disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Strongly agree") 

#put all evals together (union/rbind)
load("ClosedEvalsMaster.Rdata")
QevalsMaster <- QevalsMaster %>%
  dplyr::union(ClosedEvalsMaster) 

#clean up OverallRating column factor levels
QevalsMaster$OverallRating <- as.factor(QevalsMaster$OverallRating)
QevalsMaster$OverallRating <- recode_factor(QevalsMaster$OverallRating, "1 (low)" = "1", "5 (high)" = "5")
QevalsMaster$OverallRating <- fct_relevel(QevalsMaster$OverallRating, "5", after = Inf)

# Master Evals df Qualitative ---------------------------------------------
colnames(QevalsMaster)
QevalsMasterQual <- QevalsMaster %>%
  select(c(1:6,26:36))



# 6 Month FollowUp Surveys  ----------------------------------------------------------------------

#6 MONTH FOLLOW-UP SURVEYS - Includes Qualitative Analysis

# "RS-CO DC:0-5 Follow-up" is 6mo DC05 follow-up
# "RS-CO Tenets Follow-up" 6mo Tenets follow-up
# "RS-CO Child Welfare Follow-up" 6mo DDHS follow-up distributed to participants in 3mo batches
# "RS-CO Foundations Follow-up" CoAIMH Foundations training 6mo eval - slightly different from other 6mo follow-ups

# "RS-CO Foundations Follow-up" CoAIMH Foundations training FU eval - slightly different from other 6mo evals
colnames(`RS-CO Foundations Follow-up`)
#Q6 is 6mo FU question about shift in practice
#Q2 is the Applying Skills section (only 7 items though - items 5 and 6 from CoAIMH are skipped)
#Q4 is 6mo FU barriers
#Q5 is 6mo FU most valuable
#Q8 is 6mo FU additional supports
#Q9 is names to give to Theresa for giftcard raffle
`RS-CO Foundations Follow-up` <- `RS-CO Foundations Follow-up` %>%
  select(-c(1,22,24,26,28,30,31)) %>%
  rename(ShiftPracticeFull = "Q6", BarriersFull = "Q4", MostValuableFull = "Q5", SupportFull = "Q8", ShiftPractice = "Q6 - Topics", Barriers = "Q4 - Topics",
         MostValuable = "Q5 - Topics", Support = "Q8 - Topics")


# RS-CO DC:0-5 Follow-up
colnames(`RS-CO DC:0-5 Follow-up`)
#Q6 shift in practice
#Q2 4Qs from DC05 post: confidence and understanding of DC05, use of framework, use in diagnosing
#Q4 FU barriers
#Q5 FU most valuable
#Q8 FU additional supports
#Q9 is names to give to Theresa for giftcard raffle
`RS-CO DC:0-5 Follow-up` <- `RS-CO DC:0-5 Follow-up` %>%
  select(-c(1,20,22,24,26,28)) %>%
  rename(ShiftPracticeFull = "Q6", BarriersFull = "Q4", MostValuableFull = "Q5", SupportFull = "Q8", ShiftPractice = "Q6 - Topics", Barriers = "Q4 - Topics",
         MostValuable = "Q5 - Topics", Support = "Q8 - Topics")


# RS-CO Tenets Follow-up
colnames(`RS-CO Tenets Follow-up`)
#Q6 shift in practice - self-awareness and interactions
#Q4 FU barriers
#Q5 FU most valuable
#Q8 FU additional supports
#Q9 is names to give to Theresa for giftcard raffle
`RS-CO Tenets Follow-up` <- `RS-CO Tenets Follow-up` %>%
  select(-c(1,12:16,18,20,22,24)) %>%
  rename(ShiftPracticeFull = "Q6", BarriersFull = "Q4", MostValuableFull = "Q5", SupportFull = "Q8", ShiftPractice = "Q6 - Topics", Barriers = "Q4 - Topics",
         MostValuable = "Q5 - Topics", Support = "Q8 - Topics")

# RS-CO Child Welfare Follow-up 
colnames(`RS-CO Child Welfare Follow-up`)
#July 15 2020 - no responses to this survey
#Q6
#Q11
#Q4
#Q5
#Q8
#Q9 is names to give to Theresa for giftcard raffle
`RS-CO Child Welfare Follow-up` <- `RS-CO Child Welfare Follow-up` %>%
  select(-c(1,14:18,20,22,24,26)) %>%
  rename(ShiftPracticeFull = "Q6", BarriersFull = "Q4", MostValuableFull = "Q5", SupportFull = "Q8", ShiftPractice = "Q6 - Topics", Barriers = "Q4 - Topics",
         MostValuable = "Q5 - Topics", Support = "Q8 - Topics")


#Follow-up Qualitative Only
colnames(`RS-CO Foundations Follow-up`)
colnames(`RS-CO DC:0-5 Follow-up`)
colnames(`RS-CO Tenets Follow-up`)
colnames(`RS-CO Child Welfare Follow-up`)


SixMoFUqual <- `RS-CO Foundations Follow-up` %>%
  select(1:7,15:17,21:25) %>%
  rbind(`RS-CO DC:0-5 Follow-up` %>%
          select(1:7,12:14,19:23)) %>%
  rbind(`RS-CO Tenets Follow-up` %>%
          select(1:15)) %>%
  rbind(`RS-CO Child Welfare Follow-up` %>%
          select(1:7,10:17))


# Illuminate CO ECHO Series Eval ------------------------------------------
colnames(`RS-CO Illuminate Colorado ECHO Series Evaluation`)
`RS-CO Illuminate Colorado ECHO Series Evaluation` <- `RS-CO Illuminate Colorado ECHO Series Evaluation` %>%
  rename(Ethnicity = "Q12", EthnicityOther = "Q12_9_TEXT", Gender = "Q13", GenderOther = "Q13_4_TEXT", County = "Q15", Confidence1 = "Q14_1", Confidence2 = "Q14_2")

# "RS-CO Illuminate Colorado ECHO Series Evaluation" **has demographic questions FOR REPORTING DIS ANNUAL REPORT!!! **only 2 evalution questions (confidence in referrals)

