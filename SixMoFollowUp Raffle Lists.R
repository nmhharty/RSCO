#Script for pulling all Right Start for Colorado 6 month follow-up surveys in Qualtrics to share respondent info with Theresa for raffling gift card
#Initial creation: 5/22/2020
#Updated: 5/22/2020


library(qualtRics)
library(tidyverse)

#connect to qualtrics
source('../QualtricsConnection_nharty.R')
readRenviron("~/.Renviron")

Qsurveys <- all_surveys() %>%
  filter(str_detect(name,"RS-CO"))

#load in all RSCO surveys - not all are identical
#see notes about eval differences in OneNote: Nicole's personal OneNote > RS-CO > Data Collection, Reporting, and Analysis > *Workforce Data Nuances
Qsurveys$name

SurveyIDlist <- (Qsurveys$id)
for(i in SurveyIDlist) {
  surveyname <- Qsurveys %>%
    filter(id==i) %>%
    select(name) %>%
    pull()
  df <- fetch_survey(i, start_date = "2019-01-01",
                     label = TRUE, force_request = TRUE) %>%
    #remove unnecessary columns from each survey df (in bulk, then edit for each survey specifically)
    filter(DistributionChannel!="preview") %>%
    select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage)) %>%
    mutate(EvalName=(Qsurveys %>%
                       filter(id==i) %>%
                       select(name) %>%
                       pull()))
  assign(surveyname,df)
}

#Six Month Follow-Up Survey Names:
# "RS-CO DC:0-5 Follow-up"
# "RS-CO Tenets Follow-up"
# "RS-CO Foundations Follow-up"
# "RS-CO Child Welfare Follow-up"

#Check that it's Q9 for all follow-up surveys columns have the names for each survey
colnames(`RS-CO DC:0-5 Follow-up`) 
colnames(`RS-CO Tenets Follow-up`) 
colnames(`RS-CO Foundations Follow-up`) 
colnames(`RS-CO Child Welfare Follow-up`) 

`RS-CO DC:0-5 Follow-up` %>%
  select(RecordedDate, EvalName, Q9_1, Q9_2, Q9_3) %>%
  rbind(`RS-CO Tenets Follow-up` %>%
          select(RecordedDate, EvalName, Q9_1, Q9_2, Q9_3)) %>%
  rbind(`RS-CO Foundations Follow-up` %>%
          select(RecordedDate, EvalName, Q9_1, Q9_2, Q9_3))  %>%
  rbind(`RS-CO Child Welfare Follow-up` %>%
          select(RecordedDate, EvalName, Q9_1, Q9_2, Q9_3)) %>%
  write_csv("6moFollowUpSurveyCompletions.csv")

