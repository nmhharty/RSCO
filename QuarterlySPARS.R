
##Quarterly SPARS reporting
##LAST UPDATED: 6/24/2020 (FY 2020 Q 3 data)

source("Initial_Training_Data_PrepAnalysis.R")

#number participants per training, Clinical vs Allied
RegistDemo %>%
  filter(EndDate>'2020-03-31'&EndDate<'2020-07-01', Attended=="Yes") %>%
  group_by(FiscalYear, TrainingName, ClinicalProfessional) %>%
  summarise(Count = n_distinct(ParticipantId)) %>%
  print(n = Inf)

#number participants per quarter, Clinical vs Allied
RegistDemo %>%
  filter(Attended=='Yes', EndDate>'2020-03-31'&EndDate<'2020-07-01') %>%
  group_by(FiscalYear, ClinicalProfessional) %>%
  summarise(Count = n_distinct(ParticipantId)) %>%
  print(n = Inf)

#Check against number of evals received
colnames(EvalsWdemos)
EvalsWdemos %>%
  filter(EndDate>'2020-03-31'&EndDate<'2020-07-01') %>%
  group_by(FiscalYear, ClinicalProfessional, TrainingName) %>%
  summarise(Count = n_distinct(EvalPartID)) %>%
  print(n = Inf)

#cleaning data - check if things seem amiss, edit evaluations or other data as needed
#identify which IDs exist in registrations but not evals
# reg <- RegistDemo %>%
#   filter(Attended=='Yes', EndDate>'2019-12-31'&EndDate<'2020-04-01') %>%
#   select(ParticipantId, TrainingName)
# reg$ParticipantId <- as.factor(reg$ParticipantId)
# EvalsWdemos %>%
#   filter(EndDate>'2019-12-31'&EndDate<'2020-04-01') %>%
#   select(EvalPartID, TrainingName) %>%
#   full_join(reg, by = c("EvalPartID"="ParticipantId"))
# RegistDemo %>%
#   filter(TrainingName=="RS-CO Jan 27, 28 DC:0-5 Training") %>%
#   select(Attended, ParticipantId, FirstName, LastName, RegistrationStatus) %>%
#   arrange(ParticipantId)


#BOARD REPORT NUMBERS for THERESA
RegistDemo %>%
  filter(Attended=='Yes', EndDate>'2019-06-30'&EndDate<'2020-07-01') %>%
#  group_by(TrainingTopic) %>%
  summarise(Count=n_distinct(ParticipantId))


#total unduplicated Participants per quarter
RegistDemo %>%
  filter(Attended=='Yes', EndDate>'2020-03-31'&EndDate<'2020-07-01') %>%
  summarise(Count=n_distinct(ParticipantId))
RegistDemo %>%
  filter(Attended=='Yes', EndDate>'2020-03-31'&EndDate<'2020-07-01',
         str_detect(TrainingName, "DHS")) %>%
  group_by(TrainingName) %>%
  summarise(Count=n_distinct(ParticipantId))


#number of CMHCs and other orgs represented
RegistDemo %>%
  filter(Attended=='Yes', EndDate>'2020-03-31'&EndDate<'2020-07-01', ClinicalProfessional=="Yes") %>%
  group_by(TrainingTopic, Organization) %>%
  count(FiscalYear) %>%
  print(n = Inf)


#EI Screenings beyond BHAs
source('../BI01_RODBC.R')
ASQ <- sqlQuery(BI01, 'SELECT * FROM DataEntry.[NH].[ASQ3InformationSummary]')

ASQ %>%
  filter(Tx_Time>1) %>%
  filter(Date_Completed>'2020-03-31')