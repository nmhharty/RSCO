#run script to get registrations
source("Initial_Training_Data_PrepAnalysis.R")
library(xlsx)

#****CREATE SEPARATE ROWS FOR PRIMARY AND SECONDARY EMAIL ADDRESS FOR EACH PERSON***#


#write table to export for building Qualtrics distribution list
DC05demos %>%
filter(TrainingName=='April 8-9 DC: 0-5 Training') %>%
 write.xlsx("DC05demos9.13.19.xlsx")

DC05demos %>%
  group_by(TrainingName) %>%
  count()
DC05demos %>%
  filter(TrainingName=='April 29-30 DC: 0-5 Training') %>%
  write.xlsx("DC05_April29demos.xlsx")

levels(DC05demos$TrainingName)
DC05demos %>%
  filter(TrainingName=='RS-CO July 8,9 DC 0-5 Training', Attended=='Yes') %>%
  write.xlsx("DC05_July89AspenPointdemos.xlsx")

DC05demos %>%
  filter(TrainingName=='RS-CO Nov 14,15 DC:0-5 Training', Attended=='Yes') %>%
  write.xlsx("DC05_Nov1415demos.xlsx")

DC05demos %>%
  filter(TrainingName=='RS-CO Jan 13,14 2020 DC:0-5 Training', Attended=='Yes') %>%
  write.xlsx("DC05_Jan1314demos.xlsx")

DC05demos %>%
  filter(TrainingName=='RS-CO Jan 27, 28 DC:0-5 Training', Attended=='Yes') %>%
  write.xlsx("DC05_Jan2728demos.xlsx")



#Tenets Participants 6mo follow-up
levels(RegistDemo$TrainingName)
RegistDemo %>%
  filter(TrainingName=='RS-CO Aug 9 Tenets'|TrainingName=='RS-CO August 9 Tenets') %>%
  write.xlsx("TenetsFall2019demos.xlsx")


#Child Welfare IECMH 6mo follow-up
levels(RegistDemo$EventName)
RegistDemo %>%
  filter(TrainingName=='RS-CO April 16 DHS Training'|TrainingName=='RS-CO Mar. 26 DHS Training'|
           TrainingName=='RS-CO April 10 DHS Training Series') %>%
  write.xlsx("ChildWelfareSpring2020demos.xlsx")


