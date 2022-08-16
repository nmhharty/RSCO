#Edinburgh
##whats the length of time between completed screeners?
##Does the percentage of people with clinical-level scores change across timepoints?
##What is the average individual level change in scores?
#summary of PD, P-CDI, DC, and Total Stress Score Categories, eventually split by demographics and look for changes pre/post

source("Initial_Clinical_Data_PrepAnalysis.R")

#add FY variable to Edinburgh data
Edin$Date <- as.Date(Edin$Date, "%m/%d/%Y")
Edin$FiscalYear <- with(
  Edin, ifelse(Date %within% FY19, 'FY19',
               ifelse(Date %within% FY20, 'FY20',
                      ifelse(Date %within% FY21, 'FY21',
                             ifelse(Date %within% FY22, 'FY22',
                                    ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
Edin$FiscalYear <- as.factor(Edin$FiscalYear)

#Baseline and Overall Summaries ---------------------------------------------------------------------

EdinSummaryBaseline <- Edin %>%
  filter(`TimePoint Label`==1) %>%
  group_by(ScoreCat) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(ScoreCat, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = sum(Normal+`Possible Depression`), "Percent Normal" = scales::percent(Normal/(sum(Normal+`Possible Depression`))), 
         "Percent Possible Depression" = scales::percent(`Possible Depression`/(sum(Normal+`Possible Depression`))))

Edinsummary <- Edin %>%
  group_by(`TimePoint Label`, ScoreCat) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(ScoreCat, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = sum(Normal+`Possible Depression`), "Percent Normal" = scales::percent(Normal/(sum(Normal+`Possible Depression`))), 
         "Percent Possible Depression" = scales::percent(`Possible Depression`/(sum(Normal+`Possible Depression`))))


