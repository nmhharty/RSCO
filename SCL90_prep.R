#SCL90 ANALYSIS
##whats the length of time between completed screeners?
##Does the percentage of people with clinical-level scores change across timepoints?
##What is the average individual level change in scores?

source("Initial_Clinical_Data_PrepAnalysis.R")

#add FY variable to SCL data
SCL$Date <- as.Date(SCL$Date, "%m/%d/%Y")
SCL$FiscalYear <- with(
  SCL, ifelse(Date %within% FY19, 'FY19',
               ifelse(Date %within% FY20, 'FY20',
                      ifelse(Date %within% FY21, 'FY21',
                             ifelse(Date %within% FY22, 'FY22',
                                    ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
SCL$FiscalYear <- as.factor(SCL$FiscalYear)

#T-Scores are all we have for SCL-90, not categories, so create categories from T scores: 
##Tscore>60 means Clinically Elevated; 40-60 is normal, <40 is Low
colnames(SCL)
SCLlong <- SCL %>%
  gather(SubScale, TScore, 17:28) %>%
  mutate(Category = ifelse(TScore>60,"Clinically Elevated",
                           ifelse(TScore<40,"Low","Normal")),
         SubScale = ifelse(SubScale=="GSI_T","Global Severity Index",
                           ifelse(SubScale=="PSDI_T","Positive Symptom Distress Index",
                                  ifelse(SubScale=="PST_T","Positive Symptom Total",SubScale))))

#baseline and all responses summaries - global indices ---------------------------------------------------------------------

# Global Severity Index (GSI): Designed to measure overall psychological distress.
# Positive Symptom Distress Index (PSDI): Designed to measure the intensity of symptoms.
# Positive Symptom Total (PST): Reports number of self-reported symptoms.
SCLbaselineSummary <- SCLlong %>%
  filter(`TimePoint Label`==1, SubScale %in% c("Global Severity Index", "Positive Symptom Distress Index", "Positive Symptom Total")) %>%
  group_by(SubScale, Category) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(Category, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = Normal + `Clinically Elevated`
      #   + Low
         )

SCLSummary <- SCLlong %>%
  filter(SubScale %in% c("Global Severity Index", "Positive Symptom Distress Index", "Positive Symptom Total")) %>%
  group_by(`TimePoint Label`, SubScale, Category) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(Category, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = Normal + `Clinically Elevated` + Low) %>%
  select(`TimePoint Label`, SubScale, Low, Normal, `Clinically Elevated`, Total)







#April 2020: too few responses to do pre/post comparison of score categories
##Add in LL model later

#Repeated Measures ANOVA for T Scores - **Use lme4 to account for missing data!
library(lme4)
#Cannot run until have more data
# 
# 
# GSI_lme = lmer(TScore ~ `Enrolled Consumer ID`*`TimePoint Label` + (1|`Enrolled Consumer ID`), data=SCLlong %>% filter(SubScale=="GSI_T"))
# anova(GSI_lme)
# 
# PSDI_lme = lmer(TScore ~ `Enrolled Consumer ID`*`TimePoint Label` + (1|`Enrolled Consumer ID`), data=SCLlong %>% filter(SubScale=="PSDI_T"))
# anova(PSDI_lme)
# 
# PST_lme = lmer(TScore ~ `Enrolled Consumer ID`*`TimePoint Label` + (1|`Enrolled Consumer ID`), data=SCLlong %>% filter(SubScale=="PST_T"))
# anova(PST_lme)





