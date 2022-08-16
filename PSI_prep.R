#PSI ANALYSIS
#!!!***can only do pre/post analysis if respondent is the same person!!**
##whats the length of time between completed screeners?
##Do I need to attribute timepoint ==99 to a specific timepoint? if discharge is at 1yr, analyze as such?
##Do people change categorically across timepoints (clinical / baseline / normal)?
###stratify by demographics
##What is the average individual level change in scores?
##how do scores among home-based population compare to scores among outpatient population?
#summary of PD, P-CDI, DC, and Total Stress Score Categories at baseline, eventually split by demographics
getwd()
source("Initial_Clinical_Data_PrepAnalysis.R")

#add FY variable to PSI data
PSIhb$Date <- as.Date(PSIhb$Date, "%m/%d/%Y")
PSIhb$FiscalYear <- with(
  PSIhb, ifelse(Date %within% FY19, 'FY19',
              ifelse(Date %within% FY20, 'FY20',
                     ifelse(Date %within% FY21, 'FY21',
                            ifelse(Date %within% FY22, 'FY22',
                                   ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
PSIhb$FiscalYear <- as.factor(PSIhb$FiscalYear)


#need PSI data in long form - repeat for hb and opt
##recode as factors as needed
colnames(PSIhb)
PSIhbLong <- PSIhb %>%
  gather("PSI Domain Category", "PSI Category", 19:22) %>%
  gather("PSI Domain Percentile", "PSI Percentile", 15:18) %>%
  gather("PSI Domain", "PSI Score", 11:14)
colnames(PSIhbLong)
PSIhbLong[,c(12:14,16)] <- lapply(PSIhbLong[,c(12:14,16)], as.factor)
colnames(PSIopt)
PSIoptLong <- PSIopt %>%
  gather("PSI Domain Category", "PSI  Category", 18:21) %>%
  gather("PSI Domain Percentile", "PSI Percentile", 14:17) %>%
  gather("PSI Domain", "PSI Score", 10:13)
colnames(PSIoptLong)
PSIoptLong[,c(10:12,14)] <- lapply(PSIoptLong[,c(10:12,14)], as.factor)

##**3/25/2020 - additional aggregation, wrangling not done for PSIopt**
#identifying those with followup timepoints by different caregivers
PSIhb %>%
  select(`Child Consumer ID`, `Respondent Relationship`, `TimePoint Label`) %>%
  group_by(`Child Consumer ID`) %>%
  summarise(MultipleTimepoints = n_distinct(`TimePoint Label`), UniqueRespondents = n_distinct(`Respondent Relationship`)) %>%
  filter(UniqueRespondents>1)
#*******CHECK THE RESULTS HERE BEFORE MOVING ON******
#as of 3/2/2020 - all followups by same caregiver!

#****if there's caregiver mismatch, need to address!******
#****exclude? talk to RS-CO staff?*****


#PSI: summary of baseline PD, P-CDI, DC, and Total Stress Score Categories ---------------------------------------------------------------------

##***Need to exclude or separate out defensive responses!
PSIBaselineHB <- PSIhbLong %>%
  filter(`TimePoint Label`==1) %>%
  group_by(`Defensive Responding?`, `PSI Domain Category`, `PSI Category`) %>%
  summarise(Responses = n_distinct(`Child Consumer ID`)) %>%
  spread(`PSI Category`, Responses) %>%
  replace(., is.na(.), 0) %>%
  #will need to decide what to do with the 2 NAs for DC Score Category - impute mean? which mean?
  mutate(Total = sum(Borderline + Clinical + Normal)) %>%
  mutate("Percent Normal" = scales::percent(Normal/(sum(Borderline + Clinical + Normal)))) %>%
  mutate("Percent Borderline" = scales::percent(Borderline/(sum(Borderline + Clinical + Normal)))) %>%
  mutate("Percent Clinical" = scales::percent(Clinical/sum((Borderline + Clinical + Normal)))) %>%
  select(`Defensive Responding?`, `PSI Domain Category`, Normal, Borderline, Clinical, Total, `Percent Normal`, `Percent Borderline`, `Percent Clinical`)

PSIBaselineHB$`PSI Domain Category` <- PSIBaselineHB$`PSI Domain Category` %>%
  recode_factor(`DC Score Category` = "Difficult Child", `P-CDI Score Category` = "Parent-Child Dysfunctional Interaction",
                `PD Score Category` = "Parental Distress", `Total Stress Score Category` = "Total Stress")

#Table of count of responses at each time point in each category
##BUT GROUP AS JUST BASELINE AND ANY FOLLOW-UP? OR EACH TIMEPOINT?
##WHAT DO I DO ABOUT DISCHARGE?
PSIsummary <- PSIhbLong %>%
  group_by(`TimePoint Label`, `Defensive Responding?`, `PSI Domain Category`, `PSI Category`) %>%
  summarise(Responses = n_distinct(`Child Consumer ID`)) %>%
  spread(`PSI Category`, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = Borderline + Clinical + Normal) %>%
  select(`TimePoint Label`, `Defensive Responding?`,`PSI Domain Category`, Normal, Borderline, Clinical, Total)


PSIsummary$`PSI Domain Category` <- PSIsummary$`PSI Domain Category` %>%
  recode_factor(`DC Score Category` = "Difficult Child", `P-CDI Score Category` = "Parent-Child Dysfunctional Interaction",
                `PD Score Category` = "Parental Distress", `Total Stress Score Category` = "Total Stress")

#Summary Statistics ---------------------------------------------------------------------

library(psych)
colnames(PSIhb)
#raw score for each category, by timepoint
PSIhb %>%
  select(c(10:13)) %>%
  describeBy(PSIhb$`TimePoint Label`)

#percentile for each category, by timepoint
PSIhb %>%
  select(c(14:17)) %>%
  describeBy(PSIhb$`TimePoint Label`)

#histograms and graph summary statistics
PSIhbLong %>%
  select(`Child Consumer ID`, `TimePoint Label`, Date, `Defensive Responding?`, `PSI Domain Percentile`, `PSI Percentile`) %>%
  distinct() %>%
  ggplot() +
  geom_histogram(aes(`PSI Percentile`), bins = 10)+facet_grid(`PSI Domain Percentile`~.)
#check distribution of raw scores - if those are more normal, use those. If similar, use percentiles
PSIhbLong %>%
  select(`Child Consumer ID`, `TimePoint Label`, Date, `Defensive Responding?`, `PSI Domain`, `PSI Score`) %>%
  distinct() %>%
  ggplot() +
  geom_histogram(aes(`PSI Score`), bins = 18)+facet_grid(`PSI Domain`~.)
##Scores appear to be more normally distributed than percentiles (4/7/2020)




#Statistical Analysis ---------------------------------------------------------------------

##***APRIL 23 2020**
#different models - Wicoxon Signed Rank Test with ordinal categories / ttests for change in each domain
#wilcoxon for orginal categories (clinical, borderline, normal) increase should correspond to an increase in symptoms (so make sure I code this correctly)
###show contingency table for change between categories
#calculate score change using PERCENTILES is easier to interpret but there's issues in using them
#Calculate individual change - use just pre and 6mo for testing
#Assign number to factor levels of score cateogory to look for change in category
#Indiv change in CATEGORY
colnames(PSIhb)
PSIhbCat <- PSIhb %>%
  select(`Child Consumer ID`, `TimePoint Label`, Date, `Defensive Responding?`, FiscalYear, `PD Score Category`, `DC Score Category`, `P-CDI Score Category`, 
         `Total Stress Score Category`) %>%
  filter(`TimePoint Label`==1|`TimePoint Label`==2) %>%
  mutate_at(.vars = vars(`PD Score Category`, `DC Score Category`, `P-CDI Score Category`, `Total Stress Score Category`),
            .funs = forcats::fct_recode,
            "1" = "Normal", "2" = "Borderline", "3" = "Clinical")

#Convert factors to numeric to get change in category
PSIhbCat[6:9] <- lapply(PSIhbCat[6:9], as.character)
PSIhbCat[6:9] <- lapply(PSIhbCat[6:9], as.numeric)

#Create separate dataframes for each domain then reconnect them
#exclude defensive responding for PD and Total Stress
PSIhbCatPD <- PSIhbCat %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `PD Score Category`) %>%
  spread(`TimePoint Label`, `PD Score Category`) %>%
  mutate(PDcatChange = `2`-`1`)
PSIhbCatDC <- PSIhbCat %>%
  select(`Child Consumer ID`, `TimePoint Label`, `DC Score Category`) %>%
  spread(`TimePoint Label`, `DC Score Category`) %>%
  mutate(DCcatChange = `2`-`1`)
PSIhbCatPCDI <- PSIhbCat %>%
  select(`Child Consumer ID`, `TimePoint Label`, `P-CDI Score Category`) %>%
  spread(`TimePoint Label`, `P-CDI Score Category`) %>%
  mutate(PCDIcatChange = `2`-`1`)
PSIhbCatTS <- PSIhbCat %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `Total Stress Score Category`) %>%
  spread(`TimePoint Label`, `Total Stress Score Category`) %>%
  mutate(TScatChange = `2`-`1`)

PSI6moCatChange <- PSIhbCatDC %>%
  left_join(PSIhbCatPD, by = "Child Consumer ID") %>% 
  left_join(PSIhbCatPCDI, by = "Child Consumer ID") %>% 
  left_join(PSIhbCatTS, by = "Child Consumer ID") %>% 
  select(`Child Consumer ID`, "Parental Distress" = PDcatChange, "Difficult Child" = DCcatChange, "Parent-Child Dysfunctional Interaction" = PCDIcatChange, 
         "Total Stress" = TScatChange)

boxplot(ScoreChange~Domain,PSI6moCatChange %>% gather(Domain, ScoreChange, 2:5))

#following tests are looking at the *change in category* 
PSI_6moCat_Wilcox <- as.data.frame(PSI6moCatChange %>%
                                       gather(Domain, Score, -`Child Consumer ID`) %>%
                                       group_by(Domain) %>%
                                       do(test = wilcox.test(x = .$Score, conf.int=TRUE, conf.level=0.95)) %>%
                                       broom::tidy(test)) %>%
  mutate_at(.vars = vars(estimate, statistic, p.value, conf.low, conf.high),
            .funs = signif, 3)

#PCDI p-value = 0.03494 / there was a shift in category 


#these tests are comparing the timepoints - based upon ?wilcox.test - but what does this actually mean?
wilcox.test(PSIhbCatPCDI$`1`, PSIhbCatPCDI$`2`, paired = TRUE, alternative = "greater", conf.int = TRUE, conf.level = 0.95) # p-value = 0.01747
wilcox.test(PSIhbCatPD$`1`, PSIhbCatPD$`2`, paired = TRUE, alternative = "greater", conf.int = TRUE, conf.level = 0.95)



#indiv change in SCORE / PERCENT
colnames(PSIhb)
PSIhbScore <- PSIhb %>%
  select(`Child Consumer ID`, `TimePoint Label`, Date, `Defensive Responding?`, FiscalYear, `PD Score`, `DC Score`, `P_CDI Score`, `Total Stress Score`, `PD Percent`,
         `P_CDI Percent`, `DC Percent`, `Total Percent`) %>%
  filter(`TimePoint Label`==1|`TimePoint Label`==2) 


#Create separate dataframes for each domain and percent vs score then reconnect them
PSIhbScorePD <- PSIhbScore %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `PD Score`) %>%
  spread(`TimePoint Label`, `PD Score`) %>%
  mutate(PDchange = `2`-`1`)
PSIhbScoreDC <- PSIhbScore %>%
  select(`Child Consumer ID`, `TimePoint Label`, `DC Score`) %>%
  spread(`TimePoint Label`, `DC Score`) %>%
  mutate(DCchange = `2`-`1`)
PSIhbScorePCDI <- PSIhbScore %>%
  select(`Child Consumer ID`, `TimePoint Label`, `P_CDI Score`) %>%
  spread(`TimePoint Label`, `P_CDI Score`) %>%
  mutate(PCDIchange = `2`-`1`)
PSIhbScoreTS <- PSIhbScore %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `Total Stress Score`) %>%
  spread(`TimePoint Label`, `Total Stress Score`) %>%
  mutate(TSchange = `2`-`1`)
PSIhbPercentPD <- PSIhbScore %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `PD Percent`) %>%
  spread(`TimePoint Label`, `PD Percent`) %>%
  mutate(PDchangePct = `2`-`1`)
PSIhbPercentDC <- PSIhbScore %>%
  select(`Child Consumer ID`, `TimePoint Label`, `DC Percent`) %>%
  spread(`TimePoint Label`, `DC Percent`) %>%
  mutate(DCchangePct = `2`-`1`)
PSIhbPercentPCDI <- PSIhbScore %>%
  select(`Child Consumer ID`, `TimePoint Label`, `P_CDI Percent`) %>%
  spread(`TimePoint Label`, `P_CDI Percent`) %>%
  mutate(PCDIchangePct = `2`-`1`)
PSIhbPercentTS <- PSIhbScore %>%
  filter(`Defensive Responding?`!="Y") %>%
  select(`Child Consumer ID`, `TimePoint Label`, `Total Percent`) %>%
  spread(`TimePoint Label`, `Total Percent`) %>%
  mutate(TSchangePct = `2`-`1`)

PSI6moScoreChange <- PSIhbScoreDC %>%
  left_join(PSIhbScorePD, by = "Child Consumer ID") %>%
  left_join(PSIhbScorePCDI, by = "Child Consumer ID") %>%
  left_join(PSIhbScoreTS, by = "Child Consumer ID") %>%
  left_join(PSIhbPercentDC, by = "Child Consumer ID") %>%
  left_join(PSIhbPercentPD, by = "Child Consumer ID") %>%
  left_join(PSIhbPercentPCDI, by = "Child Consumer ID") %>%
  left_join(PSIhbPercentTS, by = "Child Consumer ID") %>%
  select(`Child Consumer ID`, DCchange, PDchange, PCDIchange, TSchange, DCchangePct, PDchangePct, PCDIchangePct, TSchangePct)

boxplot(ScoreChange~Domain,PSI6moScoreChange %>% gather(Domain, ScoreChange, 2:5))

#ttest for normal, numeric, interval data: percentiles are numeric
PSI6moPctChange <- PSI6moScoreChange %>%
  select(-c(DCchange, PDchange, PCDIchange, TSchange)) %>%
  rename("Difficult Child" = DCchangePct, "Parental Distress" = PDchangePct, "Parent-Child Dysfunctional Interaction" = PCDIchangePct, "Total Stress" = TSchangePct)

PSI_6moPCT_TTest <- as.data.frame(PSI6moPctChange %>%
  gather(Domain, Percentile, -`Child Consumer ID`) %>%
  group_by(Domain) %>%
  do(test = t.test(x = .$Percentile)) %>%
  broom::tidy(test)) %>%
  mutate_at(.vars = vars(estimate, statistic, p.value, conf.low, conf.high),
            .funs = signif, 3)


#DC approaches signif; mean change of 14.6%, p=.06749
#PCDI significant - we knew that laready from category change! mean change of -26.3%, p = .0113

#median / Wilcox test for ordinal data: scores are ordinal
PSI6moScoreChange <- PSI6moScoreChange %>%
  select(-c(DCchangePct, PDchangePct, PCDIchangePct, TSchangePct)) %>%
  rename("Difficult Child" = DCchange, "Parental Distress" = PDchange, "Parent-Child Dysfunctional Interaction" = PCDIchange, "Total Stress" = TSchange)


PSI_6moScore_Wilcox <- as.data.frame(PSI6moScoreChange %>%
                                    gather(Domain, Score, -`Child Consumer ID`) %>%
                                    group_by(Domain) %>%
                                    do(test = wilcox.test(x = .$Score, conf.int=TRUE, conf.level=0.95)) %>%
                                    broom::tidy(test)) %>%
  mutate_at(.vars = vars(estimate, statistic, p.value, conf.low, conf.high),
            .funs = signif, 3)

#DC significant! p = .04656; median change of -5.00001
#PCDI (so was score cat change though) significant! p = .01433; median change of -10.99997



#looking at specific scales based upon diagnoses would involve multivariate analysis, so could include all domains together
#Hold-off on this 4/24/2020



#Old Code / Not Used ---------------------------------------------------------------------


# #LogLinear Model
# #following: https://data.library.virginia.edu/an-introduction-to-loglinear-models/
# 
# #log-linear looks at odds ratio of counts for each condition - this will tell me where the difference lies (family function in glm3 package)
# ##this is easier to interpret becasue contingency tables are easy to describe
# #if log-linear doesn't show pre-post a difference, can do percentiles within subjects to check for differences within a category (typically want 6 per group)
# ##if I can see change in score within category I might be able to predict duration of services to see a change
# 
# #create df for frequencies of score categories for each domain in order to create models
# ##NEED TO DEAL WITH DEFENSIVE RESPONDING - don't include defensive repsonding in Parental Distress!
# ##****Need to add in timepoints beyond 1 year
# #Difficult Child
# PSIhb_DC_LL <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="DC Score Category") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "6 Month" = ifelse(`TimePoint Label`==2,"Yes","No"), "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   select(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical, `Child Consumer ID`) %>%
#   group_by(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# #Parent-Child Dysfunctional Interaction
# PSIhb_PCDI_LL <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="P-CDI Score Category") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "6 Month" = ifelse(`TimePoint Label`==2,"Yes","No"), "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   select(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical, `Child Consumer ID`) %>%
#   group_by(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# #Parental Distress
# PSIhb_PD_LL <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="DC Score Category", `Defensive Responding?`!="Y") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "6 Month" = ifelse(`TimePoint Label`==2,"Yes","No"), "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   select(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical, `Child Consumer ID`) %>%
#   group_by(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# #Total Stress
# PSIhb_Stress_LL <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="Total Stress Score Category", `Defensive Responding?`!="Y") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "6 Month" = ifelse(`TimePoint Label`==2,"Yes","No"), "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   select(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical, `Child Consumer ID`) %>%
#   group_by(Baseline, `6 Month`, `12 Month`, Discharge, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# 
# 
# #Fitting Models - Difficult Child
# modDC_LL <- glm(Freq ~ Baseline + `6 Month` + `12 Month` + Discharge + Normal + Borderline + Clinical, 
#                 data = (PSIhb_DC_LL), family = poisson)
# summary(modDC_LL)
# 
# 
# ##check residual deviance. Want it to be close to DF
# #null hypothesis of chisquare is that expected frequencies fit the model
# pchisq(deviance(modDC_LL), df = df.residual(modDC_LL))
# #compare frequencies to fitted model
# cbind(DATA = modDC_LL$data, FITTED = fitted(modDC_LL))
# 
# ##ODDS RATIOS
# exp(coef(modDC_LL)["BaselineYes"])
# 
# 
# 
# #Timepoints with too few variables, so use just baseline and most recent assessment
# PSImaxFUdate <- PSIhbLong %>%
#   select(`Child Consumer ID`, `TimePoint Label`, Date) %>%
#   distinct() %>%
#   group_by(`Child Consumer ID`) %>%
#   filter(`TimePoint Label`!=1) %>%
#   summarise("Most Recent PSI" =max(Date))
# 
# #Difficult Child
# PSIhb_DC_LLtrim <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="DC Score Category") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`, Date) %>%
#   distinct() %>%
#   #keep baseline for everyone, keep most recent for everyone and assign that a new timepoint --> new variable
#   #Add in zero frequency for missing combinations of timepoint and outcome - should have 6 total rows
#   left_join(PSImaxFUdate, by = "Child Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent PSI`,"Keep", NA)) %>%
#   filter(`TimePoint Label`==1|MaxFUkeep=="Keep") %>%
#   select(-c(`Most Recent PSI`, MaxFUkeep)) %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "FollowUp" = ifelse(`TimePoint Label`!=1,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   group_by(Baseline, FollowUp, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# #Parent-Child Dysfunctional Interaction
# PSIhb_PCDI_LLtrim <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="P-CDI Score Category") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`, Date) %>%
#   distinct() %>%
#   left_join(PSImaxFUdate, by = "Child Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent PSI`,"Keep", NA)) %>%
#   filter(`TimePoint Label`==1|MaxFUkeep=="Keep") %>%
#   select(-c(`Most Recent PSI`, MaxFUkeep)) %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "FollowUp" = ifelse(`TimePoint Label`!=1,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   group_by(Baseline, FollowUp, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# 
# #Parental Distress
# PSIhb_PD_LLtrim <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="DC Score Category", `Defensive Responding?`!="Y") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`, Date) %>%
#   distinct() %>%
#   left_join(PSImaxFUdate, by = "Child Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent PSI`,"Keep", NA)) %>%
#   filter(`TimePoint Label`==1|MaxFUkeep=="Keep") %>%
#   select(-c(`Most Recent PSI`, MaxFUkeep)) %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "FollowUp" = ifelse(`TimePoint Label`!=1,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   group_by(Baseline, FollowUp, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# 
# #Total Stress
# #creating rows for missing frequencies
# BorderlineMissing <- data.frame("Baseline" = c("Yes", "No"), "FollowUp" = c("No", "No"), "Normal" = c("No", "No"), "Borderline" = c("Yes", "Yes"), 
#                                 "Clinical" = c("No", "No"), "Freq" = c(0,0))
# 
# PSIhb_Stress_LLtrim <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="Total Stress Score Category", `Defensive Responding?`!="Y") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`, Date) %>%
#   distinct() %>%
#   left_join(PSImaxFUdate, by = "Child Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent PSI`,"Keep", NA)) %>%
#   filter(`TimePoint Label`==1|MaxFUkeep=="Keep") %>%
#   select(-c(`Most Recent PSI`, MaxFUkeep)) %>%
#   mutate("Baseline" = ifelse(`TimePoint Label`==1,"Yes","No"), "FollowUp" = ifelse(`TimePoint Label`!=1,"Yes","No"),
#          Normal = ifelse(`PSI Category`=="Normal","Yes","No"), Borderline = ifelse(`PSI Category`=="Borderline","Yes","No"), 
#          Clinical = ifelse(`PSI Category`=="Clinical","Yes","No")) %>%
#   group_by(Baseline, FollowUp, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n()) %>%
#   bind_rows(BorderlineMissing) %>%
#   arrange(Baseline)
# 
# 
# #MODELS
# #run interaction model because we can clearly see the difference in outcomes, we want to know if timepoint predicts outcome
# #Difficult Child
# modDC_LLtrim <- glm(Freq ~ Baseline + FollowUp + Normal + Borderline + Clinical, 
#                     data = (PSIhb_DC_LLtrim), family = poisson)
# summary(modDC_LLtrim)
# modDC_LLtrim2 <- glm(Freq ~ (Baseline + FollowUp + Normal + Borderline + Clinical)^2, 
#                      data = (PSIhb_DC_LLtrim), family = poisson)
# summary(modDC_LLtrim2)
# #compare models
# anova(modDC_LLtrim, modDC_LLtrim2)
# pchisq(.90052, df = 2, lower.tail = F)
# 
# pchisq(deviance(modDC_LLtrim), df = df.residual(modDC_LLtrim))
# #compare frequencies to fitted model
# cbind(DATA = modDC_LLtrim$data, FITTED = fitted(modDC_LLtrim))
# 
# #Parent Child Interaction Dysfunction
# modPCDI_LLtrim <- glm(Freq ~ Baseline + FollowUp + Normal + Borderline + Clinical, 
#                       data = (PSIhb_PCDI_LLtrim), family = poisson)
# summary(modPCDI_LLtrim)
# modPCDI_LLtrim2 <- glm(Freq ~ (Baseline + FollowUp + Normal + Borderline + Clinical)^2, 
#                        data = (PSIhb_PCDI_LLtrim), family = poisson)
# summary(modPCDI_LLtrim2)
# #compare models
# anova(modPCDI_LLtrim, modPCDI_LLtrim2)
# pchisq(3.9164, df = 2, lower.tail = F)
# 
# pchisq(deviance(modPCDI_LLtrim), df = df.residual(modPCDI_LLtrim))
# #compare frequencies to fitted model
# cbind(DATA = modPCDI_LLtrim$data, FITTED = fitted(modPCDI_LLtrim))
# 
# exp(coef(modPCDI_LLtrim2)["NormalYes"]) #odds of having a normal score in PCDI domain at any time point
# #^^significant coefficient means statistically higher odds of having normal score (19x greater)
# exp(coef(modPCDI_LLtrim2)["BaselineYes:NormalYes"]) #odds of having a normal score at baseline
# #^^because this odds ratio is less than 1 the interpretation is that having a normal score at baseline is less likely than having a normal score at followup
# ##OR = .1644 means at baseline, the estimated odds of having a normal result is .1644 times the estimated odds for follow-up time points having normal results
# ##1/.1644 = 6.08 At followup, the estimated odds of having a normal result are 6x the estimated odds for baseline
# #looking at the frequencies, there's more than expected normal at follow-up and fewer than expected at baseline
# 
# 
# #Parental Distress
# modPD_LLtrim <- glm(Freq ~ Baseline + FollowUp + Normal + Borderline + Clinical, 
#                     data = (PSIhb_PD_LLtrim), family = poisson)
# summary(modPD_LLtrim)
# modPD_LLtrim2 <- glm(Freq ~ (Baseline + FollowUp + Normal + Borderline + Clinical)^2, 
#                      data = (PSIhb_PD_LLtrim), family = poisson)
# summary(modPD_LLtrim2)
# #compare models
# anova(modPD_LLtrim, modPD_LLtrim2)
# pchisq(.6348, df = 2, lower.tail = F)
# pchisq(deviance(modPD_LLtrim), df = df.residual(modPD_LLtrim))
# #compare frequencies to fitted model
# cbind(DATA = modPD_LLtrim$data, FITTED = fitted(modPD_LLtrim))
# 
# #it's only worthwhile to look at OR for outcomes or timepoint/outcome interactions. There's no value in knowing the odds of the timepoint
# exp(coef(modPD_LLtrim)["BorderlineYes"]) #odds of responses being in borderline range
# #^^OR less than one, so 1/OR = 1/.3125 = 3.2 --> the estimated odds of a response being something other than borderline. But this isn't a helpful interpretation
# 
# #Total Stress
# modTS_LLtrim <- glm(Freq ~ Baseline + FollowUp + Normal + Borderline + Clinical, 
#                     data = (PSIhb_Stress_LLtrim), family = poisson)
# summary(modTS_LLtrim)
# modTS_LLtrim2 <- glm(Freq ~ (Baseline + FollowUp + Normal + Borderline + Clinical)^2, 
#                      data = (PSIhb_Stress_LLtrim), family = poisson)
# summary(modPD_LLtrim2)
# #compare models
# anova(modTS_LLtrim, modTS_LLtrim2)
# pchisq(1.2206, df = 1, lower.tail = F)
# pchisq(deviance(modTS_LLtrim), df = df.residual(modTS_LLtrim))
# #compare frequencies to fitted model
# cbind(DATA = modTS_LLtrim$data, FITTED = fitted(modTS_LLtrim))
# 
# exp(coef(modPD_LLtrim)["NormalYes"]) #odds of responses being in normal range
# #^^the estimated odds of having a normal score, regardless of timepoint is 68% greater than having a non-normal score
# 
# 
# #within subjects design - analysis of SCORES/PERCENTILES using glmer: https://stats.stackexchange.com/questions/125093/how-to-use-a-linear-model-with-two-factors-and-repeated-measures
# ##needs longform data with separate rows per subject
# #Repeated Measures ANOVA for T Scores - **Use lme4 to account for missing data!
# 
# library(lme4)
# ##Need to remove defensive responding from Parental Distress and Total Stress
# ##Why do I use (1|Subject) vs (Timepoint|Subject) --> subjects nested within timepoint or not
# PD_lme = lmer(`PSI Score` ~ `TimePoint Label` + (1|`Child Consumer ID`), 
#               data=PSIhbLong %>% filter(`Defensive Responding?`!="Y",`PSI Domain`=="PD Score")) 
# PD_lme %>%
#   summary()
# anova(PD_lme)
# 
# lmer(`PSI Score` ~ ( `TimePoint Label` | `Child Consumer ID`) + (1 | `Child Consumer ID`),
#      data=PSIhbLong %>% filter(`Defensive Responding?`!="Y",`PSI Domain`=="PD Score")) %>%
#   summary()
# 
# #regular non-nested models
# lm(`PSI Score` ~ `TimePoint Label` + `Child Consumer ID`, data=PSIhbLong %>% filter(`Defensive Responding?`!="Y",`PSI Domain`=="PD Score")) %>%
#   summary()
# lm(`PSI Score` ~ `TimePoint Label`*`Child Consumer ID`, data=PSIhbLong %>% filter(`Defensive Responding?`!="Y",`PSI Domain`=="PD Score")) %>%
#   summary()
# #model with just baseline + follow-up (i.e. most recent assessment)
# PSI_DC_trim <- PSIhbLong %>%
#   filter(`PSI Domain Category`=="DC Score Category", `Defensive Responding?`!="Y") %>%
#   select(`Child Consumer ID`, `TimePoint Label`, `PSI Category`, Date, `PSI Score`) %>%
#   distinct() %>%
#   left_join(PSImaxFUdate, by = "Child Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent PSI`,"Keep", NA)) %>%
#   filter(`TimePoint Label`==1|MaxFUkeep=="Keep") %>%
#   mutate(Timepoint = ifelse(`TimePoint Label`==1,"Baseline","FollowUp")) %>%
#   select(-c(`Most Recent PSI`, MaxFUkeep, `TimePoint Label`))
# 
# lm(`PSI Score` ~ `Timepoint`*`Child Consumer ID`, data = PSI_DC_trim) %>%
#   summary()
# 
# #How do I interpret these resutls?!
# #could do a regular regression, but maybe it's that I just don't have power.
# lm(`PSI Score` ~ `TimePoint Label`, data = PSIhbLong %>% filter(`Defensive Responding?`!="Y",`PSI Domain`=="PD Score"))
# ##Scatter Plots: score timepoint 1 vs score at timepoint 2 (use most recent score, possibly 6mo to baseline)
# 



###Similar to a qq plot or homoscedasticity for linear model
# pchisq(deviance(modDC_LL2), df = df.residual(modDC_LL), lower.tail = F)
# cbind(DATA = modDC_LL2$data, FITTED = fitted(modDC_LL2))
##What is my interpretation of the results? The only significant result is for Nomal=Yes. Does that mean that I see more people in the normal range than other ranges, 
##regardless of timepoint? seems like yes?

#ODDS RATIO FOR NORMAL:
# exp(coef(modDC_LL2)["NormalYes"])
#INTERPRETATION: someone is 2x as likely to be in the normal range as another range, regardless of timepoint

#But, really, I want to know if people are likely to change categories over time, which would be an interaction
# modDC_LL2_int <- glm(Freq ~ (Baseline + `6 Month` + Normal + Borderline + Clinical)^2, 
#                      data = (PSIhb_DC_LL %>%
#                                filter(Freq>1)), family = poisson)
# summary(modDC_LL2_int)
##I don't have enough data yet to say anything here
##So can I say what the results at 6months are, or I just give the baseline results and say that althought there's ___ follow-up responses,
##there's insufficient data to make pre/post comparisons. 



#Individual level analysis - Only needed if no change in category
# PSIprepostIndivPerc <- PSIlong %>%
#   mutate(Category_Time = paste0(`Domain Percentile`,'_',`TimePoint Label`)) %>%
#   group_by(`Child Consumer ID`, `Domain Percentile`, `TimePoint Label`, Percentile) %>%
#   summarise(Responses = n_distinct(`Child Consumer ID`)) %>%
#   spread(`TimePoint Label`, Percentile) %>%
#   rename("T1" = "1", "T2" = "2", "T3" = "3", "Discharge" = "99") %>%
#   mutate("T2_T1 Change" = ifelse(is.na(T2),NA,T2-T1), "T3_T2 Change" = ifelse(is.na(T3),NA,T3-T2), 
#          "T3_T1 Change" = ifelse(is.na(T3),NA,T3-T1), "Discharge_Intake Change" = ifelse(is.na(Discharge),NA,Discharge-T1))

#Parental Distress, Parent–Child Dysfunctional Interaction and Difficult Child. Child and Parent domains combine to form Total Stress Scale




