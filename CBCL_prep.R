#CBCL
##whats the length of time between completed screeners?
##Does the percentage of people with clinical-level scores change across timepoints?
##What is the average individual level change in scores?
#summary of Tscores, percentiles

source("Initial_Clinical_Data_PrepAnalysis.R")

#add FY variable to CBCL data
CBCL$Date <- as.Date(CBCL$Date, "%m/%d/%Y")
CBCL$FiscalYear <- with(
  CBCL, ifelse(Date %within% FY19, 'FY19',
              ifelse(Date %within% FY20, 'FY20',
                     ifelse(Date %within% FY21, 'FY21',
                            ifelse(Date %within% FY22, 'FY22',
                                   ifelse(Date %within% FY23, 'FY23',"N/A")))))
)
CBCL$FiscalYear <- as.factor(CBCL$FiscalYear)

#put TScore and category responses into long form data sets
colnames(CBCL)
CBCLlongTscore <- CBCL %>%
  gather("Domain", "TScore", 7:22) %>%
  mutate(Domain = str_replace_all(Domain, "_", " "))

CBCLlongCat <- CBCL %>%
  gather("Domain", "Category", 23:38)
CBCLlongTscore$TScore <-  as.numeric(CBCLlongTscore$TScore)

#identifying those with followup timepoints by different caregivers
CBCL %>%
  select(`Enrolled Consumer ID`, Relationship, Timepoint) %>%
  group_by(`Enrolled Consumer ID`) %>%
  summarise(MultipleTimepoints = n_distinct(Timepoint), UniqueRespondents = n_distinct(Relationship)) %>%
  filter(UniqueRespondents>1)
#*******CHECK THE RESULTS HERE BEFORE MOVING ON******
#as of 4/13/2020 - one ID with different respondents: 244453
CBCL %>%
  filter(`Enrolled Consumer ID`=="244453")
#**Need to edit in ASEBA so both are "APOC"

#****if there's caregiver mismatch, need to address!******
#****exclude? talk to RS-CO staff?*****


#Baseline Summary ---------------------------------------------------------------------

CBCLcategorySummaryBaseline <- CBCLlongCat %>%
  filter(Timepoint==1) %>%
  group_by(Domain, Category) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(Category,Responses) %>%
  replace(.,is.na(.),0) %>%
  mutate(Total = sum(Borderline + Clinical + Normal)) %>%
  mutate("Percent Normal" = scales::percent(Normal/(sum(Borderline + Clinical + Normal)))) %>%
  mutate("Percent Borderline" = scales::percent(Borderline/(sum(Borderline + Clinical + Normal)))) %>%
  mutate("Percent Clinical" = scales::percent(Clinical/sum((Borderline + Clinical + Normal)))) %>%
  select(Domain, Normal, Borderline, Clinical, `<NA>`, Total, `Percent Normal`, `Percent Borderline`, `Percent Clinical`)


CBCLtrimCatSummaryBaseline <- CBCLcategorySummaryBaseline %>%
  filter(Domain %in% c("Internalizing Problems","Externalizing Problems","Total Problems","Stress Problems"))


#Summary of Categories of Scores at each timepoint ---------------------------------------------------------------------

CBCLcategorySummary <- CBCLlongCat %>%
  group_by(Timepoint, Domain, Category) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(Category, Responses) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = Borderline + Clinical + Normal)  %>%
  select(Domain, Normal, Borderline, Clinical, `<NA>`, Total)


CBCLtrimCatSummary <- CBCLcategorySummary %>%
  filter(Domain %in% c("Internalizing Problems","Externalizing Problems","Total Problems","Stress Problems"))


#visualize raw data ---------------------------------------------------------------------

#histograms and graph summary statistics
CBCLlongCat %>%
  select(`Enrolled Consumer ID`, Timepoint, Date, Domain, Category) %>%
  filter(Domain %in% c("Internalizing Problems","Externalizing Problems","Total Problems","Stress Problems")) %>%
  distinct() %>%
  ggplot() +
  geom_histogram(aes(Category), stat = "count")+facet_grid(Domain~.)
#check distribution of raw scores - if those are more normal, use those. If similar, use percentiles
CBCLlongTscore %>%  
  select(`Enrolled Consumer ID`, Timepoint, Date, Domain, TScore) %>%
  filter(Domain %in% c("Internalizing_Problems_TScore", "Externalizing_Problems_TScore", "Total_Problems_TScore", "Stress_Problems_TScore")) %>%
  distinct() %>%
  ggplot() +
  geom_histogram(aes(TScore), bins = 10)+facet_grid(Domain~.)
##Total Problems are pretty normally distributed, Externalizing and Internalizing have negative skew


#Data Restructuring for Statistical Analyses ---------------------------------------------------------------------

#Wicoxon Signed Rank Test with ordinal categories / ttests for change in each domain
#wilcoxon for orginal categories (clinical, borderline, normal) increase should correspond to an increase in symptoms (so make sure I code this correctly)
###show contingency table for change between categories
#calculate score change using PERCENTILES is easier to interpret but there's issues in using them
#Calculate individual change - use just pre and 6mo for testing
#Assign number to factor levels of score cateogory to look for change in category
#Indiv change in CATEGORY
colnames(CBCL)
CBCL_Cat <- CBCL %>%
  select(`Enrolled Consumer ID`, Timepoint, Date, FiscalYear, "Emotional Reaction", "Anxious/Depressed", "Somatic Complaints", "Withdrawn", "Attention Problems",
         "ADHD Problems", "Aggressive Behaviors", "Anxiety Problems", "Autism Spectrum Problems", "Oppositional Defiance", "Sleep Problems", "Stress Problems", 
         "Depressive Problems", "Internalizing Problems", "Externalizing Problems", "Total Problems") %>%
  filter(Timepoint==1|Timepoint==2) %>%
  mutate_at(.vars = vars("Emotional Reaction", "Anxious/Depressed", "Somatic Complaints", "Withdrawn", "Attention Problems",
                         "ADHD Problems", "Aggressive Behaviors", "Anxiety Problems", "Autism Spectrum Problems", "Oppositional Defiance", "Sleep Problems", "Stress Problems", 
                         "Depressive Problems", "Internalizing Problems", "Externalizing Problems", "Total Problems"),
            .funs = forcats::fct_recode,
            "1" = "Normal", "2" = "Borderline", "3" = "Clinical")

#Convert factors to numeric to get change in category
CBCL_Cat[5:20] <- lapply(CBCL_Cat[5:20], as.character)
CBCL_Cat[5:20] <- lapply(CBCL_Cat[5:20], as.numeric)

#Create separate dataframes for each domain then reconnect them
#exclude defensive responding for PD and Total Stress
CBCL_Cat_EmotReact <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Emotional Reaction`) %>%
  spread(Timepoint, `Emotional Reaction`) %>%
  mutate(EmotReactcatChange = `2`-`1`)
CBCL_Cat_AnxDep <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Anxious/Depressed`) %>%
  spread(Timepoint, `Anxious/Depressed`) %>%
  mutate(AnxDepcatChange = `2`-`1`)
CBCL_Cat_Somatic <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Somatic Complaints`) %>%
  spread(Timepoint, `Somatic Complaints`) %>%
  mutate(SomaticcatChange = `2`-`1`)
CBCL_Cat_Withdrawn <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Withdrawn`) %>%
  spread(Timepoint, `Withdrawn`) %>%
  mutate(WithdrawncatChange = `2`-`1`)
CBCL_Cat_ADHD <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `ADHD Problems`) %>%
  spread(Timepoint, `ADHD Problems`) %>%
  mutate(ADHDcatChange = `2`-`1`)
CBCL_Cat_Autism <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Autism Spectrum Problems`) %>%
  spread(Timepoint, `Autism Spectrum Problems`) %>%
  mutate(AutismcatChange = `2`-`1`)
CBCL_Cat_Somatic <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Somatic Complaints`) %>%
  spread(Timepoint, `Somatic Complaints`) %>%
  mutate(SomaticcatChange = `2`-`1`)
CBCL_Cat_OppDef <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Oppositional Defiance`) %>%
  spread(Timepoint, `Oppositional Defiance`) %>%
  mutate(OppDefcatChange = `2`-`1`)
CBCL_Cat_Sleep <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Sleep Problems`) %>%
  spread(Timepoint, `Sleep Problems`) %>%
  mutate(SleepcatChange = `2`-`1`)
CBCL_Cat_Stress <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Stress Problems`) %>%
  spread(Timepoint, `Stress Problems`) %>%
  mutate(StresscatChange = `2`-`1`)
CBCL_Cat_Dep <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Depressive Problems`) %>%
  spread(Timepoint, `Depressive Problems`) %>%
  mutate(DepcatChange = `2`-`1`)
CBCL_Cat_Intern <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Internalizing Problems`) %>%
  spread(Timepoint, `Internalizing Problems`) %>%
  mutate(InterncatChange = `2`-`1`)
CBCL_Cat_Extern <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Externalizing Problems`) %>%
  spread(Timepoint, `Externalizing Problems`) %>%
  mutate(ExterncatChange = `2`-`1`)
CBCL_Cat_Total <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Total Problems`) %>%
  spread(Timepoint, `Total Problems`) %>%
  mutate(TotalcatChange = `2`-`1`)
CBCL_Cat_Attent <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Attention Problems`) %>%
  spread(Timepoint, `Attention Problems`) %>%
  mutate(AttentcatChange = `2`-`1`)
CBCL_Cat_Aggr <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Aggressive Behaviors`) %>%
  spread(Timepoint, `Aggressive Behaviors`) %>%
  mutate(AggrcatChange = `2`-`1`)
CBCL_Cat_Anx <- CBCL_Cat %>%
  select(`Enrolled Consumer ID`, Timepoint, `Anxiety Problems`) %>%
  spread(Timepoint, `Anxiety Problems`) %>%
  mutate(AnxcatChange = `2`-`1`)


CBCL6moCatChange <- CBCL_Cat_ADHD %>%
  left_join(CBCL_Cat_AnxDep, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Autism, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Dep, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_EmotReact, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Extern, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Intern, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_OppDef, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Sleep, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Somatic, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Stress, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Total, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_Cat_Attent, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_Cat_Aggr, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_Cat_Anx, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_Cat_Withdrawn, by = "Enrolled Consumer ID") %>%  
  select(`Enrolled Consumer ID`, ADHDcatChange, AnxDepcatChange, AutismcatChange, DepcatChange, EmotReactcatChange, ExterncatChange, InterncatChange, 
         OppDefcatChange, SleepcatChange, SomaticcatChange, StresscatChange, TotalcatChange, WithdrawncatChange, AggrcatChange, AnxcatChange, AttentcatChange)

boxplot(ScoreChange~Domain,CBCL6moCatChange %>% gather(Domain, ScoreChange, 2:17))

#use wilcox for score category
CBCL_6moCat_Wilcox <- as.data.frame(CBCL6moCatChange %>%
                                     gather(Domain, Score, -`Enrolled Consumer ID`) %>%
                                     group_by(Domain) %>%
                                     do(test = wilcox.test(x = .$Score, conf.int=TRUE, conf.level=0.95)) %>%
                                     broom::tidy(test)) %>%
  mutate_at(.vars = vars(estimate, statistic, p.value, conf.low, conf.high),
            .funs = signif, 3)

#indiv change in T SCORE
colnames(CBCL)
CBCL_T <- CBCL %>%
  select(`Enrolled Consumer ID`, Timepoint, Date, "Emotionally_Reactive_TScore", "Anxious__Depressed_TScore", "Somatic_Complaints_TScore", "Withdrawn_TScore",
         "Sleep_Problems_TScore", "Attention_Problems_TScore", "Aggressive_Behavior_TScore", "Internalizing_Problems_TScore", "Externalizing_Problems_TScore", 
         "Total_Problems_TScore", "Stress_Problems_TScore", "Depressive_Problems_TScore", "Anxiety_Problems_TScore", "Autism_Spectrum_Problems_TScore", 
         "Attention_Deficit__Hyperactivity_Problems_TScore", "Oppositional_Defiant_Problems_TScore") %>%
  filter(Timepoint==1|Timepoint==2) 


#Create separate dataframes for each domain and TScore then connect
CBCL_T_EmotReact <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Emotionally_Reactive_TScore) %>%
  spread(Timepoint, Emotionally_Reactive_TScore) %>%
  mutate(EmotReactChange = `2`-`1`)
CBCL_T_AnxDep <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Anxious__Depressed_TScore) %>%
  spread(Timepoint, Anxious__Depressed_TScore) %>%
  mutate(AnxDepChange = `2`-`1`)
CBCL_T_Somatic <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Somatic_Complaints_TScore) %>%
  spread(Timepoint, Somatic_Complaints_TScore) %>%
  mutate(SomaticChange = `2`-`1`)
CBCL_T_Withdrawn <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Withdrawn_TScore) %>%
  spread(Timepoint, Withdrawn_TScore) %>%
  mutate(WithdrawnChange = `2`-`1`)
CBCL_T_Sleep <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Sleep_Problems_TScore) %>%
  spread(Timepoint, Sleep_Problems_TScore) %>%
  mutate(SleepChange = `2`-`1`)
CBCL_T_Attent <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Attention_Problems_TScore) %>%
  spread(Timepoint, Attention_Problems_TScore) %>%
  mutate(AttentChange = `2`-`1`)
CBCL_T_Aggr <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Aggressive_Behavior_TScore) %>%
  spread(Timepoint, Aggressive_Behavior_TScore) %>%
  mutate(AggrChange = `2`-`1`)
CBCL_T_Intern <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Internalizing_Problems_TScore) %>%
  spread(Timepoint, Internalizing_Problems_TScore) %>%
  mutate(InternChange = `2`-`1`)
CBCL_T_Extern <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Externalizing_Problems_TScore) %>%
  spread(Timepoint, Externalizing_Problems_TScore) %>%
  mutate(ExternChange = `2`-`1`)
CBCL_T_Total <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Total_Problems_TScore) %>%
  spread(Timepoint, Total_Problems_TScore) %>%
  mutate(TotalChange = `2`-`1`)
CBCL_T_Stress <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Stress_Problems_TScore) %>%
  spread(Timepoint, Stress_Problems_TScore) %>%
  mutate(StressChange = `2`-`1`)
CBCL_T_Depr <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Depressive_Problems_TScore) %>%
  spread(Timepoint, Depressive_Problems_TScore) %>%
  mutate(DeprChange = `2`-`1`)
CBCL_T_Anx <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Anxiety_Problems_TScore) %>%
  spread(Timepoint, Anxiety_Problems_TScore) %>%
  mutate(AnxChange = `2`-`1`)
CBCL_T_Autism <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Autism_Spectrum_Problems_TScore) %>%
  spread(Timepoint, Autism_Spectrum_Problems_TScore) %>%
  mutate(AutismChange = `2`-`1`)
CBCL_T_ADHD <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Attention_Deficit__Hyperactivity_Problems_TScore) %>%
  spread(Timepoint, Attention_Deficit__Hyperactivity_Problems_TScore) %>%
  mutate(ADHDChange = `2`-`1`)
CBCL_T_OppDef <- CBCL_T %>%
  select(`Enrolled Consumer ID`, Timepoint, Oppositional_Defiant_Problems_TScore) %>%
  spread(Timepoint, Oppositional_Defiant_Problems_TScore) %>%
  mutate(OppDefChange = `2`-`1`)


CBCL6moTChange <- CBCL_T_ADHD %>%
  left_join(CBCL_T_Aggr, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Anx, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_AnxDep, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Attent, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Autism, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Depr, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_EmotReact, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Extern, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Intern, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_OppDef, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Sleep, by = "Enrolled Consumer ID") %>% 
  left_join(CBCL_T_Somatic, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_T_Stress, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_T_Total, by = "Enrolled Consumer ID") %>%
  left_join(CBCL_T_Withdrawn, by = "Enrolled Consumer ID") %>%
  select(`Enrolled Consumer ID`, ADHDChange, AggrChange, AnxChange, AnxDepChange, AttentChange, AutismChange, DeprChange, EmotReactChange, ExternChange,
         InternChange, OppDefChange, SleepChange, SomaticChange, StressChange, TotalChange, WithdrawnChange)

boxplot(ScoreChange~Domain,CBCL6moTChange %>% gather(Domain, ScoreChange, 2:17))

#use ttest for TScore
#Only for domains that did not have significant Category change
CBCL_6moTScore_Ttest <- as.data.frame(CBCL6moTChange %>%
                                       gather(Domain, TScore, -`Enrolled Consumer ID`) %>%
                                       group_by(Domain) %>%
                                       do(test = t.test(x = .$TScore, conf.int=TRUE, conf.level=0.95)) %>%
                                       broom::tidy(test)) %>%
  mutate_at(.vars = vars(estimate, statistic, p.value, conf.low, conf.high),
            .funs = signif, 3)
##April 2020: skip Extern, Intern, OppDef, Total because those show signif change in cat

#Agg significant! p=.002913 mean change = -9.75
#Anx approaches significance p=.06597 mean change = -4.25
#AnxDep significant! p=.007872 mean change = -6.583
#Attenr approaches significants p=.05009 mean change = -2.75
#Autism significant! p=.02412 mean change = -5.6667
#Depr significant! p=.001491 mean change = -7.83
#EmotReact significant! p=.009624 mean change = -5.75
#Stress approaches significance p=.05894 mean change = -4.583



# Old Code / Not Used -----------------------------------------------------


# 
# #LogLinear Model
# #following: https://data.library.virginia.edu/an-introduction-to-loglinear-models/
# #create df for frequencies of score categories for each domain in order to create models
# ##make 2 versions - one with all timepoints, one with just baseline and most recent
# #####Timepoints with too few variables, so use just baseline and most recent assessment
# CBCLmaxFUdate <- CBCLlongCat %>%
#   select(`Enrolled Consumer ID`, Timepoint, Date) %>%
#   distinct() %>%
#   group_by(`Enrolled Consumer ID`) %>%
#   filter(Timepoint!=1) %>%
#   summarise("Most Recent CBCL"=max(Date))
# 
# #Internalizing
# CBCL_InternalizingLL <- CBCLlongCat %>%
#   filter(Domain=="Internalizing Problems", !is.na(Category)) %>%
#   select(`Enrolled Consumer ID`, Timepoint, Category) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(Timepoint==1,"Yes","No"), "SixMonth" = ifelse(Timepoint==2,"Yes","No"),
#          # "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          #Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(Category=="Normal","Yes","No"), Borderline = ifelse(Category=="Borderline","Yes","No"), 
#          Clinical = ifelse(Category=="Clinical","Yes","No")
#          ) %>%
#   select(Baseline, `SixMonth`, 
#         # `12 Month`, 
#         # Discharge, 
#          Normal, Borderline, Clinical, `Enrolled Consumer ID`) %>%
#   group_by(Baseline, `SixMonth`,
#            #`12 Month`, 
#            #Discharge, 
#            Normal, Borderline, Clinical) %>%
#   summarise(Freq=n()) 
# 
# ##*****APRIL 2020: only have 2 timepoints of responses, so no need to use the "trimmed" df model*****
# CBCL_InternalizingLLtrim <- CBCLlongCat %>%
#   filter(Domain=="Internalizing Problems", !is.na(Category)) %>%
#   select(`Enrolled Consumer ID`, Timepoint, Category, Date) %>%
#   distinct() %>%
#   #keep baseline for everyone, keep most recent for everyone and assign that a new timepoint --> new variable
#   #Add in zero frequency for missing combinations of timepoint and outcome - should have 6 total rows
#   left_join(CBCLmaxFUdate, by = "Enrolled Consumer ID") %>%
#   mutate(MaxFUkeep = ifelse(Date==`Most Recent CBCL`,"Keep", NA)) %>%
#   filter(Timepoint==1|MaxFUkeep=="Keep") %>%
#   select(-c(`Most Recent CBCL`, MaxFUkeep)) %>%
#   mutate("Baseline" = ifelse(Timepoint==1,"Yes","No"), "FollowUp" = ifelse(Timepoint!=1,"Yes","No"),
#          Normal = ifelse(Category=="Normal","Yes","No"), Borderline = ifelse(Category=="Borderline","Yes","No"), 
#          Clinical = ifelse(Category=="Clinical","Yes","No")) %>%
#   group_by(Baseline, FollowUp, Normal, Borderline, Clinical) %>%
#   summarise(Freq=n())
# 
# #Externalizing
# CBCL_ExternalizingLL <- CBCLlongCat %>%
#   filter(Domain=="Externalizing Problems", !is.na(Category)) %>%
#   select(`Enrolled Consumer ID`, Timepoint, Category) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(Timepoint==1,"Yes","No"), "SixMonth" = ifelse(Timepoint==2,"Yes","No"),
#          # "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          #Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(Category=="Normal","Yes","No"), Borderline = ifelse(Category=="Borderline","Yes","No"), 
#          Clinical = ifelse(Category=="Clinical","Yes","No")
#   ) %>%
#   select(Baseline, `SixMonth`, 
#          # `12 Month`, 
#          # Discharge, 
#          Normal, Borderline, Clinical, `Enrolled Consumer ID`) %>%
#   group_by(Baseline, `SixMonth`,
#            #`12 Month`, 
#            #Discharge, 
#            Normal, Borderline, Clinical) %>%
#   summarise(Freq=n()) 
# 
# #Stress Problems
# CBCL_StressLL <- CBCLlongCat %>%
#   filter(Domain=="Stress Problems", !is.na(Category)) %>%
#   select(`Enrolled Consumer ID`, Timepoint, Category) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(Timepoint==1,"Yes","No"), "SixMonth" = ifelse(Timepoint==2,"Yes","No"),
#          # "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          #Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(Category=="Normal","Yes","No"), Borderline = ifelse(Category=="Borderline","Yes","No"), 
#          Clinical = ifelse(Category=="Clinical","Yes","No")
#   ) %>%
#   select(Baseline, `SixMonth`, 
#          # `12 Month`, 
#          # Discharge, 
#          Normal, Borderline, Clinical, `Enrolled Consumer ID`) %>%
#   group_by(Baseline, `SixMonth`,
#            #`12 Month`, 
#            #Discharge, 
#            Normal, Borderline, Clinical) %>%
#   summarise(Freq=n()) 
# 
# #Total Problems
# CBCL_TotalLL <- CBCLlongCat %>%
#   filter(Domain=="Total Problems", !is.na(Category)) %>%
#   select(`Enrolled Consumer ID`, Timepoint, Category) %>%
#   distinct() %>%
#   mutate("Baseline" = ifelse(Timepoint==1,"Yes","No"), "SixMonth" = ifelse(Timepoint==2,"Yes","No"),
#          # "12 Month" = ifelse(`TimePoint Label`==3,"Yes","No"),
#          #Discharge = ifelse(`TimePoint Label`==99,"Yes","No"),
#          Normal = ifelse(Category=="Normal","Yes","No"), Borderline = ifelse(Category=="Borderline","Yes","No"), 
#          Clinical = ifelse(Category=="Clinical","Yes","No")
#   ) %>%
#   select(Baseline, `SixMonth`, 
#          # `12 Month`, 
#          # Discharge, 
#          Normal, Borderline, Clinical, `Enrolled Consumer ID`) %>%
#   group_by(Baseline, `SixMonth`,
#            #`12 Month`, 
#            #Discharge, 
#            Normal, Borderline, Clinical) %>%
#   summarise(Freq=n()) 
# 
# #MODELS
# #run interaction model because we can clearly see the difference in outcomes, we want to know if timepoint predicts outcome
# #Internalizing Problems
# modIntern_LL <- glm(Freq ~ Baseline + SixMonth + Normal + Borderline + Clinical, 
#                     data = (CBCL_InternalizingLL), family = poisson)
# summary(modIntern_LL)
# modIntern_LL2 <- glm(Freq ~ (Baseline + SixMonth + Normal + Borderline + Clinical)^2, 
#                      data = (CBCL_InternalizingLL), family = poisson)
# summary(modIntern_LL2)
# #compare models - only if df>0 for interaction model
# # anova(modDC_LLtrim, modDC_LLtrim2)
# # pchisq(.90052, df = 2, lower.tail = F)
# 
# pchisq(deviance(modIntern_LL), df = df.residual(modIntern_LL))
# #compare frequencies to fitted model
# cbind(DATA = modIntern_LL$data, FITTED = fitted(modIntern_LL))
# exp(coef(modIntern_LL)["BorderlineYes"]) #odds of having a borderline score in Internalizing Problems at any time point
# #^^odds are less than one, so 1/ODDS --> estimated odds of not having borderline result is 7.6 times the estimated odds for having a borderline result
# 
# 
# #Externalizing Problems
# modExtern_LL <- glm(Freq ~ Baseline + SixMonth + Normal + Borderline + Clinical, 
#                     data = (CBCL_ExternalizingLL), family = poisson)
# summary(modExtern_LL)
# modExtern_LL2 <- glm(Freq ~ (Baseline + SixMonth + Normal + Borderline + Clinical)^2, 
#                      data = (CBCL_ExternalizingLL), family = poisson)
# summary(modIntern_LL2)
# #compare models - only if df>0 for interaction model
# # anova(modDC_LLtrim, modDC_LLtrim2)
# # pchisq(.90052, df = 2, lower.tail = F)
# 
# pchisq(deviance(modExtern_LL), df = df.residual(modExtern_LL))
# #compare frequencies to fitted model
# cbind(DATA = modExtern_LL$data, FITTED = fitted(modExtern_LL))
# #exp(coef(modIntern_LL)["  "]) #odds of having _______
# 
# 
# #Stress Problems
# modStress_LL <- glm(Freq ~ Baseline + SixMonth + Normal + Borderline + Clinical, 
#                     data = (CBCL_StressLL), family = poisson)
# summary(modStress_LL)
# modStress_LL2 <- glm(Freq ~ (Baseline + SixMonth + Normal + Borderline + Clinical)^2, 
#                      data = (CBCL_StressLL), family = poisson)
# summary(modStress_LL2)
# #compare models - only if df>0 for interaction model
# # anova(modDC_LLtrim, modDC_LLtrim2)
# # pchisq(.90052, df = 2, lower.tail = F)
# 
# pchisq(deviance(modStress_LL), df = df.residual(modStress_LL))
# #compare frequencies to fitted model
# cbind(DATA = modStress_LL$data, FITTED = fitted(modStress_LL))
# exp(coef(modStress_LL)["NormalYes"]) #odds of having Normal result in Stress Problems is 2x that of having a different (borderline or clinical) result
# exp(coef(modStress_LL)["BorderlineYes"]) #odds of not having Borderline result in Stress Problems is 2.8x that of having borderline result
# 
# 
# #Total Problems
# modTotal_LL <- glm(Freq ~ Baseline + SixMonth + Normal + Borderline + Clinical, 
#                     data = (CBCL_TotalLL), family = poisson)
# summary(modStress_LL)
# modTotal_LL2 <- glm(Freq ~ (Baseline + SixMonth + Normal + Borderline + Clinical)^2, 
#                      data = (CBCL_TotalLL), family = poisson)
# summary(modTotal_LL2)
# #compare models - only if df>0 for interaction model
# # anova(modDC_LLtrim, modDC_LLtrim2)
# # pchisq(.90052, df = 2, lower.tail = F)
# 
# pchisq(deviance(modTotal_LL), df = df.residual(modTotal_LL))
# #compare frequencies to fitted model
# cbind(DATA = modTotal_LL$data, FITTED = fitted(modTotal_LL))
# 1/exp(coef(modTotal_LL)["NormalYes"]) #odds of not having Normal result in Total Problems is 1.25x that of having a normal result
# 1/exp(coef(modTotal_LL)["BorderlineYes"]) #odds of not having Borderline result in Total Problems is 8x that of having a borderline result
# 
