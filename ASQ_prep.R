#ASQ ANALYSIS
##whats the length of time between completed screeners?
#number of screeners completed at each time point
##How many referrals were made for hearing/vision, PCP, or EI?
##counts of levels in each category grouped by tx time; counts of referral types

source("Initial_Clinical_Data_PrepAnalysis.R")


##Start with raw ASQ data, shift to longform, create new columns, calculate totals
#add FY variable to ASQ data
ASQ$Date_Completed <- as.Date(ASQ$Date_Completed, "%m/%d/%Y")
ASQ$FiscalYear <- with(
  ASQ, ifelse(Date_Completed %within% FY19, 'FY19',
                    ifelse(Date_Completed %within% FY20, 'FY20',
                           ifelse(Date_Completed %within% FY21, 'FY21',
                                  ifelse(Date_Completed %within% FY22, 'FY22',
                                         ifelse(Date_Completed %within% FY23, 'FY23',"N/A")))))
)
ASQ$FiscalYear <- as.factor(ASQ$FiscalYear)

colnames(ASQ)
ASQlongCat <- ASQ %>%
  gather(Skill_Group_Cat, Category, 10:14) %>%
  mutate(Skill_Group_Cat = str_replace_all(Skill_Group_Cat, "_", " "))
  

#Number of referrals, by timepoint
ASQ %>%
  group_by(Tx_Time) %>%
  summarise("EI Referral" = sum(ifelse(Refer_for_EI_Spec_Ed=="Y",1,0)), "Hearing and Vision Referral" = sum(ifelse(Refer_for_Hear_Vision_Behav=="Y",1,0)),
            "Primary Care Referral" = sum(ifelse(Refer_for_Primary_Care_Comm=="Y",1,0)))
#April 2020: only made referrals for EI. 6 referrals made at baseline/intake; 1 referral made at follow-up screening

ASQcategorySummary <- ASQlongCat %>%
  group_by(Tx_Time, Skill_Group_Cat, Category) %>%
  summarise(Responses = n_distinct(`Enrolled Consumer ID`)) %>%
  spread(Category, Responses) %>%
  replace(., is.na(.), 0) %>%
  rename(Borderline = B, Clinical = C, Normal = N) %>%
  mutate(Total = Borderline + Clinical + Normal, "Percent Borderline" = scales::percent(Borderline/Total), "Percent Clinical" = scales::percent(Clinical/Total), 
         "Percent Normal" = scales::percent(Normal/Total)) %>%
  select(Timepoint = Tx_Time, Domain = Skill_Group_Cat, Normal, Borderline, Clinical, Total, "Percent Normal", "Percent Borderline", "Percent Clinical")

