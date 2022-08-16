#Script for dealing with all CoAIMH Foundations training pre, post, and follow-up data
#Initial code development: May 2020

#NOTES:
##Prior to April 2020, all pre, post, and follow-up data for Colorado Foundations trainings were completed in Survey Monkey using the CoAIMH account
##Starting in spring 2020, CoAIMH stopped supporting the ongoing evaluation of this training
##RSCO decided to shorten the CoAIMH pre, post, follow-up assessment and moved all data collection to Qualtrics
##This script works with the Survey Monkey Data and then adds in the Qualtrics, RSCO-specific data

#UPLOAD CoAIMH SURVEY MONKEY FOUNDATIONS EVALS for pre/post/follow-up analysis
#Before loading into R, clean up column names in xlsx files because have 2 rows of headers
##need to join with Qualtrics surveys
library(readxl)
CoAIMHpre <- read_excel("/home/Public/Nicole/CoAIMH_SMevals/Colorado Foundations Pre-Survey 2019.xlsx") %>%
  rbind(read_excel("/home/Public/Nicole/CoAIMH_SMevals/Colorado Foundations Pre-Survey Right Start 2020.xlsx"))
SMpost <- read_excel("/home/Public/Nicole/CoAIMH_SMevals/ARCHIVE Colorado Foundations Post-Survey 2019 Right Start for Colorado.xlsx")
SMfu <- read_excel("/home/Public/Nicole/CoAIMH_SMevals/Colorado Foundations Follow-Up Survey 2019 Right Start for Colorado.xlsx")

#Add participant ID to each survey
SMpostIDs <- SMpost %>%
  select(ParticipantID, BirthMonth, BirthDay, BirthYear)
CoAIMHpre <- CoAIMHpre %>%
  left_join(SMpostIDs, by = c("BirthMonth", "BirthDay", "BirthYear"))

SMfu <- SMfu %>%
  left_join(SMpostIDs, by = c("BirthMonth", "BirthDay", "BirthYear"))


#separate out RSCO standard post-training eval questions from SMpost to join to evals df later
colnames(SMpost)
CoAIMHpostEval <- SMpost %>%
  select(6,10:28)
#POST COAIMH SURVEY: likert are 4 point scale, so need to recode responses to be on a 5pt scale!!
colnames(CoAIMHpostEval)
CoAIMHpostEval[,2:20] <- lapply(CoAIMHpostEval[,2:20], as.factor)
levels(CoAIMHpostEval$LearningObj4)
#Need 5 total levels and to assign correct response to each level
##First, fct_unify to make all questions have 4 levels (adds in the missing levels without responses)
CoAIMHpostEval[,2:20] <- CoAIMHpostEval[,2:20] %>%
  fct_unify()

##Check total levels and add any missing PLUS add in a 5th using fct_expand
#no one responded with "Strongly Agree" so add that AND "Neutral"
CoAIMHpostEval[,2:20] <- lapply(CoAIMHpostEval[,2:20], fct_expand, c("Strongly Disagree", "Neutral"))
levels(CoAIMHpostEval$Satisfaction1)
##Reorder the levels to put the neutral response as level = 3 and order others correctly
CoAIMHpostEval[,2:20] <- lapply(CoAIMHpostEval[,2:20], fct_relevel, c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
#convert levels to numbers to match Evals df
CoAIMHpostEval <- CoAIMHpostEval %>%
  mutate_at(vars(2:20),
            .funs = forcats::fct_recode,
            "1"="Strongly Disagree", "2"="Disagree", "3"="Neutral", "4"="Agree", "5"="Strongly Agree")
#Add columns for TrainingName, TrainingTopic, StartDate, EndDate
CoAIMHpostEval <- CoAIMHpostEval %>%
  mutate(TrainingName = "RS-CO Sept 4 Foundations", TrainingTopic = "Foundations", StartDate = as.POSIXct("2019-09-04"), EndDate = as.POSIXct("2019-11-06"))
#save this cleaned up version of CoAIMH Foundations post evals to join to Evals df
save(CoAIMHpostEval,file="CoAIMHpostEval.Rdata")


CoAIMHpre <- CoAIMHpre %>%
  mutate(Timepoint=1)
SMpost <- SMpost %>%
  mutate(Timepoint=2)
SMfu <- SMfu %>%
  mutate(Timepoint=3) %>%
  select(-35,-36)

CoAIMHpreIDonly <- CoAIMHpre %>%
  filter(!is.na(ParticipantID))
SMfuIDonly <- SMfu %>%
  filter(!is.na(ParticipantID))

colnames(CoAIMHpre)
colnames(SMpost)
colnames(SMfu)
colnames(SMpostIDs)
colnames(SMfuIDonly)


CoAIMHdemos <- CoAIMHpre %>%
  select(1:27, 53)

#build multiple df with all pre/post/follow-up data for Foundations; save as Rdata files to load for analysis
#First has what's common across all three time points (IMH Attitudes 1-4)
CoAIMHprePostFU <- CoAIMHpre %>%
  #28-31 are IMH Attitudes, 32-52 are Knowledge,  
  select(4,53,54,28:31) %>%
  bind_rows((SMpost %>%
               select(4,6,78,48:51))) %>%
  bind_rows((SMfu %>%
           select(4,35,36,9:12)))

CoAIMHprePostFU_IDonly <- CoAIMHpreIDonly %>%
  #28-31 are IMH Attitudes, 32-52 are Knowledge,  
  select(4,53,54,28:31) %>%
  bind_rows((SMpost %>%
               select(4,6,78,48:51))) %>%
  bind_rows((SMfuIDonly %>%
               select(4,35,36,9:12)))

#add TrainingName, TrainingTopic, StartDate, EndDate columns
# CoAIMHprePostFU %>%
#   mutate(TrainingTopic = "Foundations", 
#          ifelse(TrainingName = "RS-CO Sept 4 Foundations", StartDate = as.character.POSIXt("2019-09-04"), EndDate = as.character.POSIXt("2019-11-06")))

save(CoAIMHprePostFU,file="CoAIMHprePostFU.Rdata")
save(CoAIMHprePostFU_IDonly,file="CoAIMHprePostFU_IDonly.Rdata")

#Second for pre and post (IMH Attitudes 1-4, Knowledge)
CoAIMHprePost <- CoAIMHpre %>%
  select(53,54,28:52) %>%
  bind_rows(SMpost %>%
           select(6,78,48:51,57:77))  
CoAIMHprePost_IDonly <- CoAIMHpreIDonly %>%
  select(53,54,28:52) %>%
  bind_rows(SMpost %>%
              select(6,78,48:51,57:77))  
#Add columns for TrainingName, TrainingTopic, StartDate, EndDate

save(CoAIMHprePost,file="CoAIMHprePost.Rdata")
save(CoAIMHprePost_IDonly,file="CoAIMHprePost_IDonly.Rdata")

#Third for post and followup (IMH Attitudes 1-6, Confidence, Most Helpful Module)
CoAIMHpostFU <- SMpost %>%
  select(6,78,48:53,39:47,29:36) %>%
  bind_rows(SMfu %>%
           select(35,36,9:31))
CoAIMHpostFU_IDonly <- SMpost %>%
  select(6,78,48:53,39:47,29:36) %>%
  bind_rows(SMfuIDonly %>%
              select(35,36,9:31))
#Add columns for TrainingName, TrainingTopic, StartDate, EndDate

save(CoAIMHpostFU,file="CoAIMHpostFU.Rdata")
save(CoAIMHpostFU_IDonly,file="CoAIMHpostFU_IDonly.Rdata")


#CoAIMH Infant Mental Health Attitudes, Knowledge, and Confidence Questions
IMH_Attitudes <-  c("I believe it is important to train early childhood professionals about infant and early childhood mental health.", 
                    "I am confident in my infant and early childhood mental health skills.",
                    "I am committed to learning about infant and early childhood mental health.",
                    "I understand the value of continuing education around infant and early childhood mental health concepts.",
                    "I have used the knowledge and skills I have gained from this training in my work.",
                    "f. I have applied what I learned from the training at work.")
PrePostKnowledgeQuestions <- c("What factor strengthens the likelihood of positive outcomes for children and buffers the effects of toxic stress?",
                               "Sam is an I-ECMH professional who provides supports to families in an empathetic and non-judgmental way. What type of support is Sam providing families by doing this?",
                               "Inappropriate responses to situations can be a possible sign of I-ECMH concerns for what developmental stage?",	
                               "When observing a caregiver’s interactions with a child, Kris tries to determine whether the interactions are consistent, attuned, responsive, and engaged. What factor that affects child development is Kris observing in these interactions?",
                               "What developmental stage is typically characterized by concrete and literal thinking, the ability to follow simple rules, increased exploration and curiosity, limited ability to take others perspectives, and increased self-identity?",
                               "Setting clear and positive rules for children which are agreed upon by caregivers reflects which concept?",
                               "What is an example of a long-term impact for children who form healthy attachments with one or more adults?",
                               "By what age is a child’s brain 90% the weight and volume of an adult’s brain?",
                               "Consistent “serve and return” interactions between a caregiver and child is essential for development because it leads to which of the following outcomes?",
                               "Individuals who have had four or more adverse childhood experiences are at most risk for which of the following health problems?",
                               "Individuals with higher economic status are ________ to experience one or more adverse childhood experiences.",
                               "Alex has been homeless with his family for several years. Due to these circumstances, Alex’s caregivers are less supportive and less responsive to his needs. Alex is most likely experiencing which of the following type of stress?",
                               "One foundation for building strong relationships is the capacity to be in touch with one’s own emotions, read feelings of others, and form emotional connections. This is an example of _______________.",
                               "Ari is an I-ECMH professional who makes sure to take “time outs” regularly when she is feeling overwhelmed. This is an example of which type of protective factor?",
                               "Reframing thoughts and statements is an example of what type of strategy when coping with stress?",
                               "Ignacia is an I-ECMH professional who is having a stressful day. Instead of coping with the current stress level, Ignacia transfers her stress to the family she is currently working with. This is an example of _________________.",
                               "Which of the following should be done during reflection?",
                               "Rules for typical and socially acceptable behaviors within a group which may vary across cultures are known as ______________.",
                               "Maria works hard to recognize beliefs and values that affect her work as an I-ECMH professional and she regularly reflects on how culture affects her work. Where would Mary most likely fall on the cultural continuum?",
                               "When working with families, Jan provides caregivers with information about social-emotional development and offers a free social-emotional evidence-based course. This is an example of which type of I-ECMH support?",
                               "Which of the following messages reflects the idea that resilience is a skill?"
)
ConfidenceQs <- c("I can identify needs which warrant referrals for additional support, assessment, or intervention.",
                  "I can provide referrals for additional support, assessment, or intervention.",
                  "I can engage in self-reflection and collaboration with other members of this learning community.",	
                  "I can recognize that caregivers bring a unique culture lens to each of their relationships.",
                  "I can identify signs indicating possible I-ECMH issues.",
                  "I can recognize factors that build strong caregiver relationships.",
                  "I can respond more sensitively to challenging behaviors, given my increased appreciation of early adversity / toxic stress and brain development.",
                  "I can manage stress and apply self-care strategies.",
                  "I can identify opportunities and resources to integrate reflective practices into my work."
)



