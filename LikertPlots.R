#This file creates all Likert plots for calling in SemiAnnual and Annual report
#The files is necessary because knitting fails at creating plots with missing data (although the plots do create in line)

png(file = "LikertPlots/TrainingEvalSummaryLikert.png", width = 600, height = 450)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$TrainingTopic) %>%
  plot()
dev.off()
?png
png(file = "LikertPlots/LearningObjectivesLikert.png", width = 600, height = 450)
likert(LearningObjectives[,c(6:9)], grouping = LearningObjectives$TrainingTopic) %>%
  plot() 
dev.off()

png(file = "LikertPlots/SatisfactionLikert.png", width = 600, height = 350)
likert(Satisfaction[,c(6:8)], grouping = Satisfaction$TrainingTopic) %>%
  plot()
dev.off()

png(file = "LikertPlots/InstructionLikert.png", width = 600, height = 650)
likert(Instruction[,c(6:12)], grouping = Instruction$TrainingTopic) %>%
  plot()
dev.off()

png(file = "LikertPlots/RelationshipsLikert.png", width = 600, height = 450)
likert(Relationships[,c(6:9)], grouping = Relationships$TrainingTopic) %>%
  plot()
dev.off()


#Check into what might be driving differences in Learning Objectives, Relationships, and Satisfaction
#Could just be due to training topic
##stratifying eval results (each domain) by demographics
###currently missing demographics for a lot of eval results so this might not be valuable to include in FY19 report


###******DO THIS WITH LIKERT PLOTS FOR % BY DEMOGRAPHICS*******

png(file = "LikertPlots/Domains_ReasonAttendLikert.png", width = 600, height = 450)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$`Reason for Attending`) %>%
  plot()
dev.off()

png(file = "LikertPlots/Domains_ExperienceLikert.png", width = 600, height = 400)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$Experience) %>%
  plot()
dev.off()

png(file = "LikertPlots/Domains_RoleLikert.png", width = 700, height = 650)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$Role) %>%
  plot()
dev.off()

png(file = "LikertPlots/Domains_ClinicalNonLikert.png", width = 500, height = 250)
likert(EvalDomainsClinican[,c(6:9)], grouping = EvalDomainsClinican$ClinicalProfessional) %>% plot()
dev.off()

png(file = "LikertPlots/Domains_EthnicityLikert.png", width = 650, height = 550)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$Ethnicity) %>%
  plot()
dev.off()

png(file = "LikertPlots/Domains_EducLikert.png", width = 675, height = 450)
likert(Evaldomains[,c(6:9)], grouping = Evaldomains$Education) %>%
  plot()
dev.off()
