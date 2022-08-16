library(qualtRics)
library(tidyverse)

#connect to Qualtrics
qualtrics_api_credentials(api_key = "9b9tIGyWZ2PhxDNF97qs9rHC2aIjv2rEJw70SDOw", 
                          base_url = "https://mhcd.co1.qualtrics.com",
                          install = TRUE, overwrite = TRUE)
survey_list <- all_surveys()


#**Specific data pulls. One-offs for program staff
#Week of April 5 COVID training for DDHS
DDHS_COVID <- fetch_survey("SV_cwhdqgWnwLnnfSZ", label = TRUE, force_request = TRUE) %>%
                select(-c(StartDate, EndDate, Status, IPAddress, Progress, `Duration (in seconds)`, Finished, ResponseId, RecipientLastName, RecipientFirstName,
                          RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage))

#export file for Michelle
DDHS_COVID %>%
  select(-Q11, -Q13_1, -Q13_2) %>%
  write.csv("AprilDDHS_COVIDevalResponses.csv")

DDHS_COVID %>%
  write.csv("AprilDDHS_COVIDQA_Participants.csv")