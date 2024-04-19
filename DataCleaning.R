library(readr)
library(dplyr)

##loading the raw data
TransPop = read_csv("TransPop.csv")
CISgender = read_csv("CIS.csv")

#selecting variables for TransPop dataset
##did not include survey completed or not as CIS data doesn't have the same field
##race, do we want to use the survey response and recode our own? 
##or we want to just pre-cleaned in the dataset
TransPopNew = TransPop %>% 
  select(STUDYID,WEIGHT,GRESPONDENT_DATE,RACE,SEXUALID,SEX,AGE,GENDER,HINC,PINC,
         GEDUC1,GEDUCATION,GEMPLOYMENT2010,GURBAN,POVERTYCAT,GANN_INC,GZIPCODE,
         GZIPSTATE,GMILESAWAY) %>% 
  mutate(Survey_type = "TransPop")