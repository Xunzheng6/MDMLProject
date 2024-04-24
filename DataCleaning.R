library(readr)
library(dplyr)

##loading the raw data
TransPop = read_csv("TransPop.csv")
CISgender = read_csv("CIS.csv")

#selecting variables for TransPop dataset
##did not include survey completed or not as CIS data doesn't have the same field
TransPopNew = TransPop %>% 
  ##selecting the variable we need
  ##recorganizing the data for easier merge with the CIS data
  select(STUDYID,WEIGHT,GRESPONDENT_DATE,RACE,SEXUALID,SEX,AGE,GENDER,HINC,PINC,
         GEDUC1,GEDUCATION,GEMPLOYMENT2010,GURBAN,POVERTYCAT,GANN_INC,GCENREG,
         GCENDIV,GMILESAWAY,
         ##Kessler-6 score
         Q100A,Q100B,Q100C,Q100D,Q100E,Q100F,
         ##satisfaction with life
         Q224,Q225,Q226,Q227,Q228,
         ##social well-being
         Q04,Q05,Q06,Q07,Q08,Q09,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,
         ##identity
         Q22,Q23,Q24,Q25,Q26,Q27,
         ##Everyday discrimination
         Q171A,Q171B,Q171C,Q171D,Q171E,Q171F,Q171G,Q171H,Q171I,
         ##Social Support
         Q192A,Q192B,Q192C,Q192D,Q192E,Q192F,Q192G,Q192H,Q192I,Q192J,Q192K,Q192L,
         ##Future Expectations, Transpop Only
         Q191A,Q191B,Q191C,Q191D,Q191E,Q191F,Q191G,Q191H,Q191I,
         ##Community Connectedness, Transpop Only
         Q51,Q52,Q53,Q54,Q55,
         ##Non-Affirmation of Gender Identity, Transpop Only
         Q45,Q46,Q47,Q48,Q49,Q50) %>% 
  mutate(Data_source = "trans")

CISdata = CISgender %>% 
  ##selecting the variable we need
  ##recorganizing the data for easier merge with the transpop data
  select(STUDYID,WEIGHT,GRESPONDENT_DATE,RACE,SEXUALID,SEX,AGE,GENDER,HINC,PINC,
         GEDUC1,GEDUCATION,GEMPLOYMENT2010,GURBAN,POVERTYCAT,GANN_INC,GCENREG,
         GCENDIV,GMILESAWAY,
         ##Kessler-6 score
         Q61A,Q61B,Q61C,Q61D,Q61E,Q61F,
         ##satisfaction with life
         Q173,Q174,Q175,Q176,Q177,
         ##social well-being
         Q04,Q05,Q06,Q07,Q08,Q09,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,
         ##identity
         Q22,Q23,Q24,Q25,Q26,Q27,
         ##Everyday discrimination
         Q121A,Q121B,Q121C,Q121D,Q121E,Q121F,Q121G,Q121H,Q121I,
         ##Social Support
         Q141A,Q141B,Q141C,Q141D,Q141E,Q141F,Q141G,Q141H,Q141I,Q141J,Q141K,Q141L
         ) %>% 
  mutate(##Future Expectations_cols
         Q191A = "NA", Q191B = "NA", Q191C = "NA", Q191D = "NA", Q191E = "NA",
         Q191F = "NA", Q191G = "NA", Q191H = "NA", Q191I = "NA",
         ##Community Connectedness_cols
         Q51 = "NA", Q52 = "NA", Q53 = "NA", Q54 = "NA", Q55 = "NA",
         ##Non-Affirmation of Gender Identity_col
         Q45 = "NA", Q46 = "NA", Q47 = "NA", Q48 = "NA", Q49 = "NA",
         Q50 = "NA",
         Data_source = "cis") %>% 
  rename(##Renaming Kessler-6 score to match with transpop
         "Q100A" = Q61A, "Q100B" = Q61B, "Q100C" = Q61C, "Q100D" = Q61D, 
         "Q100E" = Q61E, "Q100F" = Q61F,
         ##Renaming satisfaction with life to match with transpop
         "Q224" = Q173, "Q225" = Q174, "Q226" = Q175, "Q227" = Q176, 
         "Q228" = Q177,
         ##Renaming Everyday discrimination to match with transpop
         "Q171A" = Q121A, "Q171B" = Q121B, "Q171C" = Q121C, "Q171D" = Q121D, 
         "Q171E" = Q121E, "Q171F" = Q121F, "Q171G" = Q121G, "Q171H" = Q121H,
         "Q171I" = Q121I,
         ##Renaming Social Support to match with transpop
         "Q192A" = Q141A, "Q192B" = Q141B, "Q192C" = Q141C, "Q192D" = Q141D, 
         "Q192E" = Q141E, "Q192F" = Q141F, "Q192G" = Q141G, "Q192H" = Q141H, 
         "Q192I" = Q141I, "Q192J" = Q141J, "Q192K" = Q141K, "Q192L" = Q141L)

##Combining the data for next step cleaning
Combined = rbind(TransPopNew,CISdata)

##Getting initial list of column name for data dictionary creation
colnames(Combined)

##Examining the data
data.frame(table(Combined$RACE))
data.frame(table(Combined$SEXUALID))
data.frame(table(Combined$SEX))
data.frame(table(Combined$GENDER))

##Column Clean
##Spliting numbers and text content into to column for gender, race, sex, urbanity, etc
##clean the quesion columns to just numbers for easier analysis
## creating sum columns for outcome variables

##Creating our own gender column
Combined = Combined %>% 
  mutate(Gender_2 = paste(Data_source,GENDER))

data.frame(table(Combined$Gender_2))

## Creating Race gender intersection

##creating regional & divisional scores based on the state-level friendiness score
##connect two datasets together

