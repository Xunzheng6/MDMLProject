library(readr)
library(dplyr)
library(stringr) 
library(tidyr)
library(lme4)

##loading the raw data
TransPop = read_csv("TransPop.csv")
CISgender = read_csv("CIS.csv")

#selecting variables for TransPop dataset
##did not include survey completed or not as CIS data doesn't have the same field
TransPopNew = TransPop %>% 
  ##selecting the variable we need
  ##recorganizing the data for easier merge with the CIS data
  select(STUDYID,WEIGHT,GRESPONDENT_DATE,RACE,RACE_RECODE,RACE_RECODE_CAT5,SEXUALID,SEX,AGE,GENDER,HINC,PINC,
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
  select(STUDYID,WEIGHT,GRESPONDENT_DATE,RACE,RACE_RECODE,RACE_RECODE_CAT5,SEXUALID,SEX,AGE,GENDER,HINC,PINC,
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

##Checking & dropping NA (Original N=1436)
## 48 dropped due to incomplete Kessler Q, 1 from trans, 47 from cis
sum(is.na(Combined$Q100A))
NAcheck = filter(Combined,is.na(Q100A))
Combined = Combined %>% 
  drop_na(Q100A,Q100B,Q100C,Q100D,Q100E,Q100F)
table(Combined$Data_source)

##Getting initial list of column name for data dictionary creation
colnames(Combined)

##Examining the data
data.frame(table(Combined$RACE))
data.frame(table(Combined$SEXUALID))
data.frame(table(Combined$SEX))
data.frame(table(Combined$GENDER))

##gender column cleaning
Combined = Combined %>% 
  mutate(Gender_text = str_extract(GENDER,"\\d"),
         Gender_text = ifelse(Gender_text==3,1,Gender_text),
         Gender_text = ifelse(Gender_text==4,2,Gender_text),
         Gender_text = ifelse(Gender_text==1,"Man",Gender_text),
         Gender_text = ifelse(Gender_text==2,"Woman",Gender_text),
         Gender_text = ifelse(Gender_text==5,"GNB",Gender_text),
         Gender_text = paste0(Data_source,Gender_text),
         Gender_num = ifelse(Gender_text=="cisMan",1,Gender_text),
         Gender_num = ifelse(Gender_text=="cisWoman",2,Gender_num),
         Gender_num = ifelse(Gender_text=="transMan",3,Gender_num),
         Gender_num = ifelse(Gender_text=="transWoman",4,Gender_num),
         Gender_num = ifelse(Gender_text=="transGNB",5,Gender_num))

##checking gender
table(Combined$Gender_text)
table(Combined$Gender_num)

## splitting numbers category from text 
Combined = Combined %>% 
  mutate(SEXUALID_num = parse_number(SEXUALID),
         HINC_num = parse_number(HINC),
         PINC_num = parse_number(PINC),
         EDU_num = parse_number(GEDUCATION),
         Employ_num = parse_number(GEMPLOYMENT2010),
         INC_Bracket_num = parse_number(GANN_INC),
         Urban_num = parse_number(GURBAN))

## clean the question columns to just numbers for easier analysis
Combined = Combined %>% 
  mutate(##Kessler-6 score
    Q100A = parse_number(Q100A), Q100B = parse_number(Q100B), Q100C = parse_number(Q100C),
    Q100D = parse_number(Q100D), Q100E = parse_number(Q100E), Q100F = parse_number(Q100F),
    Q100A = case_when(Q100A==5 ~ 1, Q100A==4 ~ 2, Q100A==2 ~ 4, Q100A==1 ~ 5, Q100A==3 ~ 3),
    Q100B = case_when(Q100B==5 ~ 1, Q100B==4 ~ 2, Q100B==2 ~ 4, Q100B==1 ~ 5, Q100B==3 ~ 3),
    Q100C = case_when(Q100C==5 ~ 1, Q100C==4 ~ 2, Q100C==2 ~ 4, Q100C==1 ~ 5, Q100C==3 ~ 3),
    Q100D = case_when(Q100D==5 ~ 1, Q100D==4 ~ 2, Q100D==2 ~ 4, Q100D==1 ~ 5, Q100D==3 ~ 3),
    Q100E = case_when(Q100E==5 ~ 1, Q100E==4 ~ 2, Q100E==2 ~ 4, Q100E==1 ~ 5, Q100E==3 ~ 3),
    Q100F = case_when(Q100F==5 ~ 1, Q100F==4 ~ 2, Q100F==2 ~ 4, Q100F==1 ~ 5, Q100F==3 ~ 3),
    ##satisfaction with life
    Q224 = parse_number(Q224), Q225 = parse_number(Q225), Q226 = parse_number(Q226),
    Q227 = parse_number(Q227), Q228 = parse_number(Q228),
    ##social well-being
    Q04 = parse_number(Q04), Q05 = parse_number(Q05), Q06 = parse_number(Q06), 
    Q07 = parse_number(Q07), Q08 = parse_number(Q08), Q09 = parse_number(Q09), 
    Q10 = parse_number(Q10), Q11 = parse_number(Q11), Q12 = parse_number(Q12), 
    Q13 = parse_number(Q13), Q14 = parse_number(Q14), Q15 = parse_number(Q15),
    Q16 = parse_number(Q16), Q17 = parse_number(Q17), Q18 = parse_number(Q18),
    Q04 = case_when(Q04==7 ~ 1, Q04==6 ~ 2, Q04==5 ~ 3, Q04==4 ~ 4, Q04==3 ~ 5,
                    Q04==2 ~ 6, Q04==1 ~ 7),
    Q08 = case_when(Q08==7 ~ 1, Q08==6 ~ 2, Q08==5 ~ 3, Q08==4 ~ 4, Q08==3 ~ 5,
                    Q08==2 ~ 6, Q08==1 ~ 7),
    Q11 = case_when(Q11==7 ~ 1, Q11==6 ~ 2, Q11==5 ~ 3, Q11==4 ~ 4, Q11==3 ~ 5,
                    Q11==2 ~ 6, Q11==1 ~ 7),
    Q12 = case_when(Q12==7 ~ 1, Q12==6 ~ 2, Q12==5 ~ 3, Q12==4 ~ 4, Q12==3 ~ 5,
                    Q12==2 ~ 6, Q12==1 ~ 7),
    Q14 = case_when(Q14==7 ~ 1, Q14==6 ~ 2, Q14==5 ~ 3, Q14==4 ~ 4, Q14==3 ~ 5,
                    Q14==2 ~ 6, Q14==1 ~ 7),
    Q15 = case_when(Q15==7 ~ 1, Q15==6 ~ 2, Q15==5 ~ 3, Q15==4 ~ 4, Q15==3 ~ 5,
                    Q15==2 ~ 6, Q15==1 ~ 7),
    Q16 = case_when(Q16==7 ~ 1, Q16==6 ~ 2, Q16==5 ~ 3, Q16==4 ~ 4, Q16==3 ~ 5,
                    Q16==2 ~ 6, Q16==1 ~ 7),
    Q17 = case_when(Q17==7 ~ 1, Q17==6 ~ 2, Q17==5 ~ 3, Q17==4 ~ 4, Q17==3 ~ 5,
                    Q17==2 ~ 6, Q17==1 ~ 7),
    ##identity
    Q22 = parse_number(Q22), Q23 = parse_number(Q23), Q24 = parse_number(Q24),
    Q25 = parse_number(Q25), Q26 = parse_number(Q26), Q27 = parse_number(Q27),
    ##Everyday discrimination
    Q171A = parse_number(Q171A), Q171B = parse_number(Q171B), Q171C = parse_number(Q171C),
    Q171D = parse_number(Q171D), Q171E = parse_number(Q171E), Q171F = parse_number(Q171F), 
    Q171G = parse_number(Q171G), Q171H = parse_number(Q171H), Q171I = parse_number(Q171I),
    ##Social Support
    Q192A = parse_number(Q192A), Q192B = parse_number(Q192B), Q192C = parse_number(Q192C), 
    Q192D = parse_number(Q192D), Q192E = parse_number(Q192E), Q192F = parse_number(Q192F),
    Q192G = parse_number(Q192G), Q192H = parse_number(Q192H), Q192I = parse_number(Q192I),
    Q192J = parse_number(Q192J), Q192K = parse_number(Q192K), Q192L = parse_number(Q192L),
    ##Future Expectations, Transpop Only
    Q191A = parse_number(Q191A), Q191B = parse_number(Q191B), Q191C = parse_number(Q191C), 
    Q191D = parse_number(Q191D), Q191E = parse_number(Q191E), Q191F = parse_number(Q191F),
    Q191G = parse_number(Q191G), Q191H = parse_number(Q191H), Q191I = parse_number(Q191I),
    ##Community Connectedness, Transpop Only
    Q51 = parse_number(Q51), Q52 = parse_number(Q52), Q53 = parse_number(Q53),
    Q54 = parse_number(Q54), Q55 = parse_number(Q55),
    Q54 = case_when(Q54==5 ~ 1, Q54==4 ~ 2, Q54==2 ~ 4, Q54==1 ~ 5, Q54==3 ~ 3),
    Q55 = case_when(Q55==5 ~ 1, Q55==4 ~ 2, Q55==2 ~ 4, Q55==1 ~ 5, Q55==3 ~ 3),
    ##Non-Affirmation of Gender Identity, Transpop Only
    Q45 = parse_number(Q45), Q46 = parse_number(Q46), Q47 = parse_number(Q47), 
    Q48 = parse_number(Q48), Q49 = parse_number(Q49), Q50 = parse_number(Q50))

## mental health score scaled to 24
Combined = Combined %>% 
  mutate(Kessler6 = (Q100A+Q100B+Q100C+Q100D+Q100E+Q100F)/30*24)

## satisfaction with life | 18 missing
LifeSat = Combined %>% 
  select(Q224,Q225,Q226,Q227,Q228)
Combined$LifeSat = rowMeans(LifeSat,na.rm = TRUE)

## social well being | 1 missing
Socialwb = Combined %>% 
  select(Q04,Q05,Q06,Q07,Q08,Q09,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18)
Combined$Socialwb = rowMeans(Socialwb,na.rm = TRUE)

##identity | 12 missing
MultiEthnicID = Combined %>% 
  select(Q22,Q23,Q24,Q25,Q26,Q27)
Combined$MultiEthnicID = rowMeans(MultiEthnicID,na.rm = TRUE)

##Everyday discrimination | 21 missing
Every_dis = Combined %>% 
  select(Q171A,Q171B,Q171C,Q171D,Q171E,Q171F,Q171G,Q171H,Q171I)
Combined$Every_dis = rowMeans(Every_dis,na.rm = TRUE)

##Social Support | 23 missing
SoSupport = Combined %>% 
  select(Q192A,Q192B,Q192C,Q192D,Q192E,Q192F,Q192G,Q192H,Q192I,Q192J,Q192K,Q192L)
Combined$SoSupport = rowMeans(SoSupport,na.rm = TRUE)

##Social Support - family | 24 missing
SoSupportFAM = Combined %>% 
  select(Q192C,Q192D,Q192H,Q192K)
Combined$SoSupportFAM = rowMeans(SoSupportFAM,na.rm = TRUE)

##Social Support - friend | 23 missing
SoSupportFriend = Combined %>% 
  select(Q192F,Q192G,Q192I,Q192L)
Combined$SoSupportFriend = rowMeans(SoSupportFriend,na.rm = TRUE)

##Future Expectations, Transpop Only
NegExp = Combined %>% 
  select(Q191A,Q191B,Q191C,Q191D,Q191E,Q191F,Q191G,Q191H,Q191I)
Combined$NegExp = rowMeans(NegExp,na.rm = TRUE)

##Community Connectedness, Transpop Only
ComCon = Combined %>% 
  select(Q51,Q52,Q53,Q54,Q55)
Combined$ComCon = rowMeans(ComCon,na.rm = TRUE)

##Non-Affirmation of Gender Identity, Transpop Only
NonAffirm = Combined %>% 
  select(Q45,Q46,Q47,Q48,Q49,Q50)
Combined$NonAffirm = rowMeans(NonAffirm,na.rm = TRUE)

##race column cleaning
Combined = Combined %>% 
  mutate(Race_text = str_extract(RACE,"\\d"),
         Race_text = ifelse(Race_text==4,9,Race_text),
         Race_text = ifelse(Race_text==5,9,Race_text),
         Race_text = ifelse(Race_text==7,9,Race_text),
         Race_text = ifelse(Race_text==1,"Asian",Race_text),
         Race_text = ifelse(Race_text==2,"Black/AA",Race_text),
         Race_text = ifelse(Race_text==3,"Hispanic/Latino",Race_text),
         Race_text = ifelse(Race_text==6,"White",Race_text),
         Race_text = ifelse(Race_text==8,"Multirace",Race_text),
         Race_text = ifelse(Race_text==9,"Other",Race_text),
         Race_num = ifelse(Race_text=="Asian",1,Race_text),
         Race_num = ifelse(Race_text=="Black/AA",2,Race_num),
         Race_num = ifelse(Race_text=="Hispanic/Latino",3,Race_num),
         Race_num = ifelse(Race_text=="White",4,Race_num),
         Race_num = ifelse(Race_text=="Multirace",5,Race_num),
         Race_num = ifelse(Race_text=="Other",6,Race_num))

## Creating Race gender intersection
Combined = Combined %>% 
  mutate(Race_Gender = paste(Race_text,Gender_text),
         Race_Gender2 = paste(Data_source,Race_text))
data.frame(table(Combined$Race_Gender))

## Descriptive Exploration
Test1 = Combined %>% 
  filter(Data_source=="trans")

Test2 = Combined %>% 
  filter(Data_source=="cis")

mean(Test1$Kessler6) ## trans kessler
mean(Test2$Kessler6) ## cis kessler

summary(Test1$Kessler6) ## trans kessler
summary(Test2$Kessler6) ## cis kessler
sd(Test1$Kessler6) ## trans kessler
sd(Test2$Kessler6) ## cis kessler
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.80    8.80   12.00   12.23   16.00   23.20 
# > summary(Test2$Kessler6) ## cis kessler
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.800   5.600   7.200   8.187   9.600  24.000 




hist(Test1$Kessler6) ## trans kessler
hist(Test2$Kessler6) ## cis kessler

plot(density(Test1$Kessler6))
plot(density(Test2$Kessler6))


Gender_split = Combined %>% 
  group_by(Gender_text) %>% 
  summarise(mean(Kessler6))
# <chr>                  <dbl>
#   1 cisMan                  7.88
# 2 cisWoman                8.47
# 3 transGNB               13.5 
# 4 transMan               11.7 
# # 5 transWoman             11.8 

##clean this up
cisMan = Combined %>% 
  filter(Gender_text=="cisMan")

cisWoman = Combined %>% 
  filter(Gender_text=="cisWoman")

transGNB = Combined %>% 
  filter(Gender_text=="transGNB")

transMan = Combined %>% 
  filter(Gender_text=="transMan")

transWoman = Combined %>% 
  filter(Gender_text=="transWoman")

##make this plots look better 
plot(density(transWoman$Kessler6))
plot(density(transMan$Kessler6))
plot(density(transGNB$Kessler6))
plot(density(cisWoman$Kessler6))
plot(density(cisMan$Kessler6))

data.frame(table(Combined$Race_Gender))
data.frame(table(Combined$Race_Gender2))

##creating regional & divisional scores based on the state-level friendiness score
##connect two datasets together

install.packages("lme4")

#############################################
### MAKE PRETTY TABLES & PLOTS!!!!###
#############################################

################# for the survey response distribution############################
# Calculate summary statistics for Kessler6 in Test1 dataset ## trans kessler
summary_stats_test1 <- Test1 %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# Print the table
print(summary_stats_test1)

# Calculate summary statistics for Kessler6 in Test2 dataset ## cis kessler
summary_stats_test2 <- Test2 %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# Print the table
print(summary_stats_test2)


#density plots for Kessler by trans and cis survey responses

# Plot density of Test1$Kessler6 with label "trans survey responses"
plot(density(Test1$Kessler6), main = "Density Comparison of Kessler for trans and cis survey responses", 
     xlab = "Kessler Scores (psychological distress)", ylab = "Density", col = "blue", 
     xlim = range(c(Test1$Kessler6, Test2$Kessler6)), ylim = c(0, 0.2))   # Adjust the x-axis limits to fit both densities

# Add density of Test2$Kessler6 to the same plot with label "cis survey responses"
lines(density(Test2$Kessler6), col = "green")

# Add a legend with relabeled entries
legend("topright", legend = c("trans survey responses", "cis survey responses"), col = c("blue", "green"), lty = 1, cex = 0.4)

################################################################################
library(dplyr)
summary_stats_Gender<- Combined %>%
  group_by(Gender_text) %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# View the summary statistics
print(summary_stats_Gender)

# Create an empty plot
plot(density(transWoman$Kessler6), main = "Density Comparison of Kessler by gender identity", 
     xlab = "Kessler Scores (psychological distress)", ylab = "Density", col = "blue",
      ylim = c(0, 0.2))

# Add density plots for other groups
lines(density(transMan$Kessler6), col = "red")
lines(density(transGNB$Kessler6), col = "green")
lines(density(cisWoman$Kessler6), col = "purple")
lines(density(cisMan$Kessler6), col = "orange")

# Add a legend
legend("topright", legend = c("Trans Woman", "Trans Man", "Gender Non-Binary", "Cis Woman", "Cis Man"), 
       col = c("blue", "red", "green", "purple", "orange"), lty = 1, cex = 0.45)


################################################################################
# Group by Race_Gender and calculate summary statistics for Kessler6
summary_stats_Race_Gender <- Combined %>%
  group_by(Race_Gender) %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# View the summary statistics
print(summary_stats_Race_Gender )

# Group by Race_Gender2 and calculate summary statistics for Kessler6
summary_stats_Race_Gender2 <- Combined %>%
  group_by(Race_Gender2) %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# View the summary statistics
print(summary_stats_Race_Gender2 )

# Load required libraries
library(ggplot2)


# Create a bar plot for mean Kessler6 scores for each category of Race_Gender2
ggplot(summary_stats_Race_Gender2, aes(x = Race_Gender2, y = Mean_Kessler6)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Kessler6 Scores by Race/Gender", x = "Race/Gender", y = "Mean Kessler6 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Create a violin plot
# Define custom colors for each race
race_colors <- c("cis Asian" = "#ADD8E6", "trans Asian" = "#4169E1",
                 "cis Black/AA" = "#FFA07A", "trans Black/AA" = "#8B4513",
                 "cis Hispanic/Latino" = "#98FB98", "trans Hispanic/Latino" = "#228B22",
                 "cis Multirace" = "#FFD700", "trans Multirace" = "#B8860B",
                 "cis Other" = "#FFC0CB", "trans Other" = "#FF69B4",
                 "cis White" = "#F0FFFF", "trans White" = "#4682B4")

# Reorder Race_Gender2 factor levels to place cis and trans groups next to each other
Combined$Race_Gender2 <- factor(Combined$Race_Gender2, levels = c("cis Asian", "trans Asian", "cis Black/AA", "trans Black/AA", 
                                                                  "cis Hispanic/Latino", "trans Hispanic/Latino", "cis Multirace", "trans Multirace",
                                                                  "cis Other", "trans Other", "cis White", "trans White"))
ggplot(Combined, aes(x = Race_Gender2, y = Kessler6, fill = Race_Gender2)) +
  geom_violin(color = "blue") +
  geom_boxplot(width = 0.1, fill = "white", color = "blue", outlier.shape = NA) +
  scale_fill_manual(values = race_colors) +  # Specify custom colors
  labs(title = "Distribution of Kessler Scores by Race/Gender",
       x = "Race/Gender",
       y = "Kessler6 Score (psychological distress)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + # Rotate x-axis labels
  guides(fill = FALSE) 


################################################################################
# Group by Race_Gender2 and GCENREG and calculate summary statistics for Kessler6
summary_stats_Race_Gender2_GCENREG <- Combined %>%
  group_by(Race_Gender2, GCENREG) %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )


# View the summary statistics
print(summary_stats_Race_Gender2_GCENREG)
View(summary_stats_Race_Gender2_GCENREG)

# Group by Race_Gender2 and GURBAN and calculate summary statistics for Kessler6
summary_stats_Race_Gender2_GURBAN <- Combined %>%
  group_by(Race_Gender2, GURBAN) %>%
  summarise(
    Mean_Kessler6 = mean(Kessler6, na.rm = TRUE),
    Median_Kessler6 = median(Kessler6, na.rm = TRUE),
    SD_Kessler6 = sd(Kessler6, na.rm = TRUE),
    Min_Kessler6 = min(Kessler6, na.rm = TRUE),
    Max_Kessler6 = max(Kessler6, na.rm = TRUE),
    N = n()
  )

# View the summary statistics
print(summary_stats_Race_Gender2_GURBAN)
View(summary_stats_Race_Gender2_GURBAN)

################################################################################
# Group by Race_Gender2 and plot association between Kessler6 and miles away from clinic

# Define custom colors for each race
race_colors <- c("cis Asian" = "#ADD8E6", "trans Asian" = "#4169E1",
                 "cis Black/AA" = "#FFA07A", "trans Black/AA" = "#8B4513",
                 "cis Hispanic/Latino" = "#98FB98", "trans Hispanic/Latino" = "#228B22",
                 "cis Multirace" = "#FFD700", "trans Multirace" = "#B8860B",
                 "cis Other" = "#FFC0CB", "trans Other" = "#FF69B4",
                 "cis White" = "#F0FFFF", "trans White" = "#4682B4")

# Create scatterplots with lines of best fit, custom colors, and exclude data points with miles over 200
scatterplots <- ggplot(Combined, aes(x = GMILESAWAY, y = Kessler6, color = Race_Gender2)) +
  geom_point(data = subset(Combined, GMILESAWAY <= 200)) +  # Exclude data points with GMILESAWAY over 200
  geom_smooth(data = subset(Combined, GMILESAWAY <= 200), method = "lm", se = FALSE) +  # Add lines of best fit
  scale_color_manual(values = race_colors) +  # Apply custom colors
  facet_wrap(~ Race_Gender2, scales = "free") +  # Separate plots for each category
  labs(x = "Miles Away from LGBT Health Care Center", y = "Kessler Score (psychological distress)") +  # Label axes
  theme_minimal()

# Print the scatterplots with lines of best fit and custom colors
print(scatterplots)
