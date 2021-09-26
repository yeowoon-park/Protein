library(plyr)
library(dplyr)
library(psych)
library(micEconAids)
library(plm)
library(reshape2)
library(readxl)
library(writexl)

# 데이터 불러오기 ----------------------------------------------------------------
data <- read.csv("aids833.csv")
bmi <- read.csv("bmi.csv")
bmi_code <- read.csv("panel_code.csv")
pet <- load("pet.grocery.data.RData")
survey <- read.csv("survey.csv")

# 각 데이터별 패널 뽑아내기 ----------------------------------------------------------
data.panel <- unique(data %>% select(panel_code))

bmi <- inner_join(bmi, bmi_code)
bmi <- bmi %>% select(panel_code, BMI)
bmi.panel <- unique(bmi %>% select(panel_code))

pet <- pet.data %>% select(panel_code, pet_YN, vegi_1, vegi_2, vegi_3, vegi_4)
pet <- pet %>% mutate(pet_YN = ifelse(grepl('예', pet_YN), '1', '0'))
pet.panel <- unique(pet %>% select(panel_code))

survey <- survey[-c(378, 391, 400, 424, 452, 462, 778, 1142, 1154, 1169, 1206),]
rownames(survey) <- NULL

survey <- subset(survey, select = -c(ED, IC_1, IC_2, IC_3, IC_4))

names(survey) <- c("panel_code", "EWE1", "EWE2","EWE3", "EWA1", "EWA2", "HLSM1", "HLSM2", "HLSM3", "HLSM4", "HLIS1", "HLIS2",
                   "HLIS3", "HLIS4", "HLIS5", "HLSA1", "HLSA2", "HLSA3", "HLSA4", "HLSA5", "HLSA6", "HLE1", "HLE2", "HLE3", "HLE4", "HLE5",
                   "HLE6", "HLHR1", "HLHR2", "HLHR3", "HLHR4", "HLN1", "HLN2", "HLN3", "HLN4", "HLN5", "HLN6", "HC1", "HC2", "HC3", "HC4", "HC5",
                   "HC6", "HC7", "HC8")

survey$HLSM2 <- as.numeric(survey$HLSM2)
survey$HLSM4 <- as.numeric(survey$HLSM4)
survey$HC5 <- as.numeric(survey$HC5)

survey$EWE1 <- ifelse(survey$EWE1<1|survey$EWE1>7, NA, survey$EWE1)
survey$EWE2 <- ifelse(survey$EWE2<1|survey$EWE2>7, NA, survey$EWE2)
survey$EWE3 <- ifelse(survey$EWE3<1|survey$EWE3>7, NA, survey$EWE3)
survey$EWA1 <- ifelse(survey$EWA1<1|survey$EWA1>7, NA, survey$EWA1)
survey$EWA2 <- ifelse(survey$EWA2<1|survey$EWA2>7, NA, survey$EWA2)

survey$HLSM1 <- ifelse(survey$HLSM1<1|survey$HLSM1>5, NA, survey$HLSM1)
survey$HLSM2 <- ifelse(survey$HLSM1<1|survey$HLSM1>5, NA, survey$HLSM2)
survey$HLSM3 <- ifelse(survey$HLSM1<1|survey$HLSM1>5, NA, survey$HLSM3)
survey$HLSM4 <- ifelse(survey$HLSM1<1|survey$HLSM1>5, NA, survey$HLSM4)

survey$HLIS1 <- ifelse(survey$HLIS1<1|survey$HLIS1>5, NA, survey$HLIS1)
survey$HLIS2 <- ifelse(survey$HLIS2<1|survey$HLIS2>5, NA, survey$HLIS2)
survey$HLIS3 <- ifelse(survey$HLIS3<1|survey$HLIS3>5, NA, survey$HLIS3)
survey$HLIS4 <- ifelse(survey$HLIS4<1|survey$HLIS4>5, NA, survey$HLIS4)
survey$HLIS5 <- ifelse(survey$HLIS5<1|survey$HLIS5>5, NA, survey$HLIS5)

survey$HLSA1 <- ifelse(survey$HLSA1<1|survey$HLSA1>5, NA, survey$HLSA1)
survey$HLSA2 <- ifelse(survey$HLSA2<1|survey$HLSA2>5, NA, survey$HLSA2)
survey$HLSA3 <- ifelse(survey$HLSA3<1|survey$HLSA3>5, NA, survey$HLSA3)
survey$HLSA4 <- ifelse(survey$HLSA4<1|survey$HLSA4>5, NA, survey$HLSA4)
survey$HLSA5 <- ifelse(survey$HLSA5<1|survey$HLSA5>5, NA, survey$HLSA5)
survey$HLSA6 <- ifelse(survey$HLSA6<1|survey$HLSA6>5, NA, survey$HLSA6)

survey$HLE1 <- ifelse(survey$HLE1<1|survey$HLE1>5, NA, survey$HLE1)
survey$HLE2 <- ifelse(survey$HLE2<1|survey$HLE2>5, NA, survey$HLE2)
survey$HLE3 <- ifelse(survey$HLE3<1|survey$HLE3>5, NA, survey$HLE3)
survey$HLE4 <- ifelse(survey$HLE4<1|survey$HLE4>5, NA, survey$HLE4)
survey$HLE5 <- ifelse(survey$HLE5<1|survey$HLE5>5, NA, survey$HLE5)
survey$HLE6 <- ifelse(survey$HLE6<1|survey$HLE6>5, NA, survey$HLE6)

survey$HLHR1 <- ifelse(survey$HLHR1<1|survey$HLHR1>5, NA, survey$HLHR1)
survey$HLHR2 <- ifelse(survey$HLHR2<1|survey$HLHR2>5, NA, survey$HLHR2)
survey$HLHR3 <- ifelse(survey$HLHR3<1|survey$HLHR3>5, NA, survey$HLHR3)
survey$HLHR4 <- ifelse(survey$HLHR4<1|survey$HLHR4>5, NA, survey$HLHR4)

survey$HLN1 <- ifelse(survey$HLN1<1|survey$HLN1>5, NA, survey$HLN1)
survey$HLN2 <- ifelse(survey$HLN2<1|survey$HLN2>5, NA, survey$HLN2)
survey$HLN3 <- ifelse(survey$HLN3<1|survey$HLN3>5, NA, survey$HLN3)
survey$HLN4 <- ifelse(survey$HLN4<1|survey$HLN4>5, NA, survey$HLN4)
survey$HLN5 <- ifelse(survey$HLN5<1|survey$HLN5>5, NA, survey$HLN5)
survey$HLN6 <- ifelse(survey$HLN6<1|survey$HLN6>5, NA, survey$HLN6)

survey$HC1 <- ifelse(survey$HC1<1|survey$HC1>5, NA, survey$HC1)
survey$HC2 <- ifelse(survey$HC2<1|survey$HC2>5, NA, survey$HC2)
survey$HC3 <- ifelse(survey$HC3<1|survey$HC3>5, NA, survey$HC3)
survey$HC4 <- ifelse(survey$HC4<1|survey$HC4>5, NA, survey$HC4)
survey$HC5 <- ifelse(survey$HC5<1|survey$HC5>5, NA, survey$HC5)
survey$HC6 <- ifelse(survey$HC6<1|survey$HC6>5, NA, survey$HC6)
survey$HC7 <- ifelse(survey$HC7<1|survey$HC7>5, NA, survey$HC7)
survey$HC8 <- ifelse(survey$HC8<1|survey$HC8>5, NA, survey$HC8)

survey <- na.omit(survey)

summary(survey)

survey.panel <- unique(survey %>% select(panel_code))

# 공통 패널 뽑아내기 (454 가구)--------------------------------------------------------------
panel1 <- inner_join(data.panel, bmi.panel)
panel2 <- inner_join(panel1, pet.panel)
panel <- inner_join(panel2, survey.panel)

# 공통 패널들의 데이터 추출하기 --------------------------------------------------------
co.data <- inner_join(data, panel)

co.bmi <- inner_join(bmi, panel) ##460
bmi_occur <- data.frame(table(co.bmi$panel_code))
bmi_occur[bmi_occur$Freq >1,]
co.bmi = co.bmi[-which(duplicated(co.bmi$panel_code)),]

co.pet <- unique(pet)
co.pet <- inner_join(co.pet, panel)

co.survey <- inner_join(survey, panel)

# 데이터 통합하기 ----------------------------------------------------------------
co1 <- inner_join(co.data, co.bmi)
co2 <- inner_join(co1, co.pet)
co <- inner_join(co2, co.survey)

co$HC1 <- as.numeric(co$HC1)
co$HC2 <- as.numeric(co$HC2)
co$HC3 <- as.numeric(co$HC3)
co$HC4 <- as.numeric(co$HC4)
co$HC5 <- as.numeric(co$HC5)

co <- co %>% mutate(EWE = (EWE1+EWE2+EWE3)/3) %>% mutate(EWA = (EWA1+EWA2)/2) %>%
  mutate(SM = (HLSM1+HLSM2+HLSM3+HLSM4)/4) %>% mutate(IS = (HLIS1+HLIS2+HLIS3+HLIS4+HLIS5)/5) %>%
  mutate(SA = (HLSA1+HLSA2+HLSA3+HLSA4+HLSA5+HLSA6)/6) %>% mutate(HLE = (HLE1+HLE2+HLE3+HLE4+HLE5+HLE6)/6) %>%
  mutate(HR = (HLHR1+HLHR2+HLHR3+HLHR4)/4) %>% mutate(HLN = (HLN1+HLN2+HLN3+HLN4+HLN5+HLN6)/6) %>%
  mutate(HC = (HC1+HC2+HC3+HC4+HC5+HC6+HC7+HC8)/8) %>% mutate(VEGE = (vegi_1+vegi_2+vegi_3+vegi_4)/4)

summary(co)
write.csv(co, file="aids454.csv")