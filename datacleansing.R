library(plyr)
library(dplyr)
library(psych)
library(micEconAids)
library(plm)
library(reshape2)
library(writexl)
library(base)

memory.limit(5000000)

# 835 데이터 로드 (2019년 12월 데이터 제거)(check)--------------------------------------------------------------
load("total835.RData")

total.835.1 <- total.835 %>% filter(year == "2015"|year == "2016"|year == "2017"|year == "2018")
total.835.2 <- total.835 %>% filter(year == "2019" & month < 12)
total.835 = rbind(total.835.1, total.835.2)

check <- unique(total.835 %>% select(year, month))
rm(check)

# 단백질 데이터만 모으기 (protein)(check)------------------------------------------------------------
beef_d <- total.835 %>% filter(middle_new == "쇠고기")%>% filter(middle == "쇠고기_국내산") %>% mutate(category = "dbeef") 

beef_f <- total.835 %>% filter(middle_new == "쇠고기")%>% filter(middle == "쇠고기_수입산") %>% mutate(category = "fbeef")

chicken <- total.835 %>% filter(middle_new == "닭고기") %>% mutate(category = "chicken") 

pork <- total.835 %>% filter(middle_new == "돼지고기") %>% mutate(category = "pork") 

clam_a <- total.835 %>% filter(detail == "갑각_조개류")
clam_a <- clam_a[grep("전복|전북", clam_a$product),] %>% mutate(category = "abal") 

clam_o <- total.835 %>% filter(detail == "굴") %>% mutate(category = "oys") 

clam_c <- total.835 %>% filter(detail == "갑각_조개류")
clam_c <- clam_c[grep("바지락|꼬막|피조개|고막|홍합|담치|가리비|재첩|제첩|대합|키조개|모시|백합|개조개|동죽|새조개|맛조개|조개|조갯살|관자|죽합|중합|생합|바지막|패류|반지락|열합|상합", clam_c$product),] %>% mutate(category = "joge") 

crab <- total.835 %>% filter(detail == "갑각_조개류")
crab <- crab[grep("게|크랩", crab$product),]%>% mutate(category = "crab") 

shrimp <- total.835 %>% filter(detail == "갑각_조개류")
shrimp <- shrimp[grep("새우|대하|블랙타이거|쉬림프|슈림프|슈프림", shrimp$product),]%>% mutate(category = "shrimp") 

squid <- total.835 %>% filter(detail == "연체류")
squid <- squid[grep("오징어", squid$product),]%>% mutate(category = "squid")

squid_s <- total.835 %>% filter(detail == "연체류")
squid_s <- squid_s[grep("낙지|낚지", squid_s$product),]%>% mutate(category = "web") 

mackerel <- total.835 %>% filter(detail == "생선류")
mackerel <- mackerel[grep("고등어", mackerel$product),]%>% mutate(category = "mack") 

jogi <- total.835 %>% filter(detail == "생선류")
jogi <- jogi[grep("조기", jogi$product),]%>% mutate(category = "jogi") 

myung <- total.835 %>% filter(detail == "생선류")
myung <- myung[grep("명태|생태|동태", myung$product),]%>% mutate(category = "myung") 

gal <- total.835 %>% filter(detail == "생선류")
gal <- gal[grep("갈치|칼치|깔치", gal$product),]%>% mutate(category = "gal") 

tofu <- total.835 %>% filter(sub_detail_1 == "두부") %>% mutate(category = "tofu") 

bean <- total.835 %>% filter(middle_new == "두류") %>% mutate(category = "bean") 

protein <- rbind(beef_d, beef_f, chicken, pork, clam_a, clam_o, clam_c, crab, shrimp, squid, squid_s, mackerel, jogi, myung, gal, tofu, bean)
protein$year <- as.numeric(protein$year)

check <- unique(protein %>% select(category))
rm(check)

write_xlsx(protein, path = "protein.xlsx")

# unit price 만들기(outlier 제거 후 unitprice 생성) ----------------------------------------------------------

# beef_d (36266.14)------------------------------------------------------------------
sum(is.na(beef_d$w_q_kg))

quantile(beef_d$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(beef_d$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(beef_d$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- beef_d[which(beef_d$w_q_kg < LowerQ),]
upperOutlier <- beef_d[which(beef_d$w_q_kg > UpperQ),]

beef_d.p <- anti_join(beef_d, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
beef_d.p <- anti_join(beef_d.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

beef_d.p <- transform(beef_d.p,
                     quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

beef_d.p <- transform(beef_d.p,
                    unit = ifelse(is.na(quant), NA, "KG"))

beef_d.p <- beef_d.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# beef_f (22039.89)------------------------------------------------------------------
sum(is.na(beef_f$w_q_kg))

quantile(beef_f$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(beef_f$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(beef_f$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- beef_f[which(beef_f$w_q_kg < LowerQ),]
upperOutlier <- beef_f[which(beef_f$w_q_kg > UpperQ),]

beef_f.p <- anti_join(beef_f, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
beef_f.p <- anti_join(beef_f.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

beef_f.p <- transform(beef_f.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

beef_f.p <- transform(beef_f.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

beef_f.p <- beef_f.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# chicken (7992.759)------------------------------------------------------------------
sum(is.na(chicken$w_q_kg))

quantile(chicken$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(chicken$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(chicken$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- chicken[which(chicken$w_q_kg < LowerQ),]
upperOutlier <- chicken[which(chicken$w_q_kg > UpperQ),]

chicken.p <- anti_join(chicken, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
chicken.p <- anti_join(chicken.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

chicken.p <- transform(chicken.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

chicken.p <- transform(chicken.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

chicken.p <- chicken.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# pork (13824.88)------------------------------------------------------------------
sum(is.na(pork$w_q_kg))

quantile(pork$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(pork$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(pork$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- pork[which(pork$w_q_kg < LowerQ),]
upperOutlier <- pork[which(pork$w_q_kg > UpperQ),]

pork.p <- anti_join(pork, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
pork.p <- anti_join(pork.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

pork.p <- transform(pork.p,
                       quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

pork.p <- transform(pork.p,
                       unit = ifelse(is.na(quant), NA, "KG"))

pork.p <- pork.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# clam_a (64878.64)------------------------------------------------------------------
sum(is.na(clam_a$w_q_kg))

quantile(clam_a$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(clam_a$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(clam_a$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- clam_a[which(clam_a$w_q_kg < LowerQ),]
upperOutlier <- clam_a[which(clam_a$w_q_kg > UpperQ),]

clam_a.p <- anti_join(clam_a, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
clam_a.p <- anti_join(clam_a.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

clam_a.p <- transform(clam_a.p,
                    quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

clam_a.p <- transform(clam_a.p,
                    unit = ifelse(is.na(quant), NA, "KG"))

clam_a.p <- clam_a.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)


# clam_o (13722.587)------------------------------------------------------------------
sum(is.na(clam_o$w_q_kg))

quantile(clam_o$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(clam_o$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(clam_o$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- clam_o[which(clam_o$w_q_kg < LowerQ),]
upperOutlier <- clam_o[which(clam_o$w_q_kg > UpperQ),]

clam_o.p <- anti_join(clam_o, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
clam_o.p <- anti_join(clam_o.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

clam_o.p <- transform(clam_o.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

clam_o.p <- transform(clam_o.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

clam_o.p <- clam_o.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# clam_c (7483.548)------------------------------------------------------------------
sum(is.na(clam_c$w_q_kg))

quantile(clam_c$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(clam_c$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(clam_c$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- clam_c[which(clam_c$w_q_kg < LowerQ),]
upperOutlier <- clam_c[which(clam_c$w_q_kg > UpperQ),]

clam_c.p <- anti_join(clam_c, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
clam_c.p <- anti_join(clam_c.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

clam_c.p <- transform(clam_c.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

clam_c.p <- transform(clam_c.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

clam_c.p <- clam_c.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# crab (11459.20)------------------------------------------------------------------
sum(is.na(crab$w_q_kg))

quantile(crab$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(crab$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(crab$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- crab[which(crab$w_q_kg < LowerQ),]
upperOutlier <- crab[which(crab$w_q_kg > UpperQ),]

crab.p <- anti_join(crab, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
crab.p <- anti_join(crab.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

crab.p <- transform(crab.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

crab.p <- transform(crab.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

crab.p <- crab.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# shrimp ()------------------------------------------------------------------
sum(is.na(shrimp$w_q_kg))

quantile(shrimp$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(shrimp$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(shrimp$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- shrimp[which(shrimp$w_q_kg < LowerQ),]
upperOutlier <- shrimp[which(shrimp$w_q_kg > UpperQ),]

shrimp.p <- anti_join(shrimp, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
shrimp.p <- anti_join(shrimp.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

shrimp.p <- transform(shrimp.p,
                    quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

shrimp.p <- transform(shrimp.p,
                    unit = ifelse(is.na(quant), NA, "KG"))

shrimp.p <- shrimp.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

write.csv(shrimp.p, "shrimp_unitp.csv")

# squid (8094.340)------------------------------------------------------------------
sum(is.na(squid$w_q_kg))

quantile(squid$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(squid$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(squid$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- squid[which(squid$w_q_kg < LowerQ),]
upperOutlier <- squid[which(squid$w_q_kg > UpperQ),]

squid.p <- anti_join(squid, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
squid.p <- anti_join(squid.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

squid.p <- transform(squid.p,
                      quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

squid.p <- transform(squid.p,
                      unit = ifelse(is.na(quant), NA, "KG"))

squid.p <- squid.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# squid_s (14765.101)------------------------------------------------------------------
sum(is.na(squid_s$w_q_kg))

quantile(squid_s$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(squid_s$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(squid_s$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- squid_s[which(squid_s$w_q_kg < LowerQ),]
upperOutlier <- squid_s[which(squid_s$w_q_kg > UpperQ),]

squid_s.p <- anti_join(squid_s, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
squid_s.p <- anti_join(squid_s.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

squid_s.p <- transform(squid_s.p,
                     quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

squid_s.p <- transform(squid_s.p,
                     unit = ifelse(is.na(quant), NA, "KG"))

squid_s.p <- squid_s.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# mackerel (15852.972)------------------------------------------------------------------
sum(is.na(mackerel$w_q_kg))

quantile(mackerel$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(mackerel$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(mackerel$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- mackerel[which(mackerel$w_q_kg < LowerQ),]
upperOutlier <- mackerel[which(mackerel$w_q_kg > UpperQ),]

mackerel.p <- anti_join(mackerel, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
mackerel.p <- anti_join(mackerel.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

mackerel.p <- transform(mackerel.p,
                       quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

mackerel.p <- transform(mackerel.p,
                       unit = ifelse(is.na(quant), NA, "KG"))

mackerel.p <- mackerel.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# jogi (7862.385)------------------------------------------------------------------
sum(is.na(jogi$w_q_kg))

quantile(jogi$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(jogi$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(jogi$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- jogi[which(jogi$w_q_kg < LowerQ),]
upperOutlier <- jogi[which(jogi$w_q_kg > UpperQ),]

jogi.p <- anti_join(jogi, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
jogi.p <- anti_join(jogi.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

jogi.p <- transform(jogi.p,
                        quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

jogi.p <- transform(jogi.p,
                        unit = ifelse(is.na(quant), NA, "KG"))

jogi.p <- jogi.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# myung (8252.521)------------------------------------------------------------------
sum(is.na(myung$w_q_kg))

quantile(myung$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(myung$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(myung$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- myung[which(myung$w_q_kg < LowerQ),]
upperOutlier <- myung[which(myung$w_q_kg > UpperQ),]

myung.p <- anti_join(myung, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
myung.p <- anti_join(myung.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

myung.p <- transform(myung.p,
                    quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

myung.p <- transform(myung.p,
                    unit = ifelse(is.na(quant), NA, "KG"))

myung.p <- myung.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# gal (12003.690)------------------------------------------------------------------
sum(is.na(gal$w_q_kg))

quantile(gal$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(gal$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(gal$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- gal[which(gal$w_q_kg < LowerQ),]
upperOutlier <- gal[which(gal$w_q_kg > UpperQ),]

gal.p <- anti_join(gal, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
gal.p <- anti_join(gal.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

gal.p <- transform(gal.p,
                     quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

gal.p <- transform(gal.p,
                     unit = ifelse(is.na(quant), NA, "KG"))

gal.p <- gal.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# tofu (4692.499)------------------------------------------------------------------
sum(is.na(tofu$w_q_kg))

quantile(tofu$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(tofu$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(tofu$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- tofu[which(tofu$w_q_kg < LowerQ),]
upperOutlier <- tofu[which(tofu$w_q_kg > UpperQ),]

tofu.p <- anti_join(tofu, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
tofu.p <- anti_join(tofu.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

tofu.p <- transform(tofu.p,
                   quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

tofu.p <- transform(tofu.p,
                   unit = ifelse(is.na(quant), NA, "KG"))

tofu.p <- tofu.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)

# bean (8118.139)------------------------------------------------------------------
sum(is.na(bean$w_q_kg))

quantile(bean$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)
UpperQ = quantile(bean$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[4]
LowerQ = quantile(bean$w_q_kg, probs = c(0.05, 0.1, 0.90, 0.95), na.rm = TRUE)[1]

lowerOutlier <- bean[which(bean$w_q_kg < LowerQ),]
upperOutlier <- bean[which(bean$w_q_kg > UpperQ),]

bean.p <- anti_join(bean, lowerOutlier, by=c('w_q_kg' = 'w_q_kg'))
bean.p <- anti_join(bean.p, upperOutlier, by=c('w_q_kg' = 'w_q_kg'))

bean.p <- transform(bean.p,
                    quant = ifelse(w_q_kg_unit == "KG", w_q_kg,))

bean.p <- transform(bean.p,
                    unit = ifelse(is.na(quant), NA, "KG"))

bean.p <- bean.p %>% filter(unit =="KG") %>%  group_by(category, unit, year, month) %>% summarise(exp = sum(purchase), obs = n(), quant = sum(quant)) %>%
  mutate(unitp = exp/quant)


# protein.demo(2015년에도 단백질을 구매한 경험이 있는 833가구 데이터) (check) -------------------------
protein.demo <- unique(protein %>% filter(year == "2015") %>% select(panel_code, income, family_num, child_index, age1, region))
protein.demo <- protein.demo %>% mutate(age = 2021-age1+1)
str(protein.demo)
panel_occur <- data.frame(table(protein.demo$panel_code))
panel_occur[panel_occur$Freq >1,]

#save(protein, file = "protein.RData")
#save(protein.demo, file = "protein_demo.RData") ##833

rm(clam_a, clam_c, clam_o, crab, shrimp, gal, jogi, mackerel, myung, pork, squid, squid_s, tofu, total.835.1, total.835.2,
   bean, beef_d, beef_f, chicken, clam_a.p, clam_c.p, clam_o.p, crab.p, shrimp.p, gal.p, jogi.p, mackerel.p, myung.p, pork.p, squid.p, squid_s.p, tofu.p, bean.p, beef_d.p, beef_f.p, chicken.p, lowerOutlier,
   panel_occur, upperOutlier)

# protein 데이터 로드 (전체, 835코드, 그러나 2015년 demo가 있는 가구는 833가구밖에 없으므로 이 가구 데이터와 나중에 조인해야함) ----------------------------------------------------------
#load("protein.RData")

# 단백질 category별 구매비중 계산(check: 835가구 중에 2015년부터 2019년 11월까지 모두 단백질을 구매한 이력이 있는 가구는 454가구) ---------------------------------------------------
protein1 <- subset(protein, select=c(panel_code, year, month, purchase, category))

protein.e.1 <- protein1 %>% group_by(panel_code, year, month) %>% summarise(E = sum(purchase)) ##각 패널들의 월별 단백질 전체 구매액(E)
protein.e.2 <- protein1 %>% group_by(panel_code, year, month, category) %>% summarise(Ed = sum(purchase)) ##각 패널들의 카테고리별 월별 단백질 전체 구매액(Ed)

protein.w.1 <- left_join(protein.e.2, protein.e.1, by = c('panel_code'='panel_code','year'='year', 'month'='month'))

protein.w <- protein.w.1 %>% mutate(w = Ed/E) ##각 패널들의 월별 카테고리별 단백질 지출 비중(w)

protein.w$year <- as.numeric(protein.w$year)
protein.w$month <- as.numeric(protein.w$month)

#save(protein.w, file = "protein_w_833.csv")
#write.csv(protein.w, file = "protein_w.csv")

rm(protein.e.1, protein.e.2, protein.w.1,protein1)

# protein.w, price_new 데이터 로드 (2015=100) ---------------------------------------------------
price <- read.csv(file="price_new2.csv")
#load("protein_w.RData")

id <- unique(subset(protein.w, select=c(panel_code, year, month))) #46721
id_demo <- unique(subset(protein.w, select=c(panel_code)))##총 835개

# category별 가격데이터 붙여서 분석용 데이터 만들기 (check)-----------------------------------------
### 쇠고기_국내산(18716)  ----------------------------------------------------------------------
dbeef <- protein.w %>% filter(category == 'dbeef')
dbeef.1 <- left_join(id, dbeef, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
dbeef.1$category[is.na(dbeef.1$category)] <- 'dbeef'
dbeef.1 <- left_join(dbeef.1, price, by = c('category'='category', 'year'='year', 'month'='month'))
dbeef.1[is.na(dbeef.1)] <- 0
dbeef.2 <- subset(dbeef.1, select=c(panel_code, year, month, price, Ed, w))
dbeef.2<- rename(dbeef.2, "pdbeef" = "price", "xdbeef" = "Ed", "wdbeef" = "w")

### 쇠고기_수입산(11002)  ----------------------------------------------------------------------
fbeef <- protein.w %>% filter(category == 'fbeef')
fbeef.1 <- left_join(id, fbeef, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
fbeef.1$category[is.na(fbeef.1$category)] <- 'fbeef'
fbeef.1 <- left_join(fbeef.1, price, by = c('category'='category','year'='year', 'month'='month'))
fbeef.1[is.na(fbeef.1)] <- 0
fbeef.2 <- subset(fbeef.1, select=c(panel_code, year, month, price, Ed, w))
fbeef.2<- rename(fbeef.2, "pfbeef" = "price", "xfbeef" = "Ed", "wfbeef" = "w")

### 닭고기 (18351)  ----------------------------------------------------------------------
chicken <- protein.w %>% filter(category == 'chicken')
chicken.1 <- left_join(id, chicken, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
chicken.1$category[is.na(chicken.1$category)] <- 'chicken'
chicken.1 <- left_join(chicken.1, price, by = c('category'='category','year'='year', 'month'='month'))
chicken.1[is.na(chicken.1)] <- 0
chicken.2 <- subset(chicken.1, select=c(panel_code, year, month, price, Ed, w))
chicken.2<- rename(chicken.2, "pchicken" = "price", "xchicken" = "Ed", "wchicken" = "w")

### 돼지고기 (35148) ----------------------------------------------------------------------
pork <- protein.w %>% filter(category == 'pork')
pork.1 <- left_join(id, pork, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
pork.1$category[is.na(pork.1$category)] <- 'pork'
pork.1 <- left_join(pork.1, price, by = c('category'='category','year'='year', 'month'='month'))
pork.1[is.na(pork.1)] <- 0
pork.2 <- subset(pork.1, select=c(panel_code, year, month, price, Ed, w))
pork.2<- rename(pork.2, "ppork" = "price", "xpork" = "Ed", "wpork" = "w")

### 전복 (2860) ----------------------------------------------------------------------
abalone <- protein.w %>% filter(category == 'abal')
abalone.1 <- left_join(id, abalone, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
abalone.1$category[is.na(abalone.1$category)] <- 'abal'
abalone.1 <- left_join(abalone.1, price, by = c('category'='category','year'='year', 'month'='month'))
abalone.1[is.na(abalone.1)] <- 0
abalone.2 <- subset(abalone.1, select=c(panel_code, year, month, price, Ed, w))
abalone.2<- rename(abalone.2, "pabal" = "price", "xabal" = "Ed", "wabal" = "w")

### 굴 (4403) ----------------------------------------------------------------------
oyster <- protein.w %>% filter(category == 'oys')
oyster.1 <- left_join(id, oyster, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
oyster.1$category[is.na(oyster.1$category)] <- 'oys'
oyster.1 <- left_join(oyster.1, price, by = c('category'='category','year'='year', 'month'='month'))
oyster.1[is.na(oyster.1)] <- 0
oyster.2 <- subset(oyster.1, select=c(panel_code, year, month, price, Ed, w))
oyster.2<- rename(oyster.2, "poys" = "price", "xoys" = "Ed", "woys" = "w")

### 조개 (8758)  ----------------------------------------------------------------------
clam <- protein.w %>% filter(category == 'joge')
clam.1 <- left_join(id, clam, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
clam.1$category[is.na(clam.1$category)] <- 'joge'
clam.1 <- left_join(clam.1, price, by = c('category'='category','year'='year', 'month'='month'))
clam.1[is.na(clam.1)] <- 0
clam.2 <- subset(clam.1, select=c(panel_code, year, month, price, Ed, w))
clam.2<- rename(clam.2, "pjoge" = "price", "xjoge" = "Ed", "wjoge" = "w")

### 게 (2312)  ----------------------------------------------------------------------
crab <- protein.w %>% filter(category == 'crab')
crab.1 <- left_join(id, crab, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
crab.1$category[is.na(crab.1$category)] <- 'crab'
crab.1 <- left_join(crab.1, price, by = c('category'='category','year'='year', 'month'='month'))
crab.1[is.na(crab.1)] <- 0
crab.2 <- subset(crab.1, select=c(panel_code, year, month, price, Ed, w))
crab.2<- rename(crab.2, "pcrab" = "price", "xcrab" = "Ed", "wcrab" = "w")

### 새우 (5711)  ----------------------------------------------------------------------
shrimp <- protein.w %>% filter(category == 'shrimp')
shrimp.1 <- left_join(id, shrimp, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
shrimp.1$category[is.na(shrimp.1$category)] <- 'shrimp'
shrimp.1 <- left_join(shrimp.1, price, by = c('category'='category','year'='year', 'month'='month'))
shrimp.1[is.na(shrimp.1)] <- 0
shrimp.2 <- subset(shrimp.1, select=c(panel_code, year, month, price, Ed, w))
shrimp.2<- rename(shrimp.2, "pshrimp" = "price", "xshrimp" = "Ed", "wshrimp" = "w")

### 오징어 (9600) ----------------------------------------------------------------------
squid <- protein.w %>% filter(category == 'squid')
squid.1 <- left_join(id, squid, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
squid.1$category[is.na(squid.1$category)] <- 'squid'
squid.1 <- left_join(squid.1, price, by = c('category'='category','year'='year', 'month'='month'))
squid.1[is.na(squid.1)] <- 0
squid.2 <- subset(squid.1, select=c(panel_code, year, month, price, Ed, w))
squid.2<- rename(squid.2, "psquid" = "price", "xsquid" = "Ed", "wsquid" = "w")

### 낙지(1971)  ----------------------------------------------------------------------
webfoot <- protein.w %>% filter(category == 'web')
webfoot.1 <- left_join(id, webfoot, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
webfoot.1$category[is.na(webfoot.1$category)] <- 'web'
webfoot.1 <- left_join(webfoot.1, price, by = c('category'='category','year'='year', 'month'='month'))
webfoot.1[is.na(webfoot.1)] <- 0
webfoot.2 <- subset(webfoot.1, select=c(panel_code, year, month, price, Ed, w))
webfoot.2<- rename(webfoot.2, "pweb" = "price", "xweb" = "Ed", "wweb" = "w")

### 고등어(10549)  ----------------------------------------------------------------------
mackerel <- protein.w %>% filter(category == 'mack')
mackerel.1 <- left_join(id, mackerel, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
mackerel.1$category[is.na(mackerel.1$category)] <- 'mack'
mackerel.1 <- left_join(mackerel.1, price, by = c('category'='category','year'='year', 'month'='month'))
mackerel.1[is.na(mackerel.1)] <- 0
mackerel.2 <- subset(mackerel.1, select=c(panel_code, year, month, price, Ed, w))
mackerel.2<- rename(mackerel.2, "pmack" = "price", "xmack" = "Ed", "wmack" = "w")

### 조기 (3118) ----------------------------------------------------------------------
jogi <- protein.w %>% filter(category == 'jogi')
jogi.1 <- left_join(id, jogi, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
jogi.1$category[is.na(jogi.1$category)] <- 'jogi'
jogi.1 <- left_join(jogi.1, price, by = c('category'='category','year'='year', 'month'='month'))
jogi.1[is.na(jogi.1)] <- 0
jogi.2 <- subset(jogi.1, select=c(panel_code, year, month, price, Ed, w))
jogi.2<- rename(jogi.2, "pjogi" = "price", "xjogi" = "Ed", "wjogi" = "w")

### 명태 (5437)  ----------------------------------------------------------------------
myung <- protein.w %>% filter(category == 'myung')
myung.1 <- left_join(id, myung, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
myung.1$category[is.na(myung.1$category)] <- 'myung'
myung.1 <- left_join(myung.1, price, by = c('category'='category','year'='year', 'month'='month'))
myung.1[is.na(myung.1)] <- 0
myung.2 <- subset(myung.1, select=c(panel_code, year, month, price, Ed, w))
myung.2<- rename(myung.2, "pmyung" = "price", "xmyung" = "Ed", "wmyung" = "w")

### 갈치 (6104)  ----------------------------------------------------------------------
gal <- protein.w %>% filter(category == 'gal')
gal.1 <- left_join(id, gal, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
gal.1$category[is.na(gal.1$category)] <- 'gal'
gal.1 <- left_join(gal.1, price, by = c('category'='category','year'='year', 'month'='month'))
gal.1[is.na(gal.1)] <- 0
gal.2 <- subset(gal.1, select=c(panel_code, year, month, price, Ed, w))
gal.2<- rename(gal.2, "pgal" = "price", "xgal" = "Ed", "wgal" = "w")

### 두부(36528) ----------------------------------------------------------------------
tofu <- protein.w %>% filter(category == 'tofu')
tofu.1 <- left_join(id, tofu, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
tofu.1$category[is.na(tofu.1$category)] <- 'tofu'
tofu.1 <- left_join(tofu.1, price, by = c('category'='category','year'='year', 'month'='month'))
tofu.1[is.na(tofu.1)] <- 0
tofu.2 <- subset(tofu.1, select=c(panel_code, year, month, price, Ed, w))
tofu.2<- rename(tofu.2, "ptofu" = "price", "xtofu" = "Ed", "wtofu" = "w")

### 콩 (3827)  ----------------------------------------------------------------------
bean <- protein.w %>% filter(category == 'bean')
bean.1 <- left_join(id, bean, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
bean.1$category[is.na(bean.1$category)] <- 'bean'
bean.1 <- left_join(bean.1, price, by = c('category'='category','year'='year', 'month'='month'))
bean.1[is.na(bean.1)] <- 0
bean.2 <- subset(bean.1, select=c(panel_code, year, month, price, Ed, w))
bean.2<- rename(bean.2, "pbean" = "price", "xbean" = "Ed", "wbean" = "w")

# dataset -----------------------------------------------------------------
aids.1 <- left_join(bean.2, dbeef.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.2 <- left_join(aids.1, fbeef.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.3 <- left_join(aids.2, chicken.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.4 <- left_join(aids.3, pork.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.5 <- left_join(aids.4, abalone.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.6 <- left_join(aids.5, oyster.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.7 <- left_join(aids.6, clam.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.8 <- left_join(aids.7, crab.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.9 <- left_join(aids.8, squid.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.10 <- left_join(aids.9, webfoot.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.11 <- left_join(aids.10, mackerel.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.12 <- left_join(aids.11, jogi.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.13 <- left_join(aids.12, myung.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.14 <- left_join(aids.13, gal.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.15 <- left_join(aids.14, tofu.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.16 <- left_join(aids.15, shrimp.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))

rm(abalone, abalone.1, abalone.2, bean, bean.1, bean.2, chicken, chicken.1, chicken.2, clam, clam.1, clam.2, crab, crab.1, crab.2, dbeef, dbeef.1, dbeef.2, fbeef, fbeef.1, fbeef.2, gal, gal.1, gal.2, jogi, jogi.1, jogi.2, mackerel,
   mackerel.1, mackerel.2, myung, myung.1, myung.2, oyster, oyster.1, oyster.2, pork, pork.1, pork.2, squid, squid.1, squid.2, tofu, tofu.1, tofu.2, webfoot, webfoot.1, webfoot.2, shrimp, shrimp.1, shrimp.2)

aids <- aids.16 %>% 
  mutate(E = xdbeef + xfbeef + xchicken + xpork + xabal + xoys + xjoge + xcrab + xshrimp + xsquid + xweb + xmack + xjogi + xmyung + xgal + xtofu + xbean) %>% 
  mutate(xbeef = xdbeef + xfbeef) %>% 
  mutate(xclam = xabal + xoys + xjoge) %>% mutate(xcephal = xsquid + xweb) %>% mutate(xfish = xmack + xjogi + xmyung + xgal) %>% mutate(xcrus = xcrab + xshrimp) %>%
  mutate(xmeat = xbeef + xpork + xchicken) %>% mutate(xsea = xclam + xcephal + xfish + xcrus) %>% mutate(xplant = xtofu + xbean) %>% 
  mutate(w2dbeef = xdbeef/xmeat) %>% mutate(w2fbeef = xfbeef/xmeat) %>%
  mutate(w2chicken = xchicken/xmeat) %>% mutate(w2pork = xpork/xmeat) %>%
  mutate(w3dbeef = xdbeef/xbeef) %>% mutate(w3fbeef = xfbeef/xbeef) %>%
  mutate(w2abal = xabal/xclam) %>% mutate(w2oys = xoys/xclam) %>% mutate(w2joge = xjoge/xclam) %>% 
  mutate(w2squid = xsquid/xcephal) %>% mutate(w2web = xweb/xcephal) %>% 
  mutate(w2mack = xmack/xfish) %>% mutate(w2jogi = xjogi/xfish) %>% mutate(w2myung = xmyung/xfish) %>% mutate(w2gal = xgal/xfish) %>%
  mutate(w2crab = xcrab/xcrus) %>% mutate(w2shrimp = xshrimp/xcrus) %>%
  mutate(w3abal = xabal/xsea) %>% mutate(w3oys = xoys/xsea) %>% mutate(w3joge = xjoge/xsea) %>% mutate(w3squid = xsquid/xsea) %>% mutate(w3web = xweb/xsea) %>% mutate(w3crab = xcrab/xsea) %>% mutate(w3shrimp = xshrimp/xsea) %>%
  mutate(w3mack = xmack/xsea) %>% mutate(w3jogi = xjogi/xsea) %>% mutate(w3myung = xmyung/xsea) %>% mutate(w3gal = xgal/xsea) %>% 
  mutate(w4tofu = xtofu/xplant) %>% mutate(w4bean = xbean/xplant)

rm(aids.1, aids.2, aids.3, aids.4, aids.5, aids.6, aids.7, aids.8, aids.9, aids.10, aids.11, aids.12, aids.13, aids.14, aids.15, aids.16)

summary(aids)
aids[is.na(aids)] <- 0

#load("protein_demo.RData")

#aids <- replace(aids, is.na(aids), 0)

#aids <- inner_join(aids, protein.demo, by =c('panel_code' = 'panel_code'))

#aids_demo <- unique(aids %>% ungroup %>% select(panel_code)) #833

#rm(id, id_demo, protein, protein.w)

# Stone price index 도출하기(check; 각 패널들마다 직면하는 stone price index는 다름)------------------------------------------------------
aids.s <- as.data.frame(aids %>% 
  mutate(lndbeef = (log(pdbeef))) %>% mutate(lnfbeef = (log(pfbeef))) %>% mutate(lnchicken = (log(pchicken))) %>% mutate(lnpork = (log(ppork))) %>%
  mutate(lnabal = (log(pabal))) %>% mutate(lnoys = (log(poys))) %>% mutate(lnjoge = (log(pjoge))) %>% mutate(lncrab = (log(pcrab))) %>% mutate(lnshrimp = (log(pshrimp))) %>% mutate(lnsquid = (log(psquid))) %>% mutate(lnweb = (log(pweb))) %>%
  mutate(lnmack = (log(pmack))) %>% mutate(lnjogi = (log(pjogi))) %>% mutate(lnmyung = (log(pmyung))) %>% mutate(lngal = (log(pgal))) %>%
  mutate(lntofu = (log(ptofu))) %>% mutate(lnbean = (log(pbean))))

plant_s <- as.data.frame(aidsPx("S", c("pbean", "ptofu"), aids, c("w4bean", "w4tofu")))
names(plant_s)[1] <- c("lnplant")

seafood_s <- as.data.frame(aidsPx("S", c("pabal", "poys", "pjoge", "pcrab", "pshrimp", "psquid", "pweb", "pmack", "pjogi", "pmyung", "pgal"),
                                  aids, c("w3abal", "w3oys", "w3joge", "w3crab", "w3shrimp", "w3squid", "w3web", "w3mack", "w3jogi", "w3myung", "w3gal")))
names(seafood_s)[1] <- c("lnsea")

meat_s <- as.data.frame(aidsPx("S", c("pdbeef", "pfbeef", "pchicken", "ppork"),
                               aids, c("w2dbeef", "w2fbeef", "w2chicken", "w2pork")))
names(meat_s)[1] <- c("lnmeat")

clam_s <- as.data.frame(aidsPx("S", c("pabal", "poys", "pjoge"),
                               aids, c("w2abal", "w2oys", "w2joge")))
names(clam_s)[1] <- c("lnclam")

cephalopod_s <- as.data.frame(aidsPx("S", c("psquid", "pweb"),
                                     aids, c("w2squid", "w2web")))
names(cephalopod_s)[1] <- c("lncephal")

fish_s <- as.data.frame(aidsPx("S", c("pmack", "pjogi", "pmyung", "pgal"),
                               aids, c("w2mack", "w2jogi", "w2myung", "w2gal")))
names(fish_s)[1] <- c("lnfish")

crus_s <- as.data.frame(aidsPx("S", c("pcrab", "pshrimp"),
                               aids, c("w2crab", "w2shrimp")))
names(crus_s)[1] <- c("lncrus")

beef_s <- as.data.frame(aidsPx("S", c("pdbeef", "pfbeef"),
                               aids, c("w3dbeef", "w3fbeef")))
names(beef_s)[1] <- c("lnbeef")

aids <- as.data.frame(cbind(aids.s, plant_s, seafood_s, clam_s, cephalopod_s, fish_s, crus_s, beef_s, meat_s))
summary(aids)
aids_demo <- unique(aids %>% ungroup %>% select(panel_code)) ##835

#write.csv(aids, "aids_w_check.csv")

# 로그 가격을 다시 변환하기 ----------------------------------------------------------
aids <- aids %>% mutate(pplant = exp(lnplant)) %>% mutate(pbeef = exp(lnbeef)) %>% mutate(pmeat = exp(lnmeat)) %>% 
  mutate(psea = exp(lnsea))%>% mutate(pclam = exp(lnclam))%>% mutate(pcephal = exp(lncephal)) %>% mutate(pfish = exp(lnfish)) %>% mutate(pcrus = exp(lncrus)) 

summary(aids)
aids[is.na(aids)] <- 0

# 필요한 변수만 추출하기  -----------------------------------------------------------
data <- as.data.frame(subset(aids, select=c(panel_code, year, month, E, 
                                  wbean, wtofu, wdbeef, wfbeef, wpork, wchicken, wabal, woys, wjoge, wsquid, wweb, wmack, wjogi, wmyung, wgal, wcrab, wshrimp,
                                  lnbean, lntofu, lndbeef, lnfbeef, lnpork, lnchicken, lnabal, lnoys, lnjoge, lncrab, lnshrimp, lnsquid, lnweb, lnmack, lnjogi, lnmyung, lngal, 
                                  lnbeef, lnfish, lncephal, lnclam, lncrus, 
                                  lnmeat, lnsea, lnplant,
                                  pbean, ptofu, pdbeef, pfbeef, ppork, pchicken, pabal, poys, pjoge, psquid, pweb, pmack, pjogi, pmyung, pgal, pcrab, pshrimp, 
                                  pbeef, pfish, pcephal, pclam, pcrus,
                                  pmeat, psea, pplant,
                                  xbean, xtofu, xdbeef, xfbeef, xpork, xchicken, xabal, xoys, xjoge, xsquid, xweb, xmack, xjogi, xmyung, xgal, xcrab, xshrimp,
                                  xbeef, xfish, xcephal, xclam, xcrus)))
                        
                        
                        
data <- data %>%
  mutate(Emeat = xbeef + xpork + xchicken) %>%
  mutate(Esea = xfish + xcephal + xclam + xcrus) %>%
  mutate(Eplant = xtofu + xbean) %>%
  mutate(wmeat = wdbeef + wfbeef + wpork + wchicken) %>%
  mutate(wsea = wabal + woys + wjoge + wsquid + wweb + wmack + wjogi + wmyung + wgal + wcrab + wshrimp) %>%
  mutate(wplant = wtofu + wbean) %>%
  mutate(wbeef = wdbeef + wfbeef) %>%
  mutate(w2beef = xbeef/Emeat) %>% mutate(w2pork = xpork/Emeat) %>% mutate(w2chicken = xchicken/Emeat) %>%
  mutate(w2fish = (xmack + xjogi + xmyung + xgal)/Esea) %>% mutate(w2cephal = (xsquid + xweb)/Esea)  %>% mutate(w2clam = (xabal + xoys + xjoge)/Esea) %>% mutate(w2crus = (xcrab + xshrimp)/Esea)

#data$income <- as.numeric(data$income)
summary(data)
data[is.na(data)] <- 0

#write.csv(data, "protein_without_demo_final")
rm(aids, aids_demo, aids.s, beef_s, cephalopod_s, clam_s, fish_s, crus_s, id, id_demo, meat_s, plant_s, price, protein, protein.w, seafood_s, total.835)

summary(data)

# sociodemographic 변수(protein.demo) ---------------------------------------
#load("protein_demo.RData")

# age 와 income 변수 scaling -------------------------------------------------
protein.demo <- transform(protein.demo,
                  incomeS = ifelse(income <= 200, "1",
                                   ifelse(income > 200 & income <= 400, "2",
                                          ifelse(income > 400 & income <= 600, "3",
                                                 ifelse(income > 600 & income <= 800, "4", "5")))))

protein.demo$incomeS <- as.numeric(protein.demo$incomeS)
quantile(protein.demo$incomeS)

protein.demo <- transform(protein.demo,
                  ageS = ifelse(age <= 30, "1",
                                ifelse(age > 30 & age <= 40, "2",
                                       ifelse(age > 40 & age <= 50, "3",
                                              ifelse(age > 50 & age <= 60, "4",
                                                     ifelse(age > 60 & age <= 70, "5", "6"))))))


protein.demo$ageS <- as.numeric(protein.demo$ageS)
quantile(protein.demo$ageS)


# region 더미 생성하기 ----------------------------------------------------------
library(fastDummies)
protein.demo <- dummy_cols(.data = protein.demo, select_columns = c("region"), remove_first_dummy = FALSE)

summary(protein.demo)
# 서베이 데이터 불러오기 ----------------------------------------------------------------
bmi <- read.csv("bmi.csv")
bmi_code <- read.csv("panel_code.csv")
survey <- read.csv("survey.csv")

# 각 데이터별 패널 뽑아내기 ----------------------------------------------------------
data.panel <- unique(protein.demo %>% select(panel_code))

bmi <- inner_join(bmi, bmi_code)
bmi <- bmi %>% select(panel_code, BMI)
bmi.panel <- unique(bmi %>% select(panel_code)) ##1528

survey <- survey[-c(378, 391, 400, 424, 452, 462, 778, 1142, 1154, 1169, 1206),]
rownames(survey) <- NULL
survey <- subset(survey, select = c(panel_code, HC_1, HC_2, HC_3, HC_4, HC_5, HC_6, HC_7, HC_8))
names(survey) <- c("panel_code", "HC1", "HC2", "HC3", "HC4", "HC5","HC6", "HC7", "HC8")
survey <- na.omit(survey)
survey.panel <- unique(survey %>% select(panel_code)) ##1228

# 공통 패널 뽑아내기 (657 가구)--------------------------------------------------------------
panel1 <- inner_join(data.panel, bmi.panel) #764
panel <- inner_join(panel1, survey.panel) #657

# 공통 패널들의 데이터 추출하기 --------------------------------------------------------
co.data <- inner_join(protein.demo, panel, by=c('panel_code' = 'panel_code'))

co.bmi <- inner_join(bmi, panel) 
bmi_occur <- data.frame(table(co.bmi$panel_code))
bmi_occur[bmi_occur$Freq >1,] #658
co.bmi = co.bmi[-which(duplicated(co.bmi$panel_code)),] #657

co.survey <- inner_join(survey, panel)
survey_occur <- data.frame(table(co.survey$panel_code))
survey_occur[survey_occur$Freq >1,]
co.survey = co.survey[-which(duplicated(co.survey$panel_code)),] #657

write.csv(co.survey, "co.survey.csv")
co.survey <- read.csv("co_survey_edit.csv")

# 데이터 통합하기 ----------------------------------------------------------------
co1 <- inner_join(co.data, co.bmi) #657
co <- inner_join(co1, co.survey, by=c('panel_code'='panel_code')) #634
co.panel <- unique(co %>% select(panel_code)) #634

co <- co %>% mutate(HC = (HC1+HC2+HC3+HC4+HC5+HC6+HC7+HC8)/8)

demo = subset(co, select=-X)

#write.csv(demo, "demo_634_final.csv")

# 지출비중 데이터랑 데모 데이터 합치기 (data, demo) #35617건----------------------------------------------------
final <- inner_join(data, demo)
final_demo <- unique(final %>% ungroup() %>% select(panel_code, HC1, HC2, HC3, HC4, HC5, HC6, HC7, HC8, income, family_num, child_index, age1, age, incomeS, ageS, BMI,
                                                    region_강원, region_경기, region_경남, region_광주, region_대구, region_대전, region_부산, region_서울, region_세종, region_울산, region_인천, region_전남, region_전북, region_제주, region_충남, region_충북)) #634

write.csv(final_demo, "final_protein_demo_634.csv")
write.csv(final, "final_protein_634_35617.csv")

sea0 <- subset(final, Esea > 0) #21717
sea0_demo <- unique(sea0 %>% ungroup() %>% select(panel_code, HC1, HC2, HC3, HC4, HC5, HC6, HC7, HC8, income, family_num, child_index, age1, age, incomeS, ageS, BMI,
                                                  region_강원, region_경기, region_경남, region_광주, region_대구, region_대전, region_부산, region_서울, region_세종, region_울산, region_인천, region_전남, region_전북, region_제주, region_충남, region_충북)) #631

write.csv(sea0_demo, "final_seafood_demo_631.csv")
write.csv(sea0, "final_seafood_631_21717.csv")

meat0 <- subset(final, Emeat > 0) #32078
meat0_demo <- unique(meat0 %>% ungroup() %>% select(panel_code, HC1, HC2, HC3, HC4, HC5, HC6, HC7, HC8, income, family_num, child_index, age1, age, incomeS, ageS, BMI,
                                                    region_강원, region_경기, region_경남, region_광주, region_대구, region_대전, region_부산, region_서울, region_세종, region_울산, region_인천, region_전남, region_전북, region_제주, region_충남, region_충북)) #634

write.csv(meat0, "final_meat_634_32078.csv")
write.csv(meat0_demo, "final_meat_demo_634.csv")

# HC pls 결과와 merge하기 ------------------------------------------------------
hc634 <- read.csv(file = "final_protein_demo_634_pls.csv")
protein634 <- inner_join(final, hc634)
meat634 <- inner_join(meat0, hc634)

hc631 <- read.csv(file = "final_seafood_demo_631_pls.csv")
sea631 <- inner_join(sea0, hc631)

# 시간 변수와 BMI 카테고리 변수 추가하기 -------------------------------------------------
#시간변수
protein634$t <- (protein634$year-2015)*12+protein634$month
protein634$co1 <- cos(2/3*3.14159*protein634$t)
protein634$si1 <- sin(2/3*3.14159*protein634$t)

p.t <- unique(protein634 %>% select(t, year, month))
protein634 <- dummy_cols(.data = protein634, select_columns = c("t"), remove_first_dummy = FALSE)

meat634$t <- (meat634$year-2015)*12+meat634$month
meat634$co1 <- cos(2/3*3.14159*meat634$t)
meat634$si1 <- sin(2/3*3.14159*meat634$t)

m.t <- unique(meat634 %>% select(t, year, month))
meat634 <- dummy_cols(.data = meat634, select_columns = c("t"), remove_first_dummy = FALSE)

sea631$t <- (sea631$year-2015)*12+sea631$month
sea631$co1 <- cos(2/3*3.14159*sea631$t)
sea631$si1 <- sin(2/3*3.14159*sea631$t)

s.t <- unique(sea631 %>% select(t, year, month))
sea631 <- dummy_cols(.data = sea631, select_columns = c("t"), remove_first_dummy = FALSE)

#bmiS
protein634 <- transform(protein634,
                                bmiS = ifelse(BMI < 18.5, "1",
                                              ifelse(BMI >= 18.5 & BMI < 25, "2",
                                                     ifelse(BMI >= 25 & BMI < 30, "3", "4"))))

write_xlsx(protein634, path = "protein634.xlsx")

meat634 <- transform(meat634,
                        bmiS = ifelse(BMI < 18.5, "1",
                                      ifelse(BMI >= 18.5 & BMI < 25, "2",
                                             ifelse(BMI >= 25 & BMI < 30, "3", "4"))))

write_xlsx(meat634, path = "meat634.xlsx")

sea631 <- transform(sea631,
                     bmiS = ifelse(BMI < 18.5, "1",
                                   ifelse(BMI >= 18.5 & BMI < 25, "2",
                                          ifelse(BMI >= 25 & BMI < 30, "3", "4"))))

write_xlsx(sea631, path = "sea631.xlsx")

# Best A0 찾기 --------------------------------------------------------------
#meat, seafood, plant(250)
priceNames <- c("pmeat", "psea", "pplant")
shareNames <- c("wmeat", "wsea", "wplant")

aidsBestA0(priceNames, shareNames, "E", data = protein634, a0min = -50, a0max = 50, stoprange = 3, stopiter = 5)

#beef, pork, chicken(75)
priceNames <- c("pbeef", "ppork", "pchicken")
shareNames <- c("w2beef", "w2pork", "w2chicken")

aidsBestA0(priceNames, shareNames, "Emeat", data = meat634, a0min = -50, a0max = 50, stoprange = 3, stopiter = 10)

#fish, cephal, shell(55)
priceNames <- c("pfish", "pcephal", "pclam", "pcrus")
shareNames <- c("w2fish", "w2cephal", "w2clam", "w2crus")

aidsBestA0(priceNames, shareNames, "Esea", data = sea631, a0min = -50, a0max = 50, stoprange = 3, stopiter = 10)
