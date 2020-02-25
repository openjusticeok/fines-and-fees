library(ojo)
options(scipen = 999)
#CASA, AFIS, CLEET, OIDS, Sheriff
collections <- read.csv('collections.csv')

collections %>% 
  group_by(year) %>% 
  count(type)

collections <- collections %>% 
  mutate(type = str_to_upper(type))

collections <- collections %>% 
  mutate(afis = str_detect(type, "FINGERPRINT"))

collections <- collections %>% 
  mutate(oids = str_detect(type, "INDIG"))

collections <- collections %>% 
  mutate(cleet = str_detect(type, "LAW ENF"))

collections <- collections %>% 
  mutate(casa = str_detect(type, "COURT APP"))

collections <- collections %>% 
  mutate(sheriff = str_detect(type, "SHERIFF"))

collections <- collections %>% 
  mutate(security = str_detect(type, "SHERIFF") & 
         str_detect(type, "SEC"))

collections <- collections %>% 
  mutate(jailfee = str_detect(type, "SHERIFF") & 
           str_detect(type, "JAIL"))

collections <- collections %>% 
  mutate(sheriff_fees = str_detect(type, "SHERIFF") & 
           str_detect(type, "FEES CIV"))

collections <- collections %>% 
  mutate(drugtest = str_detect(type, "SHERIFF") & 
           str_detect(type, "DRUG"))

collections <- collections %>% 
  mutate(erf = str_detect(type, "SHERIFF") & 
           str_detect(type, "ENVI"))

collections <- collections %>% 
  filter(afis == TRUE| oids == TRUE| cleet == TRUE| casa == TRUE| sheriff == TRUE)

total <- collections %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))
#### summarize####
oids <- collections %>% 
  filter(oids == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

cleet <- collections %>% 
  filter(cleet == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

afis <- collections %>% 
  filter(afis == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

casa <- collections %>% 
  filter(casa == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_all <- collections %>% 
  filter(sheriff == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_security <- collections %>% 
  filter(security == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_jailfee <- collections %>% 
  filter(jailfee == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_fees <- collections %>% 
  filter(sheriff_fees == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_drugtest <- collections %>% 
  filter(drugtest == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

sheriff_erf <- collections %>% 
  filter(erf == TRUE) %>% 
  group_by(year) %>% 
  summarize(fees = sum(fee, na.rm = TRUE))

#### ####
connect_ojo()
d <- dbReadTable(ojo_db, "aoc_collections")
disconnect_ojo()

d <- d %>% 
  mutate(casetype = (str_to_upper(casetype)))

d <- d %>% 
  filter(casetype == "FELONY" | casetype == "MISDEMEANOR")

sum <- d %>% 
  summarize(fees = sum(coll_amt, na.rm = TRUE))
