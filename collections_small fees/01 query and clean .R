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
