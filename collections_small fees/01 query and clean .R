library(ojodb)
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

collections$feetype <- case_when(collections$afis == TRUE ~ "AFIS",
                                 collections$oids == TRUE ~ "OIDS",
                                 collections$cleet == TRUE ~ "CLEET",
                                 collections$casa == TRUE ~ "CASA",
                                 collections$sheriff == TRUE ~ "SHERIFF",
                                 TRUE ~ "OTHER")

smallfees <- collections %>% 
  filter(feetype != "OTHER")

total_sm <- smallfees %>% 
  group_by(year, feetype) %>% 
  summarize(fees = sum(fee, na.rm = TRUE, digits = 2))


####Checking data using aoc_collections ####
connect_ojo()
d <- dbReadTable(ojo_db, "aoc_collections")
disconnect_ojo()

d <- d %>% 
  mutate(casetype = (str_to_upper(casetype)))


sum <- d %>% 
  summarize(fees = sum(coll_amt, na.rm = TRUE))

sum(d$coll_amt)
