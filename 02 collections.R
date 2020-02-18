collections <- read.csv('collections.csv')

pays_col <- oscn_pays_raw %>% 
  filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"))

pays_col <- pays_col %>% 
  filter(pay_amt < 2000)

pays_col2 <- odcr_pays_raw %>% 
  filter(!str_detect(pay_desc, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"))

pays_col2 <- pays_col2 %>% 
  filter(pay_amt < 2000)

pays_col2 <- pays_col2 %>% 
  mutate(fy = date_to_fy(pay_date))

pays_col <- pays_col %>% 
  mutate(fy = date_to_fy(pay_date))

pays_col <- pays_col %>% 
  group_by(fy) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE))

pays_col2 <- pays_col2 %>% 
  group_by(fy) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE))

pays_fy <- pays_col %>% 
  bind_rows(pays_col2)

pays_fy <- pays_fy %>% 
  group_by(year = fy) %>% 
  summarize(pays = sum(pays, na.rm = TRUE))

pays_fy <- pays_fy %>% 
  filter(year != 2011 & year != 2020 & year != 2019)

####summarize collectios ####
collections <- read.csv('collections.csv')

collections <- collections %>% 
  group_by(year) %>% 
  summarize(collected = sum(fee, na.rm = TRUE))

total_collections <- collections %>% 
  left_join(pays_fy)

total_collections <- total_collections %>% 
  mutate(remaining = collected - pays)

total_collections <- total_collections %>% 
  group_by(year) %>% 
  mutate(perc = pays/sum(collected) * 100)

total_collections <- total_collections %>% 
  group_by(year) %>% 
  mutate(perc_remaining = 100-perc)

#### OIDS ####
a <- oscn_fees_raw
b <- odcr_fees_raw

a$min_year <- str_sub(a$min_date, 1, 4) %>% 
  as.numeric()

b$min_year <- str_sub(b$min_date, 1, 4) %>% 
  as.numeric()

a<- a %>%       #filters out two rows
  filter(fee_amt < 200000)


a <- a %>% 
  mutate(oids = str_detect(min_desc, "IDS |INDIG|OIDS ") & 
           !str_detect(min_desc, "FLUIDS |FULIDS "))

oids_fees <- a %>% 
  filter(oids == TRUE)

oids_fees <- oids_fees %>% 
  mutate(fy = date_to_fy(min_date)) %>% 
  filter(fy != 2014 & fy != 2019 & fy != 2020)


oids_fees2 <- oids_fees %>% 
  group_by(fy) %>% 
  summarize(assessed = sum(fee_amt, no.rm = TRUE))

#### odcr oids####
b <- b %>% 
  mutate(oids = str_detect(min_desc, "IDS |INDIG|OIDS ") & 
           !str_detect(min_desc, "FLUIDS |FULIDS "))


b <- b %>% 
  filter(oids == TRUE)


b <- b %>% 
  mutate(fy = date_to_fy(min_date)) %>% 
  filter(fy != 2014 & fy != 2019 & fy != 2020)

b <- b %>% 
  group_by(fy) %>% 
  summarize(assessed = sum(fee_amt, no.rm = TRUE))

oids_fees2 <- b %>%
  bind_rows(oids_fees2)

oids_fees2 <- oids_fees2 %>% 
  group_by(fy) %>% 
  summarize(assessed = sum(assessed, no.rm = TRUE))

collections2 <- read.csv('collections.csv')

collections2 <- collections2 %>% 
  mutate(oids = str_detect(type, "Indigent")) %>% 
  filter(oids == TRUE)
  
collections2 <- collections2 %>% 
  group_by(fy = year) %>% 
  summarize(collected = sum(fee, na.rm = TRUE))

total_oids <- oids_fees2 %>% 
  left_join(collections2)

total_oids <- total_oids %>% 
  mutate(outstanding = assessed - collected)

total_oids <- total_oids %>%
  group_by(fy) %>% 
  mutate(perc = collected/sum(assessed) * 100)

####AOC parsed collections ####
connect_ojo()
d <- dbReadTable(ojo_db, "aoc_collections")
disconnect_ojo()

d <- d %>% 
  mutate(casetype = str_to_upper(casetype))

d <- d %>% 
  group_by(quarter, casetype) %>% 
  summarize(coll_amt = sum(coll_amt, na.rm = TRUE, digits = 2))

d <- d %>% 
  group_by(quarter) %>% 
  mutate(perc = coll_amt/sum(coll_amt) * 100)

d$casetype_new <- case_when(d$casetype == "FELONY" ~ "FELONY",
                              d$casetype == "MISDEMEANOR" ~ "MISDEMEANOR",
                              TRUE ~ "CIVIL, SMALL CLAIMS, TRAFFIC, OTHER")

d2 <- d %>%
  group_by(casetype_new, quarter) %>% 
  summarize(coll_amt = sum(coll_amt, na.rm = TRUE, digits = 2))

d2 <- d2 %>% 
  group_by(quarter) %>% 
  mutate(perc = coll_amt/sum(coll_amt) * 100)

d2$perc <- round(d2$perc, 2)

d2 <- d2 %>% 
  group_by(quarter, casetype_new) %>% 
  arrange(desc(perc))

d3 <- d2 %>% 
  group_by(casetype_new) %>% 
  summarize(coll_amt = sum(coll_amt, na.rm = TRUE, digits = 2))

d3 <- d3 %>% 
  mutate(perc = coll_amt/sum(coll_amt) * 100)

d3$perc <- round(d3$perc, 2)

write.csv(d3, file = "district court collections.csv")
