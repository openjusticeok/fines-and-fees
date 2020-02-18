library(ojo)
options(scipen = 999)

odcr_show("ATOKA", "CM", 2018, 22)

#### OSCN ####
oscn_fees <- ojo_query_mins(oscn_counties, c("CF", "CM"), 2008:2019, min_years = 2011:2018, min_type = "fees")
oscn_fees_raw <- oscn_fees
oscn_pays <- ojo_query_pays(oscn_counties, c("CF", "CM"), 2008:2019, pay_years = 2011:2018)
oscn_pays_raw <- oscn_pays
#### ODCR ####
odcr_fees <- ojo_query_mins(odcr_counties, c("CF", "CM"), 2008:2019, min_years = 2011:2018, min_type = "fees")
odcr_fees_raw <- odcr_fees
odcr_pays <- ojo_query_pays(odcr_counties, c("CF", "CM"), 2008:2019, pay_years = 2011:2018)
odcr_pays_raw <- odcr_pays

####Cleaning OSCN####
oscn_fees$min_year <- str_sub(oscn_fees$min_date, 1, 4) %>% 
  as.numeric()

oscn_fees<- oscn_fees %>% 
  filter(min_year != 2011)

oscn_fees<- oscn_fees %>%       #filters out two rows
  filter(fee_amt < 200000)

oscn_fees <- oscn_fees %>% 
  filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED"))

oscn_pays$pay_year <- str_sub(oscn_pays$pay_date, 1, 4) %>% 
  as.numeric()

oscn_pays<- oscn_pays %>% 
  filter(pay_year != 2011)

oscn_pays <- oscn_pays %>% 
  filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"))

oscn_pays <- oscn_pays %>% 
  filter(pay_amt < 2000)

oscn_fees <- oscn_fees %>% 
  mutate(mo = floor_date(ymd(min_date), "month"))

oscn_pays <- oscn_pays %>% 
  mutate(mo = floor_date(ymd(pay_date), "month")) 

oscn_pays$year <- str_sub(oscn_pays$mo, 1, 4) %>% 
  as.numeric()

oscn_fees2 <- oscn_fees %>% 
  group_by(mo) %>% 
  summarize(fees = sum(fee_amt, na.rm = TRUE))

oscn_pays2 <- oscn_pays %>% 
  group_by(mo) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE))

#graphing to check
ggplot(oscn_fees2, aes(mo, fees), color = "black") + 
  geom_line() +
  geom_line(data=oscn_pays2, aes(mo, pays), color = "red") +
  theme_ojo() + 
  ylim(0, NA)

####Cleaning ODCR####
odcr_fees$min_year <- str_sub(odcr_fees$min_date, 1, 4) %>% 
  as.numeric()

odcr_fees<- odcr_fees %>% 
  filter(min_year != 2011)

odcr_fees<- odcr_fees %>%
  filter(fee_amt < 200000)

odcr_fees <- odcr_fees %>% 
  filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED"))

odcr_pays <- odcr_pays %>% 
  filter(!str_detect(pay_desc, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"))

odcr_pays <- odcr_pays %>% 
  filter(pay_amt < 2000)

odcr_fees <- odcr_fees %>% 
  mutate(mo = floor_date(ymd(min_date), "month"))

odcr_pays$pay_year <- str_sub(odcr_pays$pay_date, 1, 4) %>% 
  as.numeric()

odcr_pays<- odcr_pays %>% 
  filter(pay_year != 2011)

odcr_pays <- odcr_pays %>% 
  mutate(mo = floor_date(ymd(pay_date), "month")) 

odcr_pays$year <- str_sub(odcr_pays$mo, 1, 4) %>% 
  as.numeric()

odcr_fees2 <- odcr_fees %>% 
  group_by(mo) %>% 
  summarize(fees = sum(fee_amt, na.rm = TRUE))

odcr_pays2 <- odcr_pays %>% 
  group_by(mo) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE))

#graphing to check
ggplot(odcr_fees2, aes(mo, fees), color = "black") + 
  geom_line() +
  geom_line(data=odcr_pays2, aes(mo, pays), color = "red") +
  theme_ojo() + 
  ylim(0, NA)

####Combining ODCR & OSCN: floor_date sums####
total_fees <- rbind(odcr_fees2, oscn_fees2)
total_pays <- rbind(odcr_pays2, oscn_pays2)

total_fees <- total_fees %>%                       #resummarize to combine odcr & oscn floor_dates
  group_by(mo) %>% 
  summarize(fees = sum(fees, na.rm = TRUE))

total_pays<- total_pays %>% 
  group_by(mo) %>% 
  summarize(pays = sum(pays, na.rm = TRUE))

#graphing to check
ggplot(total_fees, aes(mo, fees), color = "black") + 
  geom_line() +
  geom_line(data=total_pays, aes(mo, pays), color = "red") +
  theme_ojo() + 
  ylim(0, NA)

#### Tax withholdings ####
oscn_pays <- oscn_pays %>%
  mutate(tax = str_detect(pay_acct, "OTC INTERCEPTS"))

oscn_pays_tax <- oscn_pays %>% 
  group_by(mo, tax) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE)) %>% 
  ungroup()

odcr_pays <- odcr_pays %>%
  mutate(tax = str_detect(pay_desc, "TAX"))

odcr_pays_tax <- odcr_pays %>% 
  group_by(mo, tax) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE)) %>% 
  ungroup()

total_tax <- rbind(odcr_pays_tax, oscn_pays_tax)

total_tax <- total_tax %>%
  group_by(mo, tax) %>% 
  summarize(pays = sum(pays, na.rm = TRUE))

#checking
ggplot(total_tax, aes(mo, pays, color = tax)) + 
  geom_line() +
  theme_ojo() + 
  ylim(0, NA)

#### tax season only ####
odcr_pays_tax2 <- odcr_pays %>% 
  filter(pay_date >= ("2012-02-01") & pay_date <= ("2012-04-15") | pay_date >= ("2013-02-01") & pay_date <= ("2013-04-15")| pay_date >= ("2014-02-01") & pay_date <= ("2014-04-15") | pay_date >= ("2015-02-01") & pay_date <= ("2015-04-15") | pay_date >= ("2016-02-01") & pay_date <= ("2016-04-15") | pay_date >= ("2017-02-01") & pay_date <= ("2017-04-15") | pay_date >= ("2018-02-01") & pay_date <= ("2018-04-15"))

oscn_pays_tax2 <- oscn_pays %>% 
  filter(pay_date >= ("2012-02-01") & pay_date <= ("2012-04-15") | pay_date >= ("2013-02-01") & pay_date <= ("2013-04-15")| pay_date >= ("2014-02-01") & pay_date <= ("2014-04-15") | pay_date >= ("2015-02-01") & pay_date <= ("2015-04-15") | pay_date >= ("2016-02-01") & pay_date <= ("2016-04-15") | pay_date >= ("2017-02-01") & pay_date <= ("2017-04-15") | pay_date >= ("2018-02-01") & pay_date <= ("2018-04-15"))

odcr_pays_tax2 <- odcr_pays_tax2 %>% 
  group_by(year, tax) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE)) %>% 
  ungroup()

oscn_pays_tax2 <- oscn_pays_tax2 %>% 
  group_by(year, tax) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE)) %>% 
  ungroup()

total_tax2 <- rbind(odcr_pays_tax2, oscn_pays_tax2)

total_tax2 <- total_tax2 %>%
  group_by(year, tax) %>% 
  summarize(pays = sum(pays, na.rm = TRUE))

tax2 <- total_tax2 %>% 
  group_by(year, tax) %>% 
  summarize(pays = sum(pays, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = pays/sum(pays) * 100)

#### Perc. of pays from tax withholdings 
total_tax$year <- str_sub(total_tax$mo, 1, 4) %>% 
  as.numeric()

tax <- total_tax %>% 
  group_by(year, tax) %>% 
  summarize(pays = sum(pays, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = pays/sum(pays) * 100)

#### Fees by casetype ####
fees_casetype <- oscn_fees %>% 
  bind_rows(odcr_fees)

fees_casetype <- fees_casetype %>% 
  group_by(min_year, casetype) %>% 
  summarize(fees = sum(fee_amt, na.rm = TRUE))

#### Pays by casetype ####
pays_casetype <- oscn_pays %>% 
  bind_rows(odcr_pays)

pays_casetype <- pays_casetype %>% 
  group_by(pay_year, casetype) %>% 
  summarize(pays = sum(pay_amt, na.rm = TRUE))

pays_casetype <- pays_casetype %>% 
  mutate(min_year = pay_year) %>% 
  ungroup()

pays_casetype <- pays_casetype %>% 
  select(-pay_year)

total_casetype <- fees_casetype %>% 
  left_join(pays_casetype)

total_casetype <- total_casetype %>% 
  group_by(casetype, min_year) %>% 
  summarize(fees = sum(fees, na.rm = TRUE, digits = 2),
            pays = sum(pays, na.rm = TRUE, digits = 2))

total_casetype <- total_casetype %>% 
  mutate(debt = fees - pays)

#checking
ggplot(total_casetype) +
  geom_line(aes(min_year, fees, color = casetype), linetype = "dashed") + 
  geom_line(aes(min_year, pays, color = casetype)) +
  theme_ojo() +
  ylim(0, NA)


