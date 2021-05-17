library(ojodb)
options(scipen = 999)

#### OSCN ####
oscn_fees <- ojo_query_mins(oscn_counties, c("CF", "CM"), 2010:2020, min_years = 2019:2020, min_type = "fees")
oscn_fees_raw <- oscn_fees
oscn_pays <- ojo_query_pays(oscn_counties, c("CF", "CM"), 2010:2020, pay_years = 2019:2020)
oscn_pays_raw <- oscn_pays
#### ODCR ####
odcr_fees <- ojo_query_mins(odcr_counties, c("CF", "CM"), 2010:2020, min_years = 2019:2020, min_type = "fees")
odcr_fees_raw <- odcr_fees
odcr_pays <- ojo_query_pays(odcr_counties, c("CF", "CM"), 2010:2020, pay_years = 2019:2020)
odcr_pays_raw <- odcr_pays
####Cleaning OSCN####
oscn_fees$min_year <- str_sub(oscn_fees$min_date, 1, 4) %>% 
  as.numeric()

oscn_fees<- oscn_fees %>%       #filters out two rows
  filter(fee_amt < 200000)

oscn_fees <- oscn_fees %>% 
  filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED"))

oscn_pays$pay_year <- str_sub(oscn_pays$pay_date, 1, 4) %>% 
  as.numeric()

oscn_pays <- oscn_pays %>% 
  filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"))

oscn_pays <- oscn_pays %>% 
  filter(pay_amt < 2000)

oscn_fees <- oscn_fees %>% 
  mutate(mo = floor_date(ymd(min_date), "month"))

oscn_fees<- oscn_fees %>% 
  filter(mo < "2020-07-01")

oscn_pays <- oscn_pays %>% 
  mutate(mo = floor_date(ymd(pay_date), "month")) 

oscn_pays<- oscn_pays %>% 
  filter(mo < "2020-07-01")

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

odcr_fees2 <- odcr_fees2 %>% 
  filter(mo < "2020-07-01")

odcr_pays2 <- odcr_pays2 %>% 
  filter(mo < "2020-07-01")
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

####Total####
total <- total_fees %>% 
  left_join(total_pays)
