#### tax season highlight #### 
tax_season <- tibble(x_min = paste0(2012:2018, "-02-01") %>% ymd,
                     x_max = paste0(2012:2018, "-04-15") %>% ymd)

ggplot() + 
  geom_rect(data = tax_season, aes(xmin = as.Date(x_min), xmax = as.Date(x_max), ymin = 0, ymax = 15000000), fill = "#F8D64E", alpha = .4) +
  geom_line(data = total_fees, aes(mo, fees), color = "black") +
  geom_line(data=total_pays, aes(mo, pays), color = "black") +
  theme_ojo() + 
  ylim(0, NA) +
  ggtitle("Fines and fees: Charged and collected")

#### Fines & fees charged & collected by year (Blog 1, Viz 1) ####
total_pays$year <- str_sub(total_pays$mo, 1, 4) %>% 
  as.numeric()

total_fees$year <- str_sub(total_fees$mo, 1, 4) %>% 
  as.numeric()

pays_year <- total_pays %>% 
  group_by(year) %>% 
  summarize(pays = sum(pays, na.rm = TRUE))

fees_year <- total_fees %>% 
  group_by(year) %>% 
  summarize(fees = sum(fees, na.rm = TRUE, digits = 2))


ggplot(fees_year, aes(year, fees), color = "black") + 
  geom_line() +
  geom_line(data=pays_year, aes(year, pays), color = "#F8D64E") +
  theme_ojo() + 
  ylim(0, NA) +
  ggtitle("Fines and fees: Charged and collected by year")

####Fees per year ####
ggplot(fees_year, aes(year, fees), color = "black") + 
  geom_line() +
  theme_ojo() + 
  ylim(0, NA) +
  labs(title = "Total court fines and fees\n charged per year",
       subtitle = "Felony and misdemeanor cases filed\n in Oklahoma District Courts",
       caption = "Source: Open Justice Oklahoma")

#add styling and titles
ggplot(fees_year, aes(year, fees), color = "black") + 
  geom_line() +
  geom_line(data=pays_year, aes(year, pays), color = "#F8D64E") +
  theme_ojo() + 
  ylim(0, 160) +
  xlim(2012, 2018) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = paste0("$", c(40, 80, 120, 160), "M"),
                     breaks = c(40000000, 80000000, 120000000, 160000000)) +
  labs(title = "Total court fines and fees\n assessed and collected",
       subtitle = "Felony and misdemeanor cases filed\n in Oklahoma District Courts",
       caption = "Source: Open Justice Oklahoma")

#### by casetype (Blog 1, Viz 2)####
ggplot(total_casetype) +
  geom_line(aes(min_year, fees, color = casetype), linetype = "dashed") +
  geom_line(aes(min_year, pays, color = casetype)) +
  theme_ojo() + 
  scale_color_manual(values = c("black", "#F8D64E")) +
  ylim(0, NA) +
  xlim(2012, 2018) +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(labels = paste0("$", c(40, 80, 120, 160), "M"),
                     breaks = c(40000000, 80000000, 120000000, 160000000)) +
  labs(title = "Total court fines and fees\n assessed and collected",
       subtitle = "Felony and misdemeanor cases filed\n in Oklahoma District Courts",
       caption = "Source: Open Justice Oklahoma")

write.csv(total_casetype, file = "Fines and fees by casetype.csv")

#### outstanding debt by casetype ####
ggplot(total_casetype, aes(x = min_year, y = debt, fill = casetype),
       family = "Menlo") +
  geom_bar(position = "stack", stat= "identity") +
  theme_ojo() +
  ylim(0, NA) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 11),
        legend.title = element_blank()) +
  scale_y_continuous(labels = paste0("$", c(40, 80, 120, 160), "M"),
                     breaks = c(40000000, 80000000, 120000000, 160000000)) +
  labs(title= "Outstanding court debt", fill = "casetype") +
  scale_fill_manual(values = c("#F8D64E", "black")) 

#### pays by tax intercept ####
ggplot() + 
  geom_rect(data = tax_season, aes(xmin = as.Date(x_min), xmax = as.Date(x_max), ymin = 0, ymax = 5000000), fill = "#F8D64E", alpha = .4) +
  geom_line(data = total_tax, aes(mo, pays, color = tax)) +
  theme_ojo() + 
  ylim(0, NA) +
  scale_color_manual(values = c("#F8D64E", "black")) +
  ggtitle("Fines and fees: Charged \nand collected seperated by tax intercept")

#### pays with tax highlight ####
ggplot() + 
  geom_rect(data = tax_season, aes(xmin = as.Date(x_min), xmax = as.Date(x_max), ymin = 0, ymax = 6000000), fill = "#F8D64E", alpha = .4) +
  geom_line(data = total_pays, aes(mo, pays)) +
  theme_ojo() + 
  ylim(0, NA) +
  scale_color_manual(values = c("black")) +
  ggtitle("Fines and fees: Charged \nand collected seperated by tax intercept")


#### AOC collection totals ####
ggplot(total_collections, aes(x = year, y = collected, fill = perc),
       family = "Menlo") +
  geom_bar(position = "stack", stat= "identity") +
  theme_ojo() +
  ylim(0, NA) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 11),
        legend.title = element_blank()) +
  labs(title= "Court collections from criminal cases", fill = "Pays") 

#### District court collections ####
ggplot(d2, aes(x = quarter, y = perc, fill = casetype_new)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_ojo() +
  geom_label(aes(label = paste0(casetype_new," \n", "(", perc, "%)"), group = casetype_new, color = casetype_new),
             position = position_stack(vjust = .5),
             family = "Menlo") +
  ylim(0, NA) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("#F8D64E", "gray", "black")) +
  scale_color_manual(values = c("black", "black", "#F8D64E")) +
  labs(title = "Total District Court collections",
       subtitle = "District Court data",
       caption = "Source: Open Justice Oklahoma")
