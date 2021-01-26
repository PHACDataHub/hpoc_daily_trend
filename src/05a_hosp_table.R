jurisdiction <- if (Sys.getenv("hosp_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", 
                                                                           "Saskatchewan", "Manitoba", "Ontario", 
                                                                           "Quebec", "Newfoundland and Labrador", 
                                                                           "New Brunswick", "Nova Scotia", 
                                                                           "Prince Edward Island", "Yukon", 
                                                                           "Northwest Territories")

#Create table for hospitalization metrics
pt_hosp_icu_filter2 <- pt_hosp_icu %>%
  filter(prname %in% c(jurisdiction,"Canada")) %>%
  filter(date >= "2020-07-01") %>%
  group_by(prname) %>%
  mutate(label = if_else(date == max(date), as.character(round(cases, digits = 1)), NA_character_))

##code to calculate accurate Canada-wide stats (to protect against AB reporting lag)
corrected_Can_stats<-pt_hosp_icu %>%
  filter(date>=Sys.Date()-6) %>%
  filter(!prname=="Canada") %>%
  pivot_wider(names_from=type, values_from=cases) %>%
  group_by(prname) %>%
  summarise(hosp_7MA=mean(hospitalized),
            icu_7MA=mean(icu))%>%
  ungroup()%>%
  summarise(Can_hosp7MA=sum(hosp_7MA),
            Can_icu7MA=sum(icu_7MA)) %>%
  as.numeric()

hosp_metrics1 <- pt_hosp_icu_filter2 %>%
  filter(type=="hospitalized") %>%
  group_by(prname) %>%
  mutate(hosp7ma=rollmean(cases, k=7, fill=NA, align="right"))

hosp_metrics1[hosp_metrics1$prname=="Canada"&hosp_metrics1$date=="2021-01-21","hosp7ma"]<-corrected_Can_stats[1]

hosp_metrics1<-hosp_metrics1%>%
  mutate(delta7=(hosp7ma-lag(hosp7ma,7))/lag(hosp7ma,7)) %>%
  mutate(delta7=percent(delta7,accuracy = 0.1)) %>%
  select(prname, date, cases, hosp7ma, delta7) %>%
  rename("Jurisdiction"=prname, "Date"=date, "Hospitalizations"=cases, 
         #"7 Day MA of Hospitalizations"=hosp7ma, 
         #"Weekly Change in Hospitalizations"=delta7)
         "delta7h"=delta7)

hosp_metrics2 <- pt_hosp_icu_filter2 %>%
  filter(type=="icu") %>%
  group_by(prname) %>%
  mutate(icu7ma=rollmean(cases, k=7, fill=NA, align="right")) 

hosp_metrics2[hosp_metrics2$prname=="Canada"&hosp_metrics2$date=="2021-01-21","icu7ma"]<-as.numeric(corrected_Can_stats[2])

hosp_metrics2<-hosp_metrics2%>%
  mutate(delta7=(icu7ma-lag(icu7ma,7))/lag(icu7ma,7)) %>%
  mutate(delta7=percent(delta7,accuracy=0.1)) %>%
  select(prname, date, cases, icu7ma, delta7) %>%
  rename("Jurisdiction"=prname, "Date"=date, "ICU"=cases, 
         #"7 Day MA of ICU"=icu7ma,
         #"Weekly Change in ICU"=delta7)
         "delta7i"=delta7)

Hosp_Metrics <- hosp_metrics1 %>%
  left_join(hosp_metrics2, by=c("Jurisdiction","Date")) %>%
  filter(Date==max(Date))

prorder <- c("Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec",
             "Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon",
             "Northwest Territories")

Hosp_Metrics <- Hosp_Metrics %>%
  #  filter(Jurisdiction!="Repatriated travellers") %>%
  mutate(Jurisdiction =  factor(Jurisdiction, levels = prorder),
         # delta7h=case_when(delta7h>0 ~ paste0("+",delta7h),
         #                   TRUE ~ as.character(delta7h)),
         # delta7i=case_when(delta7i>0 ~ paste0("+",delta7i),
         #                   TRUE ~ as.character(delta7i))
         hosp7ma=round(hosp7ma),
                  icu7ma=round(icu7ma)) %>%
  arrange(Jurisdiction) 



remove(hosp_metrics1,hosp_metrics2)