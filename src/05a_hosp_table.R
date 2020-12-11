jurisdiction <- if (Sys.getenv("hosp_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

#Create table for hospitalization metrics
pt_hosp_icu_filter2 <- pt_hosp_icu %>%
  filter(prname %in% c(jurisdiction,"Canada")) %>%
  filter(date >= "2020-07-01") %>%
  group_by(prname) %>%
  mutate(label = if_else(date == max(date), as.character(round(cases, digits = 1)), NA_character_))

hosp_metrics1 <- pt_hosp_icu_filter2 %>%
  filter(type=="hospitalized") %>%
  group_by(prname) %>%
  mutate(hosp7ma=round(rollmean(cases, k=7, fill=NA, align="right"))) %>%
  mutate(delta7=(cases-lag(cases,7))/lag(cases,7)) %>%
  mutate(delta7=percent(delta7,accuracy = 0.1)) %>%
  select(prname, date, cases, hosp7ma, delta7) %>%
  rename("Jurisdiction"=prname, "Date"=date, "Hospitalizations"=cases, 
         #"7 Day MA of Hospitalizations"=hosp7ma, 
         #"Weekly Change in Hospitalizations"=delta7)
         "delta7h"=delta7)

hosp_metrics2 <- pt_hosp_icu_filter2 %>%
  filter(type=="icu") %>%
  group_by(prname) %>%
  mutate(icu7ma=round(rollmean(cases, k=7, fill=NA, align="right"))) %>%
  mutate(delta7=(cases-lag(cases,7))/lag(cases,7)) %>%
  mutate(delta7=percent(delta7,accuracy=0.1)) %>%
  select(prname, date, cases, icu7ma, delta7) %>%
  rename("Jurisdiction"=prname, "Date"=date, "ICU"=cases, 
         #"7 Day MA of ICU"=icu7ma,
         #"Weekly Change in ICU"=delta7)
         "delta7i"=delta7)

Hosp_Metrics <- hosp_metrics1 %>%
  left_join(hosp_metrics2, by=c("Jurisdiction","Date")) %>%
  filter(Date==max(Date))

metricorder <- c("Canada","British Columbia","Alberta","Ontario","Quebec","Manitoba","Saskatchewan")

Hosp_Metrics <- Hosp_Metrics %>% slice(match(metricorder,Jurisdiction))

remove(hosp_metrics1,hosp_metrics2)