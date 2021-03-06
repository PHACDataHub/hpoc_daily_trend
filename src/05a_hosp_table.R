all_hosp_data<-import_hosp_data()


all_hosp_icu_adj<-all_hosp_data %>%
  group_by(Jurisdiction) %>%
  mutate(label = if_else(Date == max(Date), as.character(round(cases, digits = 1)), NA_character_)) %>%
  ungroup()


##code to calculate accurate Canada-wide stats (to protect against AB reporting lag)
corrected_Can_stats<-all_hosp_icu_adj %>%
  select(-label) %>%
  filter(Date>=max(Date)-6) %>%
  filter(!Jurisdiction=="Canada") %>%
  pivot_wider(names_from=type, values_from=cases) %>%
  group_by(Jurisdiction) %>%
  summarise(hosp_7MA=mean(hospitalized),
            icu_7MA=mean(icu),
            .groups="drop_last")%>%
  ungroup()%>%
  summarise(Can_hosp7MA=sum(hosp_7MA, na.rm=TRUE),
            Can_icu7MA=sum(icu_7MA, na.rm=TRUE)) %>%
  as.numeric()

hosp_metrics1 <- all_hosp_icu_adj %>%
  filter(type=="hospitalized") %>%
  group_by(Jurisdiction) %>%
  mutate(hosp7ma=rollmean(cases, k=7, fill=NA, align="right"))

hosp_metrics1[hosp_metrics1$Jurisdiction=="Canada"&hosp_metrics1$Date==max(hosp_metrics1$Date),"hosp7ma"]<-corrected_Can_stats[1]

hosp_metrics1<-hosp_metrics1%>%
  mutate(delta7=(hosp7ma-lag(hosp7ma,7))/lag(hosp7ma,7)) %>%
  select(Jurisdiction, Date, cases, hosp7ma, delta7) %>%
  rename("Jurisdiction"=Jurisdiction, "Date"=Date, "Hospitalizations"=cases, 
         #"7 Day MA of Hospitalizations"=hosp7ma, 
         #"Weekly Change in Hospitalizations"=delta7)
         "delta7h"=delta7)

hosp_metrics2 <- all_hosp_icu_adj %>%
  filter(type=="icu") %>%
  group_by(Jurisdiction) %>%
  mutate(icu7ma=rollmean(cases, k=7, fill=NA, align="right")) 

hosp_metrics2[hosp_metrics2$Jurisdiction=="Canada"&hosp_metrics2$Date==max(hosp_metrics2$Date),"icu7ma"]<-as.numeric(corrected_Can_stats[2])

hosp_metrics2<-hosp_metrics2%>%
  mutate(delta7=(icu7ma-lag(icu7ma,7))/lag(icu7ma,7)) %>%
  select(Jurisdiction, Date, cases, icu7ma, delta7) %>%
  rename("Jurisdiction"=Jurisdiction, "Date"=Date, "ICU"=cases, 
         #"7 Day MA of ICU"=icu7ma,
         #"Weekly Change in ICU"=delta7)
         "delta7i"=delta7)


Hosp_Metrics <- hosp_metrics1 %>%
  left_join(hosp_metrics2, by=c("Jurisdiction","Date")) %>%
  left_join(latest_can_pop, by=c("Jurisdiction")) %>%
  mutate(Hosp_popadj=(Hospitalizations / Population)*100000,
         ICU_popadj=(ICU / Population) *100000)

# For the table only, add the latest hosp/icu data for each PT to the Canada daily total.
can_daily_totals<-all_hosp_icu_adj %>%
  filter(!Jurisdiction=="Canada") %>%
  arrange(desc(Date)) %>%
  filter(!is.na(label))%>%
  select(-label) %>%
  pivot_wider(names_from=type, values_from=cases) %>%
  summarise(hosp_total=sum(hospitalized),
            icu_total=sum(icu))%>%
  as.numeric()

Hosp_Metrics_Table <- Hosp_Metrics %>%
  select(-Hosp_popadj, -ICU_popadj, -Population) %>%
  filter(Date==max(Date))%>%
  filter(Jurisdiction!="Repatriated travellers") %>%
  mutate(Hospitalizations=ifelse(Jurisdiction=="Canada", can_daily_totals[1],Hospitalizations),
         ICU = ifelse(Jurisdiction=="Canada", can_daily_totals[2], ICU),
         hosp7ma=round(hosp7ma),
         icu7ma=round(icu7ma)) %>%
  factor_PT_west_to_east(size = "big",Canada_first = TRUE) %>%
  arrange(Jurisdiction) 

#Export hospitalization data
#only dif is we don't include repatriated travelers here, while they were included in the other export.
export_hosp<-Hosp_Metrics %>%
mutate(prov=Jurisdiction) %>%
  recode_PT_names_to_small(geo_variable="prov") %>%
  rename(Hosp=Hospitalizations,
         Hosp7MA=hosp7ma,
         hospweekchange=delta7h,
         ICU7MA=icu7ma,
         ICUweekchange=delta7i) %>%
  select(Jurisdiction,prov,Population,Date,Hosp,Hosp7MA,hospweekchange,Hosp_popadj,ICU,ICU7MA,ICUweekchange,ICU_popadj)

#Note - this export crashed on me today (Feb1). When manually opened, it said file was locked for use by 'another user' - can look into wrapping it in "try()" function perhaps. 

tryCatch(write_csv(export_hosp,"Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\Case count data\\Hosp_icu_historical_data.csv"),
         warning=function(x) "error in hosp export",
         error=function(x) "error in hosp export")

export_trend_hosp<-export_hosp %>%
  filter(Date>=max(Date)-14) %>%
  arrange(Date)

#this will help when manual corrections need to be made!
write_csv(export_trend_hosp, ".\\output\\hosp_15days.csv")


remove(hosp_metrics1,hosp_metrics2, Hosp_Metrics, export_hosp)



############ For summary bullets

national_hosp_stats<-Hosp_Metrics_Table %>%
  ungroup() %>%
  filter(Jurisdiction=="Canada")


key_national_7MA_hosp<-national_hosp_stats %>%
  select(hosp7ma) %>%
  as.numeric() 

key_national_hosp_change<-national_hosp_stats %>%
  select(delta7h) %>%
  as.numeric() %>%
  turn_num_to_percent_change()

key_national_7MA_icu<-national_hosp_stats %>%
  select(icu7ma) %>%
  as.numeric()

key_national_icu_change<-national_hosp_stats %>%
  select(delta7i) %>%
  as.numeric() %>%
  turn_num_to_percent_change()

key_sum_PTs_no_increase_hosp<-Hosp_Metrics_Table %>%
  filter(!Jurisdiction=="Canada" & (round(delta7h,digits=3)<=0|is.na(delta7h))) %>%
  ungroup() %>%
  count() %>%
  as.numeric()

if (key_sum_PTs_no_increase_hosp<13){
key_PTs_increase_hosp<-Hosp_Metrics_Table %>%
  filter(!Jurisdiction=="Canada" & delta7h>0) %>%
  select(Jurisdiction, delta7h) %>%
  arrange(desc(delta7h)) %>%
  turn_num_to_percent_change(numeric_variable="delta7h",accuracy = 1) %>%
  mutate(Jurisdiction=as.character(Jurisdiction)) %>%
  recode_PT_names_to_small()%>%
  mutate(delta7h=ifelse(delta7h=="0%","+<1%",delta7h)) %>%
  mutate(text_var=paste0(Jurisdiction," (",delta7h,")")) %>%
  ungroup()

key_PTs_increase_hosp<-PHACTrendR::turn_char_vec_to_comma_list(key_PTs_increase_hosp$text_var)
}

key_sum_PTs_no_increase_icu<-Hosp_Metrics_Table %>%
  filter(!Jurisdiction=="Canada" & (round(delta7i, digits = 3)<=0|is.na(delta7i))) %>%
  ungroup() %>%
  count() %>%
  as.numeric()
if (key_sum_PTs_no_increase_icu<13){
  key_PTs_increase_icu<-Hosp_Metrics_Table %>%
    filter(!Jurisdiction=="Canada" & delta7i>0) %>%
    select(Jurisdiction, delta7i) %>%
    arrange(desc(delta7i)) %>%
    turn_num_to_percent_change(numeric_variable="delta7i",accuracy = 1) %>%
    mutate(Jurisdiction=as.character(Jurisdiction)) %>%
    recode_PT_names_to_small()%>%
    mutate(delta7i=ifelse(delta7i=="0%","+<1%",delta7i)) %>%
    mutate(text_var=paste0(Jurisdiction," (",delta7i,")")) %>%
    ungroup()
  
  key_PTs_increase_icu<-PHACTrendR::turn_char_vec_to_comma_list(key_PTs_increase_icu$text_var)
}
