df_filter <- df %>%
  filter(date >= "2020-03-08")

table_nat_stat <- df_filter %>%
  dplyr::rename(Jurisdiction = prname,
                Cases_Cumulative = numtotal,
                Deaths_Cumulative = numdeaths,
                Cases_Daily=numtoday,
                Deaths_Daily=numdeathstoday,
                Date = date) %>%
  group_by(Jurisdiction) %>%
  filter(Jurisdiction!="Repatriated Travellers") %>%
  mutate(Cases_Daily_7MA = rollmean(Cases_Daily, k=7, fill=NA, align=c("right")),
         Deaths_Daily_7MA = rollmean(Deaths_Daily, k=7, fill=NA, align=c("right")),
         Weekly_Change_Cases = (Cases_Daily_7MA-lag(Cases_Daily_7MA,n=7L,default=0))/lag(Cases_Daily_7MA,n=7L,default=0),
         Weekly_Change_Deaths = (Deaths_Daily_7MA-lag(Deaths_Daily_7MA,n=7L,default=0))/lag(Deaths_Daily_7MA,n=7L,default=0)) %>%
  arrange(Jurisdiction,Date)

Canada7 <- table_nat_stat %>%
  filter(Jurisdiction=="Canada") %>%
  mutate(CanadaCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(CanadaDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,CanadaCase7,CanadaDeath7)

PT7 <- table_nat_stat %>%
  group_by(Jurisdiction) %>%
  mutate(PTCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(PTDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
  left_join(Canada7,by="Date",keep=FALSE) %>%
  mutate(National_Case_Proportion=PTCase7/CanadaCase7) %>%
  mutate(National_Death_Proportion=PTDeath7/CanadaDeath7) %>%
  dplyr::rename(Jurisdiction=Jurisdiction.x)

Case_Death_Stats_1 <- PT7 %>% 
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Weekly_Change_Deaths,National_Death_Proportion) %>%
  group_by(Jurisdiction) %>% 
  filter(Date==max(Date)) %>%
  mutate(Weekly_Change_Cases=percent(Weekly_Change_Cases,accuracy=0.1)) %>%
  mutate(National_Case_Proportion=percent(National_Case_Proportion,accuracy=0.1)) %>%
  mutate(Weekly_Change_Deaths=percent(Weekly_Change_Deaths,accuracy=0.1)) %>%
  mutate(National_Death_Proportion=percent(National_Death_Proportion,accuracy=0.1)) %>%
  mutate(Cases_Daily_7MA=round(Cases_Daily_7MA)) %>%
  mutate(Deaths_Daily_7MA=round(Deaths_Daily_7MA,1)) 

juriorder <- c("Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut")

Case_Death_Stats <- Case_Death_Stats_1 %>%
  filter(Jurisdiction!="Repatriated travellers") %>%
  left_join(latest_can_pop, by=c("Jurisdiction"="GEO")) %>%
  mutate(Jurisdiction =  factor(Jurisdiction, levels = juriorder),
         Date = format(Date, "%B %d"),
         # Weekly_Change_Cases=case_when(Weekly_Change_Cases>0 ~ paste0("+",Weekly_Change_Cases),
         #                               TRUE ~ as.character(Weekly_Change_Cases)),
         # Weekly_Change_Deaths=case_when(Weekly_Change_Deaths>0 ~ paste0("+",Weekly_Change_Deaths),
         #                                TRUE ~ as.character(Weekly_Change_Deaths)),
         Cases_7MA_per100k = round((Cases_Daily_7MA / Population)*100000,digits = 1),
         Deaths_7MA_per100k=round((Deaths_Daily_7MA / Population)*100000,digits = 2)) %>%
  arrange(Jurisdiction)%>%
  select(Jurisdiction,Date,
         Cases_Daily,Cases_Daily_7MA,Cases_7MA_per100k,Weekly_Change_Cases,National_Case_Proportion,
         Deaths_Daily,Deaths_Daily_7MA, Deaths_7MA_per100k, Weekly_Change_Deaths, National_Death_Proportion)

Case_per_100K <- PT7 %>%
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Weekly_Change_Deaths,National_Death_Proportion) %>%
  filter(Jurisdiction=="Canada") %>%
  left_join(latest_can_pop,by=c("Jurisdiction"="GEO"),keep=FALSE) %>%
  mutate(Case_per_100K = (Cases_Daily/Population)*100000) %>%
  mutate(Case_per_100K_7MA = rollmean(Case_per_100K,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,Cases_Daily,Case_per_100K,Case_per_100K_7MA)

write.csv(Case_per_100K,"Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\case_per_100k.csv")

# creating "COVID_CaseDeath_7MA.csv" file currently exported by 01.sas file in the trend report code. ----
# Still some vars missing: Recovered_Cumulative, Recovered_Daily, Recovered_Daily_7MA, Tested_Cumulative, Tested_Daily, Tested_Daily_7MA, National_cases_currentweek, National_deaths_currentweek
export_case_death<-PT7 %>%
  rename(Cases_WeeklyPercentChange=Weekly_Change_Cases,
         Deaths_WeeklyPercentChange=Weekly_Change_Deaths,
         Cases_National_Proportion=National_Case_Proportion,
         Deaths_National_Proportion=National_Death_Proportion,
         Recovered_Cumulative=numrecover,
         Recovered_Daily=numrecoveredtoday) %>%
  group_by(Jurisdiction) %>%
  mutate(Cases_CurrentWeek=rollsum(x=Cases_Daily, k=7,align="right", fill=NA),
         Cases_PreviousWeek=lag(Cases_CurrentWeek,n=7L),
         Deaths_CurrentWeek=rollsum(x=Deaths_Daily, k=7,align="right", fill=NA),
         Deaths_PreviousWeek=lag(Deaths_CurrentWeek,n=7L),
         Cases_Daily_7MA=round(Cases_Daily_7MA),
         Deaths_Daily_7MA=round(Deaths_Daily_7MA,1),
         Cases_WeeklyPercentChange=round(Cases_WeeklyPercentChange*100,1),
         Deaths_WeeklyPercentChange=round(Deaths_WeeklyPercentChange*100,1),
         Cases_National_Proportion=label_percent(accuracy = 0.01)(round(Cases_National_Proportion,3)),
         Deaths_National_Proportion=label_percent(accuracy = 0.01)(round(Deaths_National_Proportion,3))) %>%
  ungroup() %>%
  select(Jurisdiction, update, Date, Cases_Cumulative, Cases_Daily, Cases_Daily_7MA, Cases_CurrentWeek, Cases_PreviousWeek,Cases_WeeklyPercentChange,  Cases_National_Proportion, 
         Deaths_Cumulative, Deaths_Daily, Deaths_Daily_7MA, Deaths_CurrentWeek, Deaths_PreviousWeek, Deaths_WeeklyPercentChange, Deaths_National_Proportion)

write_csv(export_case_death, "Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\Case count data\\COVID_CaseDeath_7MA.csv")


export_trend_cd<-export_case_death %>%
  filter(Date>=max(Date)-14)

#this will help when manual corrections need to be made!
write_csv(export_trend_cd, "cases_deaths_15days.csv")
