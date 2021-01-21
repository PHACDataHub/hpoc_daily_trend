df_filter <- df %>%
  filter(date >= "2020-03-08")

table_nat_stat <- df_filter %>%
  dplyr::rename(Jurisdiction = prname,
                Cases_Cumulative = numtotal,
                Deaths_Cumulative = numdeaths,
                Date = date) %>%
  group_by(Jurisdiction) %>%
  filter(Jurisdiction!="Repatriated Travellers") %>%
  mutate(Cases_Daily = Cases_Cumulative-lag(Cases_Cumulative)) %>%
  mutate(Deaths_Daily = Deaths_Cumulative-lag(Deaths_Cumulative)) %>%
  mutate(Cases_Daily_7MA = rollmean(Cases_Daily, k=7, fill=NA, align=c("right"))) %>%
  mutate(Deaths_Daily_7MA = rollmean(Deaths_Daily, k=7, fill=NA, align=c("right"))) %>%
  mutate(Weekly_Change_Cases = (Cases_Daily_7MA-lag(Cases_Daily_7MA,n=7L,default=0))/lag(Cases_Daily_7MA,n=7L,default=0)) %>%
  mutate(Weekly_Change_Deaths = (Deaths_Daily_7MA-lag(Deaths_Daily_7MA,n=7L,default=0))/lag(Deaths_Daily_7MA,n=7L,default=0)) %>%
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,Deaths_Daily,Deaths_Daily_7MA,
         Weekly_Change_Deaths) %>%
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
  select(Jurisdiction.x,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Weekly_Change_Deaths,National_Death_Proportion) %>%
  dplyr::rename(Jurisdiction=Jurisdiction.x)

Case_Death_Stats <- PT7 %>% 
  group_by(Jurisdiction) %>% 
  filter(Date==max(Date)) %>%
  mutate(Weekly_Change_Cases=percent(Weekly_Change_Cases,accuracy=0.1)) %>%
  mutate(National_Case_Proportion=percent(National_Case_Proportion,accuracy=0.1)) %>%
  mutate(Weekly_Change_Deaths=percent(Weekly_Change_Deaths,accuracy=0.1)) %>%
  mutate(National_Death_Proportion=percent(National_Death_Proportion,accuracy=0.1)) %>%
  mutate(Cases_Daily_7MA=round(Cases_Daily_7MA)) %>%
  mutate(Deaths_Daily_7MA=round(Deaths_Daily_7MA,1)) 

juriorder <- c("Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut")


Canada_pop <- pt_pop_raw %>%
  mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  filter(`Age group`=="All ages",REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  select(GEO,VALUE) %>%
  dplyr::rename(Population=VALUE)

Case_Death_Stats <- Case_Death_Stats %>%
  filter(Jurisdiction!="Repatriated travellers") %>%
  left_join(Canada_pop, by=c("Jurisdiction"="GEO")) %>%
  mutate(Jurisdiction =  factor(Jurisdiction, levels = juriorder),
         Date = format(Date, "%B %d"),
         Cases_7MA_per100k = round((Cases_Daily_7MA / Population)*100000,digits = 1),
         Deaths_7MA_per100k=round((Deaths_Daily_7MA / Population)*100000,digits = 2)) %>%
  arrange(Jurisdiction)%>%
  select(Jurisdiction,Date,
         Cases_Daily,Cases_Daily_7MA,Cases_7MA_per100k,Weekly_Change_Cases,National_Case_Proportion,
         Deaths_Daily,Deaths_Daily_7MA, Deaths_7MA_per100k, Weekly_Change_Deaths, National_Death_Proportion)

Case_per_100K <- PT7 %>%
  filter(Jurisdiction=="Canada") %>%
  left_join(Canada_pop,by=c("Jurisdiction"="GEO"),keep=FALSE) %>%
  mutate(Case_per_100K = (Cases_Daily/Population)*100000) %>%
  mutate(Case_per_100K_7MA = rollmean(Case_per_100K,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,Cases_Daily,Case_per_100K,Case_per_100K_7MA)

write.csv(Case_per_100K,"Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\case_per_100k.csv")
