#using package to get data"
df<-PHACTrendR::import_adjusted_infobase_data()
df_raw<-PHACTrendR::import_raw_infobase_data()

df_filter <- df %>%
  filter(date >= "2020-03-08")

#Derive 7dMA for cases and deaths, as well as weekly changes for cases/deaths. NOTE - the national numbers are vulnerable to non-reporting here.
df_moving_averages <- df_filter %>%
  dplyr::rename(Cases_Cumulative = numtotal,
                Deaths_Cumulative = numdeaths,
                Cases_Daily=numtoday,
                Deaths_Daily=numdeathstoday,
                Date = date) %>%
  group_by(Jurisdiction) %>%
  filter(Jurisdiction!="Repatriated Travellers") %>%
  mutate(Cases_Daily_7MA = rollmean(Cases_Daily, k=7, fill=NA, align=c("right")),
         Deaths_Daily_7MA = rollmean(Deaths_Daily, k=7, fill=NA, align=c("right"))) %>%
  ungroup()

#Loop that goes through the last 7 days and recalculates the national 7dMAs for cases and deaths
# this is done because when a PT does not report, it shifts the days of their contribution to national 7MA
dates<-seq(max(df_filter$date)-6, max(df_filter$date), by=1)
correct_Can_7MA<-function(input_date=""){
  can_corrected_7mas<-df_filter %>%
  filter(!Jurisdiction %in% c("Canada", "Repatriated travellers")) %>%
  filter(date>= as.Date(input_date)-6 & date<= as.Date(input_date)) %>%
  group_by(Jurisdiction) %>%
  summarise(PT_case7ma=mean(numtoday),
            PT_death7ma=mean(numdeathstoday),
            .groups="drop_last") %>%
  ungroup() %>%
  summarise(can_case7ma=sum(PT_case7ma),
            can_death7ma=sum(PT_death7ma))
  
  df_moving_averages<-df_moving_averages %>%
    mutate(Cases_Daily_7MA=ifelse((Jurisdiction=="Canada")&(Date==as.Date(input_date)), can_corrected_7mas[[1]],Cases_Daily_7MA),
           Deaths_Daily_7MA=ifelse((Jurisdiction=="Canada")&(Date==as.Date(input_date)), can_corrected_7mas[[2]],Deaths_Daily_7MA))
    return(df_moving_averages)
}
for (i in dates){ 
  df_moving_averages<-correct_Can_7MA(input_date=i)
}
## Back to regularly scheduled programming...

df_weekly_changes<-df_moving_averages %>%
  group_by(Jurisdiction) %>%
  mutate(Weekly_Change_Cases = (Cases_Daily_7MA-lag(Cases_Daily_7MA,n=7L,default=0))/lag(Cases_Daily_7MA,n=7L,default=0),
         Weekly_Change_Deaths = (Deaths_Daily_7MA-lag(Deaths_Daily_7MA,n=7L,default=0))/lag(Deaths_Daily_7MA,n=7L,default=0)) %>%
  arrange(Jurisdiction,Date)

#Getting Canada weekly case and death totals to calculate National proportions
Canada7 <- df_weekly_changes %>%
  filter(Jurisdiction=="Canada") %>%
  mutate(CanadaCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(CanadaDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,CanadaCase7,CanadaDeath7)

#Driving national proportion of cases, and national proportion of deaths variables
PT7 <- df_weekly_changes %>%
  group_by(Jurisdiction) %>%
  mutate(PTCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(PTDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
  left_join(Canada7,by="Date",keep=FALSE) %>%
  mutate(National_Case_Proportion=PTCase7/CanadaCase7) %>%
  mutate(National_Death_Proportion=PTDeath7/CanadaDeath7) %>%
  dplyr::rename(Jurisdiction=Jurisdiction.x) %>%
  left_join(PHACTrendR::latest_can_pop, by=c("Jurisdiction")) %>%
  mutate( Cases_7MA_per100k = (Cases_Daily_7MA / Population)*100000,digits = 2,
            Deaths_7MA_per100k=(Deaths_Daily_7MA / Population)*100000,digits = 2)



Case_Death_Stats_1 <- PT7 %>% 
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Cases_7MA_per100k, Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Deaths_7MA_per100k,Weekly_Change_Deaths,National_Death_Proportion) %>%
  group_by(Jurisdiction) %>% 
  filter(Date==max(Date))

Case_Death_Stats <- Case_Death_Stats_1 %>%
  filter(Jurisdiction!="Repatriated travellers") %>%
  factor_PT_west_to_east(size="big", Canada_first = TRUE) %>%
  mutate(Date = format(Date, "%B %d")) %>%
  arrange(Jurisdiction)%>%
  select(Jurisdiction,Date,
         Cases_Daily,Cases_Daily_7MA,Cases_7MA_per100k,Weekly_Change_Cases,National_Case_Proportion,
         Deaths_Daily,Deaths_Daily_7MA, Deaths_7MA_per100k, Weekly_Change_Deaths, National_Death_Proportion)

Case_per_100K <- PT7 %>%
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Weekly_Change_Deaths,National_Death_Proportion) %>%
  filter(Jurisdiction=="Canada") %>%
  left_join(latest_can_pop,by=c("Jurisdiction"),keep=FALSE) %>%
  mutate(Case_per_100K = (Cases_Daily/Population)*100000,
         Case_per_100K_7MA = (Cases_Daily_7MA/Population)*100000) %>%
  select(Jurisdiction,Date,Cases_Daily,Case_per_100K,Case_per_100K_7MA)

# No longer running the python code as .py, rather using .rmd file, so writing/reading a csv file no longer needed!
# write.csv(Case_per_100K,"Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\case_per_100k.csv")

# creating "COVID_CaseDeath_7MA.csv" file currently exported by 01.sas file in the trend report code.
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
         Cases_Daily_7MA=ifelse(Cases_Daily_7MA<1, round(Cases_Daily_7MA, digits = 0.1), round(Cases_Daily_7MA)),
         Deaths_Daily_7MA=round(Deaths_Daily_7MA,1),
         Cases_WeeklyPercentChange=round(Cases_WeeklyPercentChange*100,1),
         Deaths_WeeklyPercentChange=round(Deaths_WeeklyPercentChange*100,1),
         Cases_National_Proportion=label_percent(accuracy = 0.01)(round(Cases_National_Proportion,3)),
         Deaths_National_Proportion=label_percent(accuracy = 0.01)(round(Deaths_National_Proportion,3))) %>%
  ungroup() %>%
  select(Jurisdiction, update, Date, Cases_Cumulative, Cases_Daily, Cases_Daily_7MA, Cases_7MA_per100k, Cases_CurrentWeek, Cases_PreviousWeek,Cases_WeeklyPercentChange,  Cases_National_Proportion, 
         Deaths_Cumulative, Deaths_Daily, Deaths_Daily_7MA, Deaths_7MA_per100k, Deaths_CurrentWeek, Deaths_PreviousWeek, Deaths_WeeklyPercentChange, Deaths_National_Proportion)

tryCatch(write_csv(export_case_death, "Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\Case count data\\COVID_CaseDeath_7MA.csv"),
         warning=function(x) "error in case/death export",
         error=function(x) "error in case/death export")

export_trend_cd<-export_case_death %>%
  filter(Date>=max(Date)-14)

#this will help when manual corrections need to be made!
write_csv(export_trend_cd, ".\\output\\cases_deaths_15days.csv")

# For summary bullets / table footnotes:
#Deriving "key" variables for automation of summary bullets

key_national_7MA_cases<-comma(Case_Death_Stats$Cases_Daily_7MA[Case_Death_Stats$Jurisdiction=="Canada"])
key_national_weekly_change_cases<-PHACTrendR::turn_num_to_percent_change(Case_Death_Stats$Weekly_Change_Cases[Case_Death_Stats$Jurisdiction=="Canada"])
key_national_7MA_deaths<-comma(Case_Death_Stats$Deaths_Daily_7MA[Case_Death_Stats$Jurisdiction=="Canada"],accuracy = 0.1)
key_national_weekly_change_deaths<-PHACTrendR::turn_num_to_percent_change(Case_Death_Stats$Weekly_Change_Deaths[Case_Death_Stats$Jurisdiction=="Canada"])

key_sum_PTs_no_increase_cases<-Case_Death_Stats %>%
  filter(!Jurisdiction=="Canada" & (round(Weekly_Change_Cases,digits=3)<=0|is.na(Weekly_Change_Cases))) %>%
  ungroup() %>%
  count() %>%
  as.numeric()

if (key_sum_PTs_no_increase_cases<13){
  
  key_PTs_increase_cases<-Case_Death_Stats %>%
    filter(!Jurisdiction=="Canada" & Weekly_Change_Cases>0) %>%
    select(Jurisdiction, Weekly_Change_Cases) %>%
    arrange(desc(Weekly_Change_Cases)) %>%
    turn_num_to_percent_change(numeric_variable="Weekly_Change_Cases",accuracy = 1) %>%
    mutate(Jurisdiction=as.character(Jurisdiction)) %>%
    recode_PT_names_to_small()%>%
    mutate(Weekly_Change_Cases=ifelse(Weekly_Change_Cases=="0%","+<1%",Weekly_Change_Cases)) %>%
    mutate(text_var=paste0(Jurisdiction," (",Weekly_Change_Cases,")")) %>%
    ungroup()
  
  key_PTs_increase_cases<-PHACTrendR::turn_char_vec_to_comma_list(key_PTs_increase_cases$text_var)
}


key_sum_PTs_no_increase_deaths<-Case_Death_Stats %>%
  filter(!Jurisdiction=="Canada" & (round(Weekly_Change_Deaths,digits=3)<=0|is.na(Weekly_Change_Deaths))) %>%
  ungroup() %>%
  count() %>%
  as.numeric()

if (key_sum_PTs_no_increase_deaths<13){
  key_PTs_increase_deaths<-Case_Death_Stats %>%
    filter(!Jurisdiction=="Canada" & Weekly_Change_Deaths>0) %>%
    select(Jurisdiction, Weekly_Change_Deaths) %>%
    arrange(desc(Weekly_Change_Deaths)) %>%
    turn_num_to_percent_change(numeric_variable="Weekly_Change_Deaths",accuracy = 1) %>%
    mutate(Jurisdiction=as.character(Jurisdiction)) %>%
    recode_PT_names_to_small()%>%
    mutate(Weekly_Change_Deaths=ifelse(Weekly_Change_Deaths=="0%","+<1%",Weekly_Change_Deaths)) %>%
    mutate(text_var=paste0(Jurisdiction," (",Weekly_Change_Deaths,")")) %>%
    ungroup()
  
  key_PTs_increase_deaths<-PHACTrendR::turn_char_vec_to_comma_list(key_PTs_increase_deaths$text_var)
}
#to automate a footnote on the cases/deaths table
any_non_report_flag<-ifelse(nrow(df_raw[df_raw$date==max(df_raw$date)&df_raw$update==FALSE&!is.na(df_raw$update),])>0, TRUE, FALSE)
if(any_non_report_flag==TRUE){
  key_PTs_nonreport<-df_raw$Jurisdiction[df_raw$date==max(df_raw$date)&df_raw$update==FALSE&!is.na(df_raw$update)] 
  
  key_PTs_nonreport<-recode_PT_names_to_small(key_PTs_nonreport)
}
key_latest_case_rate_7MA<-round(Case_per_100K$Case_per_100K_7MA[Case_per_100K$Date==max(Case_per_100K$Date)],digits = 2)
key_latest_date_100k_fig<-format(max(Case_per_100K$Date), "%B %d")
key_100k_caption<-paste0("Spring peak: April 26, 4.55 cases/100k, Winter peak: January 10, 21.74 cases/100k, Today's value (",key_latest_date_100k_fig,"): ",key_latest_case_rate_7MA," cases/100k           Updated daily (Sun-Thurs). Data as of: ",key_latest_date_100k_fig)

