salt_raw<-import_SALT_data()

SALT <- salt_raw #filter any PTs, etc. out here.


SALT2 <- SALT %>%
    mutate(Date=as.Date(str_sub(Report.Date,1,10)),
           Time=as_hms(str_sub(Report.Date,13,20)),
           datetime=strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
           Start_of_week=floor_date(Date, "week"),
           End_of_week=date(Start_of_week)+6,
           Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week)),
           Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
    filter(Date <= floor_date(max(Date), "week")-1) %>%
    mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
    arrange(Jurisdiction,datetime)

SALT3 <- SALT2 %>%
    group_by(Jurisdiction,Week) %>%
    filter(datetime==max(datetime))

SALT4 <- SALT3 %>%
    group_by(Jurisdiction) %>%
    mutate(week_tests_performed = Tests.Performed-lag(Tests.Performed),
           week_positive_tests = Positive.Test.Results-lag(Positive.Test.Results),
           week_negative_tests = week_tests_performed-week_positive_tests,
           avg_tests_per_day = week_tests_performed/7,
           percent_positive = week_positive_tests/week_tests_performed)

National <- SALT4 %>%
    select(Week, Start_of_week, week_tests_performed, week_positive_tests, week_negative_tests) %>%
    group_by(Week, Start_of_week) %>%
    summarise(across(where(is.numeric),sum)) %>%
    mutate(Jurisdiction="Canada",
           percent_positive = week_positive_tests/week_tests_performed,
           avg_tests_per_day = week_tests_performed/7) %>%
    arrange(Start_of_week)
  
National <- subset(National, select=-Start_of_week)

National <- National[,c(1,5,2,3,4,6,7)]

Provincial <- SALT4 %>%
    select(Week, Jurisdiction, week_tests_performed, week_positive_tests, week_negative_tests, percent_positive, avg_tests_per_day)

Testing <- rbind(National,Provincial)

Testing <- Testing[complete.cases(Testing), ]

Testing <- Testing %>%
    mutate(Week=gsub(" -","",Week)) %>%
    mutate(Week=gsub("  to  Jan ","-",Week)) %>%
    mutate(Week=gsub("  to  Feb ","-",Week)) %>%
    mutate(Week=gsub("  to  Mar ","-",Week)) %>%
    mutate(Week=gsub("  to  Apr ","-",Week)) %>%
    mutate(Week=gsub("  to  May ","-",Week)) %>%
    mutate(Week=gsub("  to  Jun ","-",Week)) %>%
    mutate(Week=gsub("  to  Jul ","-",Week)) %>%
    mutate(Week=gsub("  to  Aug ","-",Week)) %>%
    mutate(Week=gsub("  to  Sep ","-",Week)) %>%
    mutate(Week=gsub("  to  Oct ","-",Week)) %>%
    mutate(Week=gsub("  to  Nov ","-",Week)) %>%
    mutate(Week=gsub("  to  Dec ","-",Week)) %>%
    group_by(Jurisdiction) %>%
    mutate(Week_no = 1:n()) %>%
    slice(tail(row_number(),12))
    

Testing <- Testing[,c(8,1,2,3,4,5,6,7)]

#to replace the export that used to go to Yann Pelchat - awaiting confirmation that it is still needed.
export_testing1<-Testing %>%
  select(Jurisdiction, Week, week_tests_performed, week_positive_tests)%>%
  filter(Jurisdiction=="Canada")
# write.csv(export_testing1)

export_testing2<-Testing %>%
  select(Jurisdiction, Week, week_tests_performed, week_positive_tests, percent_positive) %>%
  rename(Week_tests_performed=week_tests_performed,
         Week_positive_tests=week_positive_tests,
         Percent_Positive=percent_positive)
write.csv(export_testing2, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Data Analysis\\NML_weekly_testing.csv')

#For use in python script
write.csv(Testing, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\testing.csv')

  Testing_Metrics <- Testing %>%
    group_by(Jurisdiction) %>%
    slice(tail(row_number(),2)) %>%
    mutate(this_week=max(Week_no),
           week_label=ifelse(Week_no==this_week, "thisweek","lastweek")) %>%
    select(Jurisdiction,week_label,avg_tests_per_day,percent_positive) %>%
    pivot_longer(cols = c("avg_tests_per_day","percent_positive"),
                 names_to="type",
                 values_to="value") %>%
    pivot_wider(names_from = c(week_label, type),
                names_glue= "{type}_{week_label}" ,
                values_from=value) %>%
    mutate(change_in_tests=(avg_tests_per_day_thisweek-avg_tests_per_day_lastweek)/avg_tests_per_day_lastweek,
           change_in_positivity=(percent_positive_thisweek-percent_positive_lastweek)/percent_positive_lastweek) %>%
    select(Jurisdiction,avg_tests_per_day_thisweek,avg_tests_per_day_lastweek,change_in_tests,percent_positive_thisweek,percent_positive_lastweek,change_in_positivity  )
  
  this_week_num<-max(Testing$Week_no)
  this_week_label<-unique(Testing$Week[Testing$Week_no==this_week_num])
  last_week_num<-max(Testing$Week_no)-1
  last_week_label<-unique(Testing$Week[Testing$Week_no==last_week_num])
  
  juriorder <- c("Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut")
  Testing_Metrics <- Testing_Metrics %>%
    rename(!!paste0("Average Daily Tests (",this_week_label,")") := avg_tests_per_day_thisweek,
           !!paste0("Percent Positivity (",this_week_label,")") := percent_positive_thisweek,
           !!paste0("Average Daily Tests (",last_week_label,")") := avg_tests_per_day_lastweek,
           !!paste0("Percent Positivity (",last_week_label,")") := percent_positive_lastweek,
    ) %>%
    mutate(Jurisdiction =  factor(Jurisdiction, levels = juriorder)) %>%
    arrange(Jurisdiction)

#Creating "key_" R variables for inclusion in the slide text of the .Rmd
key_Can_weekly_tests<-Testing$week_tests_performed[Testing$Week_no==this_week_num&Testing$Jurisdiction=="Canada"]
key_Can_weekly_perc_positive<-percent(Testing$percent_positive[Testing$Week_no==this_week_num&Testing$Jurisdiction=="Canada"], accuracy = 0.1)



# Create dataset for daily testing

SALT2a<-SALT %>%
  mutate( Date = as.Date(str_sub(Report.Date, 1, 10)),
    Time = as_hms(str_sub(Report.Date, 13, 20)),
    datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"))%>%
  mutate(Current_week = ifelse(date(Date) + 7 <= max(Date), "No", "Yes")) %>%
  arrange(Jurisdiction, datetime)

SALT3a <- SALT2a %>%
  group_by(Jurisdiction,Date) %>%
  filter(datetime==max(datetime)) 

SALT4a <- SALT3a %>%
  group_by(Jurisdiction) %>%
  mutate(daily_tests_performed = Tests.Performed-lag(Tests.Performed),
         daily_tests_positive = Positive.Test.Results-lag(Positive.Test.Results),
         daily_tests_negative = daily_tests_performed-daily_tests_positive,
         percent_positive = daily_tests_positive/daily_tests_performed)



PTs_missing_latest_lab_date<-SALT4a %>%
  group_by(Jurisdiction) %>%
  filter(Date==max(Date)) %>%
  ungroup %>%
  filter(!Date==max(Date)) %>%
  recode_PT_names_to_small(geo_variable = "Jurisdiction") %>%
  mutate(Date=format(Date, "%b %d")) %>%
  mutate(text_var=paste0(Jurisdiction, " (last reported: ",Date,")")) %>%
  select(text_var)

key_lab_update<-format(max(SALT2a$Date)-1, "%B %d")

if (nrow(PTs_missing_latest_lab_date>0)){
any_PTs_missing_latest_lab_date_flag=TRUE
  key_PTs_missing_latest_lab_date<-turn_char_vec_to_comma_list(PTs_missing_latest_lab_date[[1]])
} else{
  any_PTs_missing_latest_lab_date_flag=FALSE
}


National_Daily_a <- SALT4a %>%
  select(Date, daily_tests_performed, daily_tests_positive, daily_tests_negative) %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric),sum)) %>%
  mutate(Jurisdiction="Canada") %>%
  mutate(percent_positive = daily_tests_positive/daily_tests_performed) %>%
  arrange(Date)

#Question about whether or not the "daily tests performed" should be a 7dMA. I think in SAS right now it isn't.
National_Daily <- National_Daily_a %>%
  # mutate(tests_performed=rollmean(daily_tests_performed,k=7,fill=NA,align="right")) %>%
  mutate(tests_performed=daily_tests_performed,
         percent_positive=rollmean(percent_positive,k=7,fill=NA,align="right")) %>%
  select(Date,Jurisdiction,tests_performed,percent_positive)  %>%
  filter(Date>"2021-01-23") %>%   #For now adding filter here, as there is two weeks of valid test data in Dec. and then nothing until Jan.24, making for a very weird figure.
  filter(Date<=max(Date)-1) #some PTs are reporting "current date" in SALT in the evenings, will not want to include partial day's worth of data

write.csv(National_Daily, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\testing_daily.csv')

