salt_raw<-import_SALT_data()

SALT <- salt_raw %>%
  select(Report.Date,Jurisdiction,Tests.Performed,Positive.Test.Results,Percent.Positive.Test.Results, Latest.Update.Date) %>%
  rename(tests_performed=Tests.Performed,
         positive_tests=Positive.Test.Results,
         percent_positive=Percent.Positive.Test.Results) %>%
  mutate(update_date = as.Date(str_sub(Latest.Update.Date, 1, 10)),
         Date = as.Date(str_sub(Report.Date, 1, 10)),
         Time = as_hms(str_sub(Report.Date, 13, 20)),
         datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3)))


SALT2 <- SALT %>%
  select(-Latest.Update.Date,-update_date)%>%
    mutate(Start_of_week=floor_date(Date, "week"),
           End_of_week=date(Start_of_week)+6,
           Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week)),
           Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
    filter(Date <= floor_date(max(Date), "week")-1) %>%
    mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
  filter(Date>="2021-01-23") %>% #Issues with historical data missing for some PTs - only taking last two weeks data for now.
    arrange(Jurisdiction,datetime)

SALT3 <- SALT2 %>%
  group_by(Jurisdiction,Week, Start_of_week) %>%
  summarise(days_reported=n(),
            week_tests_performed=sum(tests_performed),
            week_positive_tests=sum(positive_tests),
            .groups="drop_last") %>%
  mutate(week_negative_tests=week_tests_performed-week_positive_tests,
         week_percent_positive=round(week_positive_tests/week_tests_performed,digits = 4),
         avg_tests_per_day=round(week_tests_performed/days_reported))

National <- SALT3 %>%
    select(Week, Jurisdiction, Start_of_week, week_tests_performed, week_positive_tests, week_negative_tests, days_reported, avg_tests_per_day) %>%
    group_by(Week, Start_of_week) %>%
    summarise(across(where(is.numeric),sum),
              .groups="drop_last") %>%
    mutate(Jurisdiction="Canada",
           week_percent_positive = week_positive_tests/week_tests_performed) %>%
    arrange(Start_of_week) %>%
  select(Week, Jurisdiction, week_tests_performed, week_positive_tests, week_negative_tests, week_percent_positive, avg_tests_per_day, days_reported)

Provincial <- SALT3 %>%
  arrange(Start_of_week) %>%
    select(Week, Jurisdiction, week_tests_performed, week_positive_tests, week_negative_tests, week_percent_positive, avg_tests_per_day, days_reported)

Testing <- rbind(National,Provincial) %>%
  tidyr::drop_na() %>%
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
  slice(tail(row_number(),12)) %>%
  select(Week_no, Week, Jurisdiction, week_tests_performed,week_positive_tests,week_negative_tests,week_percent_positive,avg_tests_per_day, days_reported)
    

#to replace the export that used to go to Yann Pelchat - awaiting confirmation that it is still needed.
# export_testing1<-Testing %>%
#   select(Jurisdiction, Week, week_tests_performed, week_positive_tests)%>%
#   filter(Jurisdiction=="Canada")
# write.csv(export_testing1)

export_testing2<-Testing %>%
  select(Jurisdiction, Week, week_tests_performed, week_positive_tests, week_percent_positive, avg_tests_per_day) %>%
  rename(Week_tests_performed=week_tests_performed,
         Week_positive_tests=week_positive_tests,
         Percent_Positive=week_percent_positive,
         Average_daily_tests=avg_tests_per_day)
write.csv(export_testing2, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Data Analysis\\NML_weekly_testing.csv')

#For use in python script - code blocked out for now as not being used in trend report.
# write.csv(Testing, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\testing.csv')

  Testing_Metrics <- Testing %>%
    group_by(Jurisdiction) %>%
    slice(tail(row_number(),2)) %>%
    mutate(this_week=max(Week_no),
           week_label=ifelse(Week_no==this_week, "thisweek","lastweek")) %>%
    select(Jurisdiction,week_label,avg_tests_per_day,week_percent_positive) %>%
    pivot_longer(cols = c("avg_tests_per_day","week_percent_positive"),
                 names_to="type",
                 values_to="value") %>%
    pivot_wider(names_from = c(week_label, type),
                names_glue= "{type}_{week_label}" ,
                values_from=value) %>%
    mutate(change_in_tests=(avg_tests_per_day_thisweek-avg_tests_per_day_lastweek)/avg_tests_per_day_lastweek,
           change_in_positivity=(week_percent_positive_thisweek-week_percent_positive_lastweek)/week_percent_positive_lastweek) %>%
    select(Jurisdiction,avg_tests_per_day_thisweek,avg_tests_per_day_lastweek,change_in_tests,week_percent_positive_thisweek,week_percent_positive_lastweek,change_in_positivity  )
  
  this_week_num<-max(Testing$Week_no)
  this_week_label<-unique(Testing$Week[Testing$Week_no==this_week_num])
  last_week_num<-max(Testing$Week_no)-1
  last_week_label<-unique(Testing$Week[Testing$Week_no==last_week_num])
  
  Testing_Metrics <- Testing_Metrics %>%
    rename(!!paste0("Average Daily Tests (",this_week_label,")") := avg_tests_per_day_thisweek,
           !!paste0("Percent Positivity (",this_week_label,")") := week_percent_positive_thisweek,
           !!paste0("Average Daily Tests (",last_week_label,")") := avg_tests_per_day_lastweek,
           !!paste0("Percent Positivity (",last_week_label,")") := week_percent_positive_lastweek,
    ) %>%
    factor_PT_west_to_east(size="big",Canada_first = TRUE) %>%
    arrange(Jurisdiction)



# Create dataset for daily testing

SALT2a<-SALT %>%
  select(-Latest.Update.Date,-update_date)%>%
  mutate(Current_week = ifelse(date(Date) + 7 <= max(Date), "No", "Yes")) %>%
  arrange(Jurisdiction, datetime)

SALT3a <- SALT2a %>%
  group_by(Jurisdiction,Date) %>%
  filter(datetime==max(datetime)) 

SALT4a <- SALT3a %>%
  group_by(Jurisdiction) %>%
  rename(daily_tests_performed=tests_performed,
         daily_tests_positive=positive_tests) %>%
  mutate(daily_tests_negative=daily_tests_performed-daily_tests_positive,
         percent_positive=daily_tests_positive/daily_tests_performed)




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



##########
#Creating "key_" R variables for inclusion in the lab slide text, and summary slide of the .Rmd


can_current_lab_testing<-Testing %>%
  filter(Jurisdiction=="Canada"&Week_no==max(Week_no))

can_last_week_lab_testing<-Testing%>%
  filter(Jurisdiction=="Canada"&Week_no==max(Week_no)-1)


key_Can_weekly_tests<-scales::comma(can_current_lab_testing$week_tests_performed)
key_Can_avg_tests_per_day<-scales::comma(can_current_lab_testing$avg_tests_per_day)
key_Can_weekly_perc_positive<-percent(can_current_lab_testing$week_percent_positive, accuracy = 0.1)

key_Can_avg_tests_change<-PHACTrendR::turn_num_to_percent_change((can_current_lab_testing$avg_tests_per_day-can_last_week_lab_testing$avg_tests_per_day)/can_last_week_lab_testing$avg_tests_per_day)
key_Can_weekly_perc_positive_change<-PHACTrendR::turn_num_to_percent_change((can_current_lab_testing$week_percent_positive-can_last_week_lab_testing$week_percent_positive)/can_last_week_lab_testing$week_percent_positive)

PTs_missing_lab_days_current_week<-SALT3 %>%
  group_by(Jurisdiction) %>%
  filter(Start_of_week==max(Start_of_week) & !days_reported==7) %>%
  recode_PT_names_to_small() %>%
  mutate(text_var=paste0(Jurisdiction," (",days_reported," days of reporting)")) %>%
  ungroup() %>%
  select(text_var) %>%
  as.character()

any_PTs_missing_current_week_lab_days_flag<-(length(PTs_missing_lab_days_current_week)>0)
if (any_PTs_missing_current_week_lab_days_flag==TRUE){
  key_labtesting_table_footnote<-paste0("The following PTs did not report all 7 days in the current week: ",PHACTrendR::turn_char_vec_to_comma_list(PTs_missing_lab_days_current_week))
}

# For footnote on python figure (daily testing)
PTs_missing_latest_lab_date<-SALT4a %>%
  group_by(Jurisdiction) %>%
  filter(Date==max(Date)) %>%
  ungroup %>%
  filter(!Date==max(Date)) %>%
  recode_PT_names_to_small(geo_variable = "Jurisdiction") %>%
  mutate(Date=format(Date, "%b %d")) %>%
  mutate(text_var=paste0(Jurisdiction, " (last reported: ",Date,")")) %>%
  select(text_var) %>%
  as.character()

key_lab_update<-format(max(SALT$update_date), "%B %d")

any_PTs_missing_latest_lab_date_flag<-(length(PTs_missing_latest_lab_date)>0)

if (any_PTs_missing_latest_lab_date_flag==TRUE){
  key_PTs_missing_latest_lab_date<-turn_char_vec_to_comma_list(PTs_missing_latest_lab_date)
}