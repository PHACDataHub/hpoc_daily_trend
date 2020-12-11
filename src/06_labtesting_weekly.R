SALT <- salt_raw

SALT2 <- SALT %>%
    mutate(Date=as.Date(str_sub(Report.Date,1,10))) %>%
    mutate(Time=as_hms(str_sub(Report.Date,13,20))) %>%
    mutate(datetime=strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S")) %>%
    mutate(Start_of_week=floor_date(Date, "week")) %>%
    mutate(End_of_week=date(Start_of_week)+6) %>%
    mutate(Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week))) %>%
    mutate(Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
    filter(Date <= floor_date(max(Date), "week")-1) %>%
    mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
    arrange(Jurisdiction,datetime)

SALT3 <- SALT2 %>%
    group_by(Jurisdiction,Week) %>%
    filter(datetime==max(datetime))

SALT4 <- SALT3 %>%
    group_by(Jurisdiction) %>%
    mutate(Week_patients_tested = Patients.Tested-lag(Patients.Tested)) %>%
    mutate(Week_confirmed_positive = Confirmed.Positive-lag(Confirmed.Positive)) %>%
    mutate(Week_confirmed_negative = Confirmed.Negative-lag(Confirmed.Negative)) %>%
    mutate(Avg_tests_per_day = Week_patients_tested/7) %>%
    mutate(Percent_positive = Week_confirmed_positive/Week_patients_tested)

National <- SALT4 %>%
    select(Week, Start_of_week, Week_patients_tested, Week_confirmed_positive, Week_confirmed_negative) %>%
    group_by(Week, Start_of_week) %>%
    summarise(across(where(is.numeric),sum)) %>%
    mutate(Jurisdiction="Canada") %>%
    mutate(Percent_positive = Week_confirmed_positive/Week_patients_tested) %>%
    mutate(Avg_tests_per_day = Week_patients_tested/7) %>%
    arrange(Start_of_week)
  
National <- subset(National, select=-Start_of_week)

National <- National[,c(1,5,2,3,4,6,7)]

Provincial <- SALT4 %>%
    select(Week, Jurisdiction, Week_patients_tested, Week_confirmed_positive, Week_confirmed_negative, Percent_positive, Avg_tests_per_day)

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

write.csv(Testing, 'C:\\Users\\FISLAM\\Documents\\testing.csv')

Testing_Metrics <- Testing %>%
  group_by(Jurisdiction) %>%
  slice(tail(row_number(),2)) %>%
  select(Jurisdiction,Week,Avg_tests_per_day,Percent_positive) %>%
  mutate(Avg_tests_per_day = round(Avg_tests_per_day)) %>%
  mutate(Avg_tests_per_day = number(Avg_tests_per_day,big.mark=",")) %>%
  mutate(Percent_positive = percent(Percent_positive,accuracy = 0.1)) %>%
  rename("Average # of people tested daily" = Avg_tests_per_day) %>%
  rename("% positive" = Percent_positive)
  
Testing_Metrics <- Testing_Metrics %>%
  gather(key,value,-Jurisdiction,-Week) %>%
  unite(new.col, c(key,Week), sep=" ") %>%
  spread(new.col,value)

Testing_Metrics <- Testing_Metrics[,c(1,5,4,3,2)]