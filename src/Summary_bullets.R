# Summary bullets code

#################################################### #
################### Reporting #######################
#################################################### #

summary_reporting_header<-"Reporting"
if(any_non_report_flag==FALSE){
  summary_reporting_bullet_1<-"All PTs provided updates today on cases and deaths"
} else {
  summary_reporting_bullet_1<-paste0("All PTs except for ", PHACTrendR::turn_char_vec_to_comma_list(key_PTs_nonreport), " provided updates today on cases and deaths")
}


#################################################### #
##################### Cases #########################
#################################################### #

summary_cases_header<-"Cases"

summary_cases_bullet_1_part_1<-paste0("The 7 day moving average for cases was ",key_national_7MA_cases,", ")

summary_cases_direction=ifelse(str_detect(key_national_weekly_change_cases, "\\+")==TRUE, "increase",
                              ifelse(str_detect(key_national_weekly_change_cases,"\\-")==TRUE, "decrease", "no change"))
if (summary_cases_direction=="no change"){
  summary_cases_bullet_1_part_2<-" no change from the week prior"
} else{
  summary_cases_bullet_1_part_2<-paste0("a ",key_national_weekly_change_cases," ",summary_cases_direction," from the week prior")
}

summary_cases_bullet_1<-c(paste0(summary_cases_bullet_1_part_1, summary_cases_bullet_1_part_2))


summary_cases_bullet_1_sub_1<-paste0(key_sum_PTs_no_increase_cases,"/13 PTs reported a decrease or no change in weekly cases")
if(key_sum_PTs_no_increase_cases<13){
  summary_cases_bullet_1_sub_2<-paste0("Increases were reported by: ",key_PTs_increase_cases)
} else {
  summary_cases_bullet_1_sub_2<-""
}

#Will need to figure out how to automate a bullet about the cases/death points for now keeping consistent message
summary_cases_bullet_2<-"Those aged 80+ no longer have the highest incidence rates nationally"


#################################################### #
##################### Severity #########################
#################################################### #
summary_severity_header<-"Severity"

#deaths
summary_severity_bullet_1_part_1<-paste0("The 7 day moving average for deaths was ",key_national_7MA_deaths,", ")

summary_severity_deaths_direction=ifelse(str_detect(key_national_weekly_change_deaths, "\\+")==TRUE, "increase",
                               ifelse(str_detect(key_national_weekly_change_deaths,"\\-")==TRUE, "decrease", "no change"))
if (summary_severity_deaths_direction=="no change"){
  summary_severity_bullet_1_part_2<-" no change from the week prior"
} else{
  summary_severity_bullet_1_part_2<-paste0("a ",key_national_weekly_change_deaths," ",summary_severity_deaths_direction," from the week prior")
}

summary_severity_bullet_1<-c(paste0(summary_severity_bullet_1_part_1, summary_severity_bullet_1_part_2))
rm(summary_severity_bullet_1_part_1, summary_severity_bullet_1_part_2, summary_severity_deaths_direction)

summary_severity_bullet_1_sub_1<-paste0(key_sum_PTs_no_increase_deaths,"/13 PTs reported a decrease or no change in weekly deaths")
if(key_sum_PTs_no_increase_deaths<13){
  summary_severity_bullet_1_sub_2<-paste0("Increases were reported by: ",key_PTs_increase_deaths)
} else {
  summary_severity_bullet_1_sub_2<-""
}

#hospitalizations
summary_severity_bullet_2_part1<-paste0("The 7 day moving average for hospitalizations was ",scales::comma(key_national_7MA_hosp),", ")
summary_severity_hosp_direction=ifelse(str_detect(key_national_hosp_change, "\\+")==TRUE, "increase",
                                         ifelse(str_detect(key_national_hosp_change,"\\-")==TRUE, "decrease", "no change"))
if (summary_severity_hosp_direction=="no change"){
  summary_severity_bullet_2_part_2<-" no change from the week prior"
} else{
  summary_severity_bullet_2_part_2<-paste0("a ",key_national_hosp_change," ",summary_severity_hosp_direction," from the week prior")
}

summary_severity_bullet_2<-paste0(summary_severity_bullet_2_part1, summary_severity_bullet_2_part_2)
rm(summary_severity_bullet_2_part1, summary_severity_bullet_2_part_2, summary_severity_hosp_direction)

summary_severity_bullet_2_sub_1<-paste0(key_sum_PTs_no_increase_hosp,"/13 PTs reported a decrease or no change in weekly hospitalizations")
if(key_sum_PTs_no_increase_hosp<13){
  summary_severity_bullet_2_sub_2<-paste0("Increases were reported by: ",key_PTs_increase_hosp)
} else {
  summary_severity_bullet_2_sub_2<-""
}

#ICU
summary_severity_bullet_3_part1<-paste0("The 7 day moving average for ICU counts was ",scales::comma(key_national_7MA_icu),", ")
summary_severity_icu_direction=ifelse(str_detect(key_national_icu_change, "\\+")==TRUE, "increase",
                                       ifelse(str_detect(key_national_icu_change,"\\-")==TRUE, "decrease", "no change"))
if (summary_severity_icu_direction=="no change"){
  summary_severity_bullet_3_part_2<-" no change from the week prior"
} else{
  summary_severity_bullet_3_part_2<-paste0("a ",key_national_icu_change," ",summary_severity_icu_direction," from the week prior")
}

summary_severity_bullet_3<-paste0(summary_severity_bullet_3_part1, summary_severity_bullet_3_part_2)
rm(summary_severity_bullet_3_part1, summary_severity_bullet_3_part_2, summary_severity_icu_direction)

summary_severity_bullet_3_sub_1<-paste0(key_sum_PTs_no_increase_icu,"/13 PTs reported a decrease or no change in weekly hospitalizations")
if(key_sum_PTs_no_increase_icu<13){
  summary_severity_bullet_3_sub_2<-paste0("Increases were reported by: ",key_PTs_increase_icu)
} else {
  summary_severity_bullet_3_sub_2<-""
}

######################################################################################################## #
#####################       Laboratory testing (to be included on Monday's)    ###########################
######################################################################################################## #

summary_lab_header<-"Laboratory Testing"

summary_lab_bullet_1<-"Note: lab testing numbers may vary slightly day to day as PTs continually update lab testing data"

summary_lab_bullet_2_part_1<-paste0("There was an average of ",key_Can_avg_tests_per_day," daily tests last week (",this_week_label,"), ")

summary_lab_testing_direction=ifelse(str_detect(key_Can_avg_tests_change, "\\+")==TRUE, "increase",
                                      ifelse(str_detect(key_Can_avg_tests_change,"\\-")==TRUE, "decrease", "no change"))
if (summary_lab_testing_direction=="no change"){
  summary_lab_bullet_2_part_2<-" no change from the week prior"
} else{
  summary_lab_bullet_2_part_2<-paste0("a ",key_Can_avg_tests_change," ",summary_lab_testing_direction," from the week prior")
}
summary_lab_bullet_2<-paste0(summary_lab_bullet_2_part_1, summary_lab_bullet_2_part_2)
rm(summary_lab_bullet_2_part_1, summary_lab_bullet_2_part_2, summary_lab_testing_direction)

summary_lab_bullet_3_part_1<-paste0("The national percent positivity was ",key_Can_weekly_perc_positive," last week (",this_week_label,"), ")

summary_lab_perc_positive_direction=ifelse(str_detect(key_Can_weekly_perc_positive_change, "\\+")==TRUE, "increase",
                                     ifelse(str_detect(key_Can_weekly_perc_positive_change,"\\-")==TRUE, "relative decrease", "no change"))
if (summary_lab_perc_positive_direction=="no change"){
  summary_lab_bullet_3_part_2<-" no change from the week prior"
} else{
  summary_lab_bullet_3_part_2<-paste0("a ",key_Can_weekly_perc_positive_change," ",summary_lab_perc_positive_direction," from the week prior")
}
summary_lab_bullet_3<-paste0(summary_lab_bullet_3_part_1, summary_lab_bullet_3_part_2)
rm(summary_lab_bullet_3_part_1, summary_lab_bullet_3_part_2, summary_lab_perc_positive_direction)

######################################################################################################## #
#####################           International Trends (to be included Thursdays)    ###########################
######################################################################################################## #

summary_international_header<-"International Trends"

summary_international_bullet_1<-paste0("The countries with the highest 7MA incidence rates (per million): ",key_int_top_3_cases)
summary_international_bullet_2<-paste0("The countries with the highest 7MA death rates (per million): ",key_int_top_3_deaths)

#################################################### #
##################### Testing summary bullets #########################
#################################################### #
# 
# summary_reporting_header
# summary_reporting_bullet_1
# 
# summary_cases_header
# 
# summary_cases_bullet_1
# summary_cases_bullet_1_sub_1
# summary_cases_bullet_1_sub_2
# 
# summary_cases_bullet_2
# 
# 
# summary_severity_header
# 
# summary_severity_bullet_1
# summary_severity_bullet_1_sub_1
# summary_severity_bullet_1_sub_2
# 
# summary_severity_bullet_2
# summary_severity_bullet_2_sub_1
# summary_severity_bullet_2_sub_2
# 
# summary_severity_bullet_3
# summary_severity_bullet_3_sub_1
# summary_severity_bullet_3_sub_2
# 
# summary_lab_header
# summary_lab_bullet_1
# summary_lab_bullet_2
# summary_lab_bullet_3
# 
# summary_international_header
# summary_international_bullet_1
# summary_international_bullet_2
# 
# 
# #Gating certain bullets based on day of week
# day_of_week=weekdays(Sys.Date())

