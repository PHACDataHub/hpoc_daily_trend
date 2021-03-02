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
  summary_cases_bullet_1_part_2<-paste0("a ",key_national_weekly_change_cases," ",summary_case_direction," from the week prior")
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
rm(summary_severity_bullet_1_part_1, summary_severity_bullet_1_part_2)

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
rm(summary_severity_bullet_2_part1, summary_severity_bullet_2_part_2)

summary_severity_bullet_2_sub_1<-paste0(key_sum_PTs_no_increase_hosp,"/13 PTs reported a decrease or no change in weekly deaths")




#################################################### #
##################### Laboratory testing #########################
#################################################### #



#################################################### #
##################### International trends #########################
#################################################### #


#################################################### #
##################### Testing summary bullets #########################
#################################################### #

summary_cases_header

summary_cases_bullet_1
summary_cases_bullet_1_sub_1
summary_cases_bullet_1_sub_2

summary_cases_bullet_2


summary_severity_header

summary_severity_bullet_1
summary_severity_bullet_1_sub_1
summary_severity_bullet_1_sub_2

summary_severity_bullet_2
summary_severity_bullet_2_sub_1
