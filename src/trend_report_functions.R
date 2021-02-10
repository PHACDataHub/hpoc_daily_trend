## Run Trend Report script


#Script to generate Daily Trend Report!
## This is currently broken, will look into this at some point.
generate_trend_report<-function(report_date=""){

if (report_date==""){
  report_date<-format(Sys.Date(), "%d%b%Y")
}
library("rmarkdown")
setwd("C:/rmd/")

rmarkdown::render('daily-trend-report-v3.rmd',
                  output_file = paste0('DailyTrendReport_', report_date,'.pptx'))
}

############################################################################################################################################ #
############################################################################################################################################ #


# correct_df() is a function to be able to more elegantly make corrections to the data.
# data= ""            - dataset you wish to correct. df_corrected data set as default (probably should be forced choice)
# metric= ""          - choice between updating "cases" or "deaths" = other text inputs will be ignored
# Jurisdiction=""     - region that is being corrected. Currently takes only one input by design
# correction_date=""  - date that you want to make correction for
# corrected_value=""  - new input value that you want to make

correct_df<-function(data=df_corrected,metric="",Jurisdiction="",correction_date="",corrected_value=""){
  correction_date=as.Date(correction_date)
  if (metric=="cases"){
    data[data$prname==Jurisdiction & data$date==correction_date, "numtoday"]<-corrected_value
  }else if (metric=="deaths"){
    data[data$prname==Jurisdiction & data$date==correction_date, "numdeathstoday"]<-corrected_value
  }
  return(data)
}


############################################################################################################################################ #
############################################################################################################################################ #

## Format tables 

format_casedeath_table<-function(input_table){

  input_table<-input_table %>%
    mutate(Weekly_Change_Cases=percent(Weekly_Change_Cases,accuracy=0.1),
           National_Case_Proportion=percent(National_Case_Proportion,accuracy=0.1),
           Weekly_Change_Deaths=percent(Weekly_Change_Deaths,accuracy=0.1),
           National_Death_Proportion=percent(National_Death_Proportion,accuracy=0.1),
           Deaths_Daily_7MA=round(Deaths_Daily_7MA,1),
           Cases_Daily_7MA=ifelse(Cases_Daily_7MA<1, number(Cases_Daily_7MA, accuracy = 0.1), number(Cases_Daily_7MA, big.mark = ",")),
           Weekly_Change_Cases=case_when(Weekly_Change_Cases>0 ~ paste0("+",Weekly_Change_Cases),
                                         is.na(Weekly_Change_Cases) ~ "NA",
                                         TRUE ~ as.character(Weekly_Change_Cases)),
           Weekly_Change_Deaths=case_when(Weekly_Change_Deaths>0 ~ paste0("+",Weekly_Change_Deaths),
                                         is.na(Weekly_Change_Deaths) ~ "NA",
                                         TRUE ~ as.character(Weekly_Change_Deaths)))
  
  ft <- flextable(input_table)
  ft <- color(ft, j = "Weekly_Change_Cases", i = ~ str_detect(Weekly_Change_Cases, "\\+"), color="red")
  ft <- color(ft, j = "Weekly_Change_Cases", i = ~ str_detect(Weekly_Change_Cases, "\\-"), color="green4")
  ft <- color(ft, j = "Weekly_Change_Deaths", i = ~ str_detect(Weekly_Change_Deaths, "\\+"), color="red")
  ft <- color(ft, j = "Weekly_Change_Deaths", i = ~ str_detect(Weekly_Change_Deaths, "\\-"), color="green4")
  ft <- set_header_labels(ft, Cases_Daily="Daily Cases", Cases_Daily_7MA="7 Day MA, Cases",Cases_7MA_per100k="7 Day cases MA per 100,000",Weekly_Change_Cases="Weekly Change in Cases",National_Case_Proportion="National Proportion of Cases",Deaths_Daily="Daily Deaths",Deaths_Daily_7MA="7 Day MA, Deaths",Deaths_7MA_per100k="7 Day deaths MA per 100,000",Weekly_Change_Deaths="Weekly Change in Deaths",National_Death_Proportion="National Proportion of Deaths")
  #ft <- width(ft, width=1.2)
  ft <- height(ft, height=.52, part = "header")
  ft <- height(ft, height=.26, part = "body")
  ft <- fontsize(ft, size = 11, part="all")
  #ft <- fontsize(ft, size = 12, part="body")
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, j = 1:2,align = "left", part="body")
  ft <- align(ft, j=3:ncol(input_table), align = "right", part="body")
  #Fill colours - first two columns in green
  ft<-bg(ft,j=1:2, bg="#9bbb59", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=1:2, bg="#d7e4bd")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=1:2, bg="#ebf1de")
  #columns 3-7 in blue
  ft<-bg(ft,j=3:7, bg="#17375e", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=3:7, bg="#b9cde5")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=3:7, bg="#dce6f2")
  #columns 8-12 in grey
  ft<-bg(ft,j=8:12, bg="#7f7f7f", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=8:12, bg="#d9d9d9")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=8:12, bg="#f2f2f2")
  #Header colour to white
  ft<-color(ft,color="white",part="header")
  big_border = fp_border(color="white", width=2)
  border_v = fp_border(color="white")
  border_h = fp_border(color="white")
  ft <- border_outer(ft, part = "all", border = big_border)
  ft <- border_inner_v(ft, part = "all", border = border_v)
  ft <- border_inner_h(ft, part = "all", border = border_h)
  
  ft_1 <- ft %>% footnote(., i=1, j=6, value = as_paragraph(
    c("Weekly change is how the current week (days 1 to 7 days ago) compares against the previous week (8 to 14 days ago).  e.g. previous week = 75 total cases; current week = 50 total cases; weekly percent change from previous week to current week = 33.3% decrease (-33.3%). 'NA' is used when there were no cases or deaths during either week, 'Inf' is used when there were is at least one case or death during the current week, but none the prior week"#,
      #"Source: Provincial and territorial website data")
    )),
    ref_symbols = c("*"),
    part = "header") %>%
    footnote(., i=1, j=11, value = as_paragraph(
      c("Weekly change is how the current week (days 1 to 7 days ago) compares against the previous week (8 to 14 days ago).  e.g. previous week = 75 total cases; current week = 50 total cases; weekly percent change from previous week to current week = 33.3% decrease (-33.3%). 'NA' is used when there were no cases or deaths during either week, 'Inf' is used when there were is at least one case or death during the current week, but none the prior week"#,
        #"Source: Provincial and territorial website data")
      )),
      ref_symbols = c("*"),
      part = "header") %>% merge_v(part="footer") %>%
    footnote(., value = as_paragraph(
      if(any_non_report_flag==FALSE){
        "All PTs provided daily updates today"
      } else {
        paste0("Note that daily updates were not provided by: ",turn_char_vec_to_comma_list(key_PTs_nonreport),". 7MA of cases and deaths for these PTs are based on the date of last report. For calculation of the national 7MA of cases and deaths, days after the date of last report for these PTs were omitted.")
      }),
      ref_symbols = c(""),
      part = "header") %>%
    
    footnote(., value = as_paragraph(
      c("Source: Provincial and territorial website data. ")
    ),
    ref_symbols = c(""),
    part = "header") %>%
    
    footnote(., value = as_paragraph(
      paste0("Updated daily (Sun-Thurs). Data as of: ",max(Case_Death_Stats$Date))),
      ref_symbols = c(""),
      part = "header")
  
  #ft_1 <- autofit(ft_1)
  
  return(ft_1)
}

############################################################################################################################################ #
############################################################################################################################################ #

format_hospicu_table<-function(input_table){
  input_table<-input_table %>%
    mutate(Date=format(Date, "%B %d"),
           delta7h=case_when(delta7h>0 ~ paste0("+",delta7h),
                                         is.na(delta7h) ~ "NA",
                                         TRUE ~ as.character(delta7h)),
           delta7i=case_when(delta7i>0 ~ paste0("+",delta7i),
                                          is.na(delta7i) ~ "NA",
                                          TRUE ~ as.character(delta7i)))
  
  
  ft <- flextable(input_table)
  ft <- color(ft, j = "delta7h", i = ~ str_detect(delta7h, "\\+"), color="red")
  ft <- color(ft, j = "delta7h", i = ~ str_detect(delta7h, "\\-"), color="green4")
  ft <- color(ft, j = "delta7i", i = ~ str_detect(delta7i, "\\+"), color="red")
  ft <- color(ft, j = "delta7i", i = ~ str_detect(delta7i, "\\-"), color="green4")
  ft <- set_header_labels(ft, hosp7ma="Hospitalizations, 7 Day MA", delta7h="Weekly Change in Hospitalizations",icu7ma="ICU, 7 Day MA",delta7i="Weekly Change in ICU")
  #ft <- width(ft, width=1.2)
  ft <- height(ft, height=.39, part="header")
  ft <- height(ft, height=.26, part="body")
  ft <- fontsize(ft, size = 12, part="all")
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, j = 1:2,align = "left", part="body")
  ft <- align(ft, j=3:ncol(input_table), align = "right", part="body")
  
  #shading headers, and alternating rows.
  ft<-bg(ft, bg="#4f81bd", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2), bg="#d0d8e8")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2), bg="#e8edf4")
  ft<-color(ft,color="white",part="header")
  
  big_border = fp_border(color="black", width=2)
  border_v = fp_border(color="black")
  border_h = fp_border(color="black")
  
  ft <- border_outer(ft, part = "all", border = big_border)
  ft <- border_inner_v(ft, part = "all", border = border_v)
  ft <- border_inner_h(ft, part = "all", border = border_h)
  ft <- autofit(ft)
  
  ft_1 <- footnote( ft, value = as_paragraph(
    c("Source: Provincial and territorial website data. When a PT does not report updated hospitalization/ICU numbers, the previous day's values are carried over.")),
    ref_symbols = c("")) %>%
    footnote(., value = as_paragraph(
      paste0("Updated Daily (Sun-Thurs). Data as of: ",format(max(Hosp_Metrics_Table$Date),"%B %d"))),
      ref_symbols = c(""),
      part = "header")
  
  #ft_1 <- autofit(ft_1)
  
  return(ft_1)
}

############################################################################################################################################ #
############################################################################################################################################ #

#Will update this function to be more like the other format_X_table functions next week when all columns are able to be completed.
format_labtesting_table<-function(input_table){

  input_table <- input_table %>%
    mutate(across(contains("Average"),number, big.mark=","),
           across(contains("Percent"),label_percent(accuracy = 0.1)),
           across(contains("change"),label_percent(accuracy = 0.1)),
           change_in_tests=case_when(change_in_tests>0 ~ paste0("+",change_in_tests),
                             is.na(change_in_tests) ~ "NA",
                             TRUE ~ as.character(change_in_tests)),
           change_in_positivity=case_when(change_in_positivity>0 ~ paste0("+",change_in_positivity),
                             is.na(change_in_positivity) ~ "NA",
                             TRUE ~ as.character(change_in_positivity))) %>%
    rename(`Weekly Change in Tests` = change_in_tests,
           `Weekly Change in Percent Positivity` = change_in_positivity)
  
  
ft <- flextable(input_table)
ft <- width(ft, width=1.2)
ft <- height_all(ft, height=.26)
ft <- fontsize(ft, size = 14, part="header")
ft <- fontsize(ft, size = 12, part="body")
ft <- align(ft, align = "center", part="header")
ft <- align(ft, j = 1,align = "left", part="body")
ft <- align(ft, j=2:ncol(input_table), align = "right", part="body")

big_border = fp_border(color="black", width=2)
border_v = fp_border(color="black")
border_h = fp_border(color="black")

ft <- border_outer(ft, part = "all", border = big_border)
ft <- border_inner_v(ft, part = "all", border = border_v)
ft <- border_inner_h(ft, part = "all", border = border_h)

#shading headers, and alternating rows.
ft<-bg(ft, bg="#4f81bd", part="header")
ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2), bg="#d0d8e8")
ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2), bg="#e8edf4")
ft<-color(ft,color="white",part="header")

#commented out the below for this week as causing program to crash due to non-existent columns
#add conditional colour to change in testing variable 
ft <- color(ft, j = "Weekly Change in Tests", i = ~ str_detect(`Weekly Change in Tests`, "\\-"), color="red")
ft <- color(ft, j = "Weekly Change in Tests", i = ~ str_detect(`Weekly Change in Tests`, "\\+"), color="green4")
ft <- color(ft, j = "Weekly Change in Percent Positivity", i = ~ str_detect(`Weekly Change in Percent Positivity`, "\\+"), color="red")
ft <- color(ft, j = "Weekly Change in Percent Positivity", i = ~ str_detect(`Weekly Change in Percent Positivity`, "\\-"), color="green4")

ft_1 <- footnote(ft, value = as_paragraph(
  paste0("Updated Mondays. Data as of: ", format(max(SALT4$Date), "%B %d"))),
  ref_symbols = c(""),
  part = "header")  

ft_1 <- autofit(ft_1)

return(ft_1)
}


#############################

## Recode PT names

recode_PT_names_to_small <- function(dataset, varname = "") {
  if (class(dataset)[1]=="character"){
    dataset<-recode(dataset, 
                              "British Columbia"="BC",
                              "Alberta" = "AB",
                              "Saskatchewan"="SK",
                              "Manitoba"="MB",
                              "Ontario"="ON",
                              "Quebec"="QC",
                              "Newfoundland and Labrador"="NL",
                              "New Brunswick"="NB",
                              "Nova Scotia"="NS",
                              "Prince Edward Island"="PE",
                              "Yukon"="YK",
                              "Northwest Territories"="NT",
                              "Nunavut"="NU",
                              "Canada"="CA")
  }
  else{
  dataset <- dataset %>%
    mutate(
      !!varname := case_when(
        !!as.name(varname) == "British Columbia" ~ "BC",
        !!as.name(varname) == "Alberta" ~ "AB",
        !!as.name(varname) == "Saskatchewan" ~ "SK",
        !!as.name(varname) == "Manitoba" ~ "MB",
        !!as.name(varname) == "Ontario" ~ "ON",
        !!as.name(varname) == "Quebec" ~ "QC",
        !!as.name(varname) == "Newfoundland and Labrador" ~ "NL",
        !!as.name(varname) == "New Brunswick" ~ "NB",
        !!as.name(varname) == "Nova Scotia" ~ "NS",
        !!as.name(varname) == "Prince Edward Island" ~ "PE",
        !!as.name(varname) == "Yukon" ~ "YK",
        !!as.name(varname) == "Northwest Territories" ~ "NT",
        !!as.name(varname) == "Nunavut" ~ "NU",
        !!as.name(varname) == "Canada" ~ "CA",
        TRUE~!!as.name(varname)))
  }
  return(dataset)
}
  
recode_PT_names_to_big <- function(dataset, varname = "") {
  if (class(dataset)[1]=="character"){
    dataset<-recode(dataset, 
                    "BC" = "British Columbia",
                    "AB" = "Alberta",
                    "SK" = "Saskatchewan",
                    "MB" = "Manitoba",
                    "ON" = "Ontario",
                    "QC" = "Quebec",
                    "NL" = "Newfoundland and Labrador",
                    "NB" = "New Brunswick",
                    "NS" = "Nova Scotia",
                    "PE" = "Prince Edward Island",
                    "YK" = "Yukon",
                    "NT"= "Northwest Territories",
                    "NU" = "Nunavut",
                    "CA" = "Canada")
  }
  else{
  dataset <- dataset %>%
    mutate(
      !!varname := case_when(
        !!as.name(varname) ==  "BC" ~ "British Columbia",
        !!as.name(varname) ==  "AB" ~ "Alberta",
        !!as.name(varname) ==  "SK" ~ "Saskatchewan",
        !!as.name(varname) ==  "MB" ~ "Manitoba",
        !!as.name(varname) ==  "ON" ~ "Ontario",
        !!as.name(varname) ==  "QC" ~ "Quebec",
        !!as.name(varname) ==  "NL" ~ "Newfoundland and Labrador",
        !!as.name(varname) ==  "NB" ~ "New Brunswick",
        !!as.name(varname) ==  "NS" ~"Nova Scotia",
        !!as.name(varname) ==  "PE" ~ "Prince Edward Island",
        !!as.name(varname) ==  "YK" ~ "Yukon",
        !!as.name(varname) ==  "NT" ~ "Northwest Territories",
        !!as.name(varname) ==  "NU" ~ "Nunavut",
        !!as.name(varname) ==  "CA" ~ "Canada",
        TRUE~ !!as.name(varname)))

  }
  return(dataset)
}


############################################################################################################################################ #
############################################################################################################################################ #

# Function to take a character vec, to collapse into single comma-separated string, but add the word "and" before last of the list
# used in Summary slide / footnotes
turn_char_vec_to_comma_list<-function(vector){
  if(class(vector)!="character"){
    print("This is not a character vector")
  }else if (length(vector)==1){
    final_string<-vector
  }else if (length(vector)==2){
    final_string<-str_c(vector, collapse = " and ")
  }else if (length(vector)>2){
      final_string<-paste0(str_c(vector[1:length(vector)-1], collapse = ", "), ", and ",vector[length(vector)])
  }
  return(final_string)
}

############################################################################################################################################ #
############################################################################################################################################ #


#TODO: Function to change PRNAME var into a factor by PT order West to East, or alphabetically



