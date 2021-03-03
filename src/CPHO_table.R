# Code to generate CPHO table!
#designed to be run as part of Trend_Report.rmd
# 
#packages used:
# library(dplyr)
# library(tibble)
# library(kableExtra)
# library(scales)

#in case want to switch to params$date later on
CPHO_table_date<-Sys.Date()

#different date formats used in this table
CPHO_table_date_filename<-format(CPHO_table_date, "%Y-%m-%d")
CPHO_table_date_title<-format(CPHO_table_date, "%B %d, %Y")
CPHO_table_value_date<-format(CPHO_table_date,"%b %d")
#created as part of 04 labtesting weekly.R
lab_dates<-this_week_label



#deriving additional key variables not created as part of the trend report
Canada_latest_stats<-df_raw %>%
  filter(date==max(date)&Jurisdiction=="Canada") 

new_cases<-Canada_latest_stats%>%
  select(numtoday) %>%
  as.numeric()

new_deaths<-Canada_latest_stats%>%
  select(numdeathstoday) %>%
  as.numeric()

active_cases<-Canada_latest_stats %>%
  select(numactive) %>%
  as.numeric()

cum_cases<-Canada_latest_stats %>%
  select(numtotal)%>%
  as.numeric()

cum_deaths<-Canada_latest_stats %>%
  select(numdeaths) %>%
  as.numeric()

key_national_7MA_cases
key_national_7MA_deaths
key_national_7MA_hosp
key_national_7MA_icu

key_labtesting_table_footnote

key_Can_weekly_tests
key_Can_avg_tests_per_day
key_Can_weekly_perc_positive



#Numbers for CPHO statement - February XX 2021
#National Indicator  |  Value   |  Cut off Date


`National Indicator`<-c("Number of new cases",
                    "Number of new deaths",
                    "Number of active cases",
                    "Cumulative number of cases",
                    "Cumulative number of deaths",
                    "7-day MA cases",
                    "7-day MA deaths",
                    "7 day MA hosp",
                    "7-day MA ICU",
                    "Test positivity (last week)",
                    "Total number of tests performed (last week)",
                    "Daily average number of tests performed (last week)")

Value<-c(comma(new_cases),
         comma(new_deaths),
         comma(active_cases),
         comma(cum_cases),
         comma(cum_deaths),
         key_national_7MA_cases,
         key_national_7MA_deaths,
         comma(key_national_7MA_hosp),
         comma(key_national_7MA_icu),
         key_Can_weekly_perc_positive,
         key_Can_weekly_tests,
         key_Can_avg_tests_per_day)

`Cut-off Date`<-c(rep(CPHO_table_value_date,9),rep(lab_dates,3))

CPHO_table<-tibble(`National Indicator`,Value,`Cut-off Date`)

CPHO_table_formatted<-flextable(CPHO_table)
CPHO_table_formatted <- CPHO_table_formatted %>% footnote(., i=10:12, j=1, value = as_paragraph(
  c("Lab testing numbers from last week may vary slightly as PTs 
continually update lab testing data.")
  ),
  ref_symbols = c("*"),
  part = "body")

CPHO_table_title=paste0("Numbers for CPHO statement - ",CPHO_table_date_title)



CPHO_table_formatted<-add_header(CPHO_table_formatted,
                        `National Indicator`=CPHO_table_title,
                        Value=CPHO_table_title,
                        `Cut-off Date`=CPHO_table_title) %>%
  merge_h(part="header") %>%
  bold(bold=TRUE, part="header") %>%
  align(j=2:3, align = "right", part="all") %>%
  fontsize(size = 14, part="all") %>%
  fontsize(size = 16, part="header") %>%
  fontsize(i=1,size = 18, part="header")
  
  CPHO_table_formatted<-autofit(CPHO_table_formatted) 
  CPHO_table_formatted<-fix_border_issues(CPHO_table_formatted, part="header")

save_as_pptx(path=paste0(".\\output\\CPHO_table_",CPHO_table_date_filename,".pptx"),CPHO_table_formatted)

#for some reason, save_as_image() isn't working...
# save_as_image(CPHO_table_formatted, "output/CPHO_table.png",webshot="webshot")
