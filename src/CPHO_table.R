# Code to generate CPHO table!
#designed to be run as part of Trend_Report.rmd
# 
#packages used:
# library(dplyr)
# library(tibble)
# library(kableExtra)



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

value_date<-format(Sys.Date(),"%b %d")
lab_dates<-this_week_label

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

`Cut off Date`<-c(rep(value_date,9),rep(lab_dates,3))

CPHO_table<-tibble(`National Indicator`,Value,`Cut off Date`)

CPHO_table_formatted<-flextable(CPHO_table)
save_as_image(CPHO_table_formatted, "output/CPHO_table.png",webshot="webshot")

".\\output\\cases_deaths_15days.csv"