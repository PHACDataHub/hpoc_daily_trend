


####################################################################################### #
######## PT cases and deaths data; this gets updated around 7:30 PM EST everyday ########
####################################################################################### #

df_raw <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)


#Removing trailing unreported days from PTs, in the past 7 days.
df<-df_raw %>%
  group_by(prname) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  filter(!(!prname %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE))

df_corrected<-df %>%
  select(-numtotal,-numdeaths)

########### Hard-coded manual corrections
# May 3rd, extra 1317 cases from April were reported in QC, real value should be 892 (https://www.cbc.ca/news/canada/montreal/covid-19-quebec-may-3-1.5553881)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Quebec",correction_date = "2020-05-03",corrected_value = 892)
# May 31st, extra 165 cases were reported in QC, real value should be 37 (https://montreal.ctvnews.ca/quebec-records-37-new-covid-19-deaths-but-adds-165-that-weren-t-recorded-1.4962370)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Quebec",correction_date = "2020-05-31",corrected_value = 37)

# #data dump of deaths in Ontario on October 2-4
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-02",corrected_value = 2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-03",corrected_value = 4)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-04",corrected_value = 4)
#Oct10-12 weekend
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-10",corrected_value = 170)
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-11",corrected_value = 159)
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-12",corrected_value = 119)
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-13",corrected_value = 101)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-10",corrected_value = 236)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-11",corrected_value = 260)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-12",corrected_value = 246)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-13",corrected_value = 220)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Ontario",correction_date = "2020-10-12",corrected_value = 807)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Ontario",correction_date = "2020-10-13",corrected_value = 736)
# cases over Xmas and NY 2020
df_corrected<-correct_df(metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-25",corrected_value = 2246)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-26",corrected_value = 2246)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-25",corrected_value = 173.66)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-26",corrected_value = 173.67)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-27",corrected_value = 173.67)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-01",corrected_value = 163)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-02",corrected_value = 163)
#deaths over Xmas and NY 2020
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-25",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-26",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-27",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-28",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",         correction_date = "2020-12-25",corrected_value = 40.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",         correction_date = "2020-12-26",corrected_value = 40.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-25",corrected_value = 9.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-26",corrected_value = 9.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-27",corrected_value = 9.34)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-01",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-02",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-03",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-04",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",         correction_date = "2020-12-31",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-01",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-02",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-03",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-04",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2021-01-01",corrected_value = 5.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2021-01-02",corrected_value = 5.5)
#Weekend Jan23-24 stuff
#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-23",corrected_value = 8.66)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British columbia",correction_date = "2021-01-24",corrected_value = 8.67)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-25",corrected_value = 8.67)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2021-01-25",corrected_value = 360) 
#Weekend Jan30-31stuff (21 deaths reported on Feb.1 after not reporting over the weekend)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-30",corrected_value = 7)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-31",corrected_value = 7)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-01",corrected_value = 7)
#Weekend Feb6-7stuff (13 deaths reported on Feb.8 after not reporting over the weekend)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-06",corrected_value = 4.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-07",corrected_value = 4.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-08",corrected_value = 4.34)

#getting corrected values for the national number now
can_corrected_case_death<-df_corrected %>%
  filter(!prname=="Canada") %>%
  select(prname, date, numtoday, numdeathstoday) %>%
  group_by(date) %>%
  summarise(can_numtoday=sum(numtoday, na.rm = TRUE),
            can_deathstoday=sum(numdeathstoday, na.rm = TRUE)) %>%
  ungroup()

df_corrected2<-df_corrected %>%
  left_join(can_corrected_case_death, by = "date") %>%
  mutate(numtoday=ifelse(prname=="Canada", can_numtoday, numtoday),
         numdeathstoday=ifelse(prname=="Canada",can_deathstoday,numdeathstoday),
         numtoday=as.numeric(numtoday),
         numdeathstoday=as.numeric(numdeathstoday)) %>%
  select(-can_numtoday, -can_deathstoday)%>%
  group_by(prname)%>%
  mutate(numtotal=cumsum(numtoday),
         numdeaths=cumsum(numdeathstoday))

#For now, we can just take our corrections forward, but may want to differentiate between raw data and corrected data one day
df<-df_corrected2
rm(df_corrected, df_corrected2)

####################################################################################### #
########      International comparison data; this gets updated once daily        ########
####################################################################################### #


df_int <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date <= params$date)

####################################################################################### #
########               Provincial population data from StatsCan                  ########
####################################################################################### #


pt_pop_raw <- get_cansim("17-10-0005-01")

#Filter to latest year of data
latest_can_pop <- pt_pop_raw %>%
  mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  filter(`Age group`=="All ages",REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  select(GEO,VALUE) %>%
  dplyr::rename(Population=VALUE)

#Deriving population 20 year age groups instead of importing excel file with this data pre-calculated
pt_pop20 <- pt_pop_raw %>%
  mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  filter(str_detect(`Age group`, "to|90 years and over"),REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  mutate(age_group_20=case_when(`Age group`=="0 to 4 years" ~ "0 to 19",
                                `Age group`=="5 to 9 years" ~ "0 to 19",
                                `Age group`=="10 to 14 years" ~ "0 to 19",
                                `Age group`=="15 to 19 years" ~ "0 to 19",
                                `Age group`=="20 to 24 years" ~ "20 to 39",
                                `Age group`=="25 to 29 years" ~ "20 to 39",
                                `Age group`=="30 to 34 years" ~ "20 to 39",
                                `Age group`=="35 to 39 years" ~ "20 to 39",
                                `Age group`=="40 to 44 years" ~ "40 to 59",
                                `Age group`=="45 to 49 years" ~ "40 to 59",
                                `Age group`=="50 to 54 years" ~ "40 to 59",
                                `Age group`=="55 to 59 years" ~ "40 to 59",
                                `Age group`=="60 to 64 years" ~ "60 to 79",
                                `Age group`=="65 to 69 years" ~ "60 to 79",
                                `Age group`=="70 to 74 years" ~ "60 to 79",
                                `Age group`=="75 to 79 years" ~ "60 to 79",
                                `Age group`=="80 to 84 years" ~ "80 or plus",
                                `Age group`=="85 to 89 years" ~ "80 or plus",
                                `Age group`=="90 years and over" ~ "80 or plus",
                                TRUE ~ "remove")) %>%
  filter(!age_group_20=="remove") %>%
  select(GEO,VALUE, age_group_20) %>%
  group_by(GEO, age_group_20) %>%
  summarise(Population=sum(VALUE)) %>%
  rename(Jurisdiction=GEO,
         AgeGroup20=age_group_20,
         Population20=Population)


####################################################################################### #
########                  Get the hospitalization and ICU data                   ########
####################################################################################### #
# 

#Yet another way to import data, through google sheets! (Work in progress)
# g_sheets_user<-"hsfluepi@phac-aspc.gc.ca"
# g_sheets_pass<-"epiavian"


#3.6 seconds time! just need to reformat now
hosp_data<-read_sheet("https://docs.google.com/spreadsheets/d/1aodkeukVF1r3F-w2jJnTclVIW_swlLeFHv_Xnsmsgtc/edit#gid=1708078290", sheet="hosp_and_icu")

# First scraped data for Alberta
ab_severity <- xml2::read_html("https://www.alberta.ca/stats/covid-19-alberta-statistics.htm") %>%
    html_nodes(xpath = "//*[@id='summary']/div/script/text()") %>%
    .[1] %>%
    html_text()

#extracts dates
dates <- ab_severity %>% 
  str_extract_all("\\d{4}-\\d{2}-\\d{2}") %>% 
  unlist() %>% 
  as.Date() %>% 
  unique()

counts <- ab_severity %>%
    str_extract_all("((?:\\d+,)+\\d+)") %>%
    unlist()

non_icu <- counts[15] %>% # changed element as that had changed from before
    strsplit(split = ",") %>%
    unlist() %>%
    as.numeric()

icu <- counts[7] %>% # changed element as that had changed from before
    strsplit(split = ",") %>%
    unlist() %>%
    as.numeric()

ab_all <- tibble(date = dates, hospitalized = non_icu + icu, icu) %>%
  mutate(prname = "AB")

ab_hosp <- ab_all %>%
  select(prname, date, hospitalized)

ab_icu <- ab_all %>%
  select(prname, date, icu)

# Then import data for rest of provinces from the file that has human scraped data
# Hospitalization data, wrangling the input from the Python script
pt_hosp <- pivot_longer(pt_hosp_raw, !"P/T", names_to = "date", values_to = "hospitalized") %>%
  mutate(date = as.Date((date))) %>%
  dplyr::rename("prname" = "P/T") %>%
  mutate(prname = recode(prname, "Ttl" = "Canada")) %>%
  filter(prname != "AB")

pt_hosp_filter <- bind_rows(pt_hosp, ab_hosp)

# ICU data, wrangling the input from the Python script
pt_icu <- pivot_longer(pt_icu_raw, !"P/T", names_to = "date", values_to = "icu") %>%
  mutate(date = as.Date((date))) %>%
  dplyr::rename("prname" = "P/T") %>%
  mutate(prname = recode(prname, "Ttl" = "Canada")) %>%
  filter(prname != "AB")

pt_icu_filter <- bind_rows(pt_icu, ab_icu)

# combine hosp and ICU data
pt_hosp_icu <- pt_hosp_filter %>%
    left_join(pt_icu_filter, by = c("prname", "date")) %>%
    filter(prname %in% c("Canada", "BC", "AB", "SK", "MB", "ON", "QC","NL","NB","NS","PE","YK","NT","NU")) %>%
    pivot_longer("hospitalized":"icu", names_to = "type", values_to = "cases") %>%
    recode_PT_names_to_big(geo_variable="prname")%>%
    mutate(prname = factor(prname, c("Canada", "British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut"))) %>%
  filter(date <= params$date)


####################################################################################### #
########                        Get case Report Form data                        ########
####################################################################################### #

# 
# metabase_user='**********'
# metabase_pass='**********'
# 
# # Time benchmarking:
# # user  system elapsed 
# # 10.91    0.25   80.22 +2.74 for metabase_login() 
# handle<- metabase_login(base_url = "https://discover-metabase.hres.ca/api",
#                          database_id = 2, # phac database
#                          username = metabase_user,
#                          password = metabase_pass)
# qry_cases_raw <- metabase_query(handle, "select phacid, phacreporteddate, episodedate, pt, age_years, agegroup10, agegroup20, onsetdate, earliestlabcollectiondate, sex, gender, sexgender, coviddeath, hosp, icu, exposure_cat from all_cases;") %>%
#   rename(age=age_years)




#note - we make a call to this dataset in 04.R, and 04a.R codes, in case this is set to be deleted
# Time benchmarking:
#   user  system elapsed 
#   13.01    0.09   48.43
qry_cases_raw <- readRDS("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/CaseReportForm/trend_extract.rds") %>%
  mutate(onsetdate = as.Date(onsetdate),
         episodedate=as.Date(episodedate),
         earliestlabcollectiondate = as.Date(earliestlabcollectiondate)) %>%
  rename(age=age_years)

qry_canada <- qry_cases_raw %>%
  clean_names() %>%
  select(phacid, pt, episodedate, age, agegroup10, agegroup20) %>%
  filter(!is.na(age)) %>%
  group_by(episodedate, agegroup20) %>%
  tally() %>%
  mutate(prname = "Canada") %>%
  filter(!is.na(episodedate))

qry_cases <- qry_cases_raw %>%
    clean_names() %>%
    select(phacid, pt, episodedate, age, agegroup10, agegroup20) %>%
    mutate(prname = toupper(pt)) %>%
    recode_PT_names_to_big(geo_variable="prname") %>%
    group_by(episodedate, agegroup20, prname) %>%
    tally() %>%
    filter(!is.na(episodedate)) %>%
    bind_rows(qry_canada) %>%
    filter(prname %in% c("Canada", "British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")) %>%
    mutate(prname = factor(prname, c("Canada", "British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec"))) %>%
    dplyr::rename(cases = n)

#generate onset lab collection dataframes

qry_lab_onset <- qry_cases_raw %>%
  clean_names() %>%
  filter(pt != "Repatriate") %>%
  filter(onsetdate >= "2020-03-01") %>%
  filter(onsetdate <= (max(onsetdate - days(15)))) %>%
  select(onsetdate, earliestlabcollectiondate) %>%
  filter(!is.na(onsetdate)) %>%
  mutate(delay = earliestlabcollectiondate - onsetdate) %>%
  filter(between(delay, 0, 15)) %>% # filtering any outliers as identified in the SAS file
  group_by(onsetdate) %>%
  dplyr::summarise(mean_delay = mean(delay, na.rm = TRUE),
            daily_case = n())

#export onset lab collection dataframe as csv
write.csv(qry_lab_onset, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\onset.csv')

#create dataframe for onset metrics

lab_onset_metrics <- qry_lab_onset %>%
  mutate(Date = format(as.Date(onsetdate), "%b %Y")) %>%
  group_by(Date) %>%
  mutate(Avg_Onset = round(mean(mean_delay),digits = 2)) %>%
  distinct(Date, Avg_Onset, .keep_all = TRUE) %>%
  select(Date, Avg_Onset)


####################################################################################### #
########                              Get SALT lab data                          ########
####################################################################################### #
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")

