


####################################################################################### #
######## PT cases and deaths data; this gets updated around 7:30 PM EST everyday ########
####################################################################################### #

df <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)

df <- read_csv("covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)


####  hard-coded manual corrections
#deaths over oct2-3 long weekend
df[df$prname=="Ontario"&df$date=="2020-10-02","numdeathstoday"]<-2
df[df$prname=="Ontario"&df$date=="2020-10-03","numdeathstoday"]<-4
df[df$prname=="Ontario"&df$date=="2020-10-04","numdeathstoday"]<-4
df[df$prname=="Canada"&df$date=="2020-10-02","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-10-02","numdeathstoday"]-74
df[df$prname=="Canada"&df$date=="2020-10-03","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-10-02","numdeathstoday"]-37
df[df$prname=="Canada"&df$date=="2020-10-04","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-10-02","numdeathstoday"]-3
#Oct10-12 weekend
df[df$prname=="British Columbia"&df$date=="2020-10-10","numtoday"]<-170 #0 -> 170, +170
df[df$prname=="British Columbia"&df$date=="2020-10-11","numtoday"]<-159 #0 -> 159, +159
df[df$prname=="British Columbia"&df$date=="2020-10-12","numtoday"]<-119 #0 -> 119, +119 
df[df$prname=="British Columbia"&df$date=="2020-10-13","numtoday"]<-101 #549 -> 101, -448
df[df$prname=="Alberta"&df$date=="2020-10-10","numtoday"]<-236 #0 -> 236, +236
df[df$prname=="Alberta"&df$date=="2020-10-11","numtoday"]<-260 # 0-> 260, +260
df[df$prname=="Alberta"&df$date=="2020-10-12","numtoday"]<-246 # 0-> 246, +246
df[df$prname=="Alberta"&df$date=="2020-10-13","numtoday"]<-220 # 961 -> 220, -741
df[df$prname=="Ontario"&df$date=="2020-10-12","numtoday"]<-807 # 0-> 807, +807
df[df$prname=="Ontario"&df$date=="2020-10-13","numtoday"]<-746 # 1553 -> 746, -746
df[df$prname=="Canada"&df$date=="2020-10-10","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-10-10","numtoday"] +170+236
df[df$prname=="Canada"&df$date=="2020-10-11","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-10-11","numtoday"] +159+260 
df[df$prname=="Canada"&df$date=="2020-10-12","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-10-12","numtoday"] + 119+246+807
df[df$prname=="Canada"&df$date=="2020-10-13","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-10-13","numtoday"] - 448-741-746
# cases over Xmas and NY 2020
df[df$prname=="Quebec"&df$date=="2020-12-25","numtoday"]<-2246
df[df$prname=="Quebec"&df$date=="2020-12-26","numtoday"]<-2246
df[df$prname=="Manitoba"&df$date=="2020-12-25","numtoday"]<-173.66
df[df$prname=="Manitoba"&df$date=="2020-12-26","numtoday"]<-173.67
df[df$prname=="Manitoba"&df$date=="2020-12-27","numtoday"]<-173.67
df[df$prname=="Canada"&df$date=="2020-12-25","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-25","numtoday"]+2246+173.66
df[df$prname=="Canada"&df$date=="2020-12-26","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-26","numtoday"]-2246+173.66
df[df$prname=="Canada"&df$date=="2020-12-27","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-27","numtoday"]-(173.66*2)
df[df$prname=="Manitoba"&df$date=="2021-01-01","numtoday"]<-163
df[df$prname=="Manitoba"&df$date=="2021-01-02","numtoday"]<-163
df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]+163
df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]-163
#deaths over Xmas and NY 2020
df[df$prname=="Nova Scotia"&df$date=="2020-12-25","numdeathstoday"]<-3.25
df[df$prname=="Nova Scotia"&df$date=="2020-12-26","numdeathstoday"]<-3.25
df[df$prname=="Nova Scotia"&df$date=="2020-12-27","numdeathstoday"]<-3.25
df[df$prname=="Nova Scotia"&df$date=="2020-12-28","numdeathstoday"]<-3.25
df[df$prname=="Ontario"&df$date=="2020-12-25","numdeathstoday"]<-40.5
df[df$prname=="Ontario"&df$date=="2020-12-26","numdeathstoday"]<-40.5
df[df$prname=="Manitoba"&df$date=="2020-12-25","numdeathstoday"]<-9.33
df[df$prname=="Manitoba"&df$date=="2020-12-26","numdeathstoday"]<-9.33
df[df$prname=="Manitoba"&df$date=="2020-12-27","numdeathstoday"]<-9.33
df[df$prname=="Canada"&df$date=="2020-12-25","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-12-25","numdeathstoday"]+3.25 +40.5 + 9.33
df[df$prname=="Canada"&df$date=="2020-12-26","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-12-26","numdeathstoday"]+3.25 -40.5 + 9.33
df[df$prname=="Canada"&df$date=="2020-12-27","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-12-27","numdeathstoday"]+3.25 - (9.33*2)
df[df$prname=="Canada"&df$date=="2020-12-28","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-12-28","numdeathstoday"]-(3.25*3)
df[df$prname=="British Columbia"&df$date=="2021-01-01","numdeathstoday"]<-11.25
df[df$prname=="British Columbia"&df$date=="2021-01-02","numdeathstoday"]<-11.25
df[df$prname=="British Columbia"&df$date=="2021-01-03","numdeathstoday"]<-11.25
df[df$prname=="British Columbia"&df$date=="2021-01-04","numdeathstoday"]<-11.25
df[df$prname=="Alberta"&df$date=="2020-12-31","numdeathstoday"]<-19.2
df[df$prname=="Alberta"&df$date=="2021-01-01","numdeathstoday"]<-19.2
df[df$prname=="Alberta"&df$date=="2021-01-02","numdeathstoday"]<-19.2
df[df$prname=="Alberta"&df$date=="2021-01-03","numdeathstoday"]<-19.2
df[df$prname=="Alberta"&df$date=="2021-01-04","numdeathstoday"]<-19.2
df[df$prname=="Manitoba"&df$date=="2021-01-01","numdeathstoday"]<-5.5
df[df$prname=="Manitoba"&df$date=="2021-01-02","numdeathstoday"]<-5.5
df[df$prname=="Canada"&df$date=="2021-12-31","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2020-12-31","numdeathstoday"]+19.2
df[df$prname=="Canada"&df$date=="2021-01-01","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-01","numdeathstoday"]+11.25+19.2 +5.5
df[df$prname=="Canada"&df$date=="2021-01-02","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-02","numdeathstoday"]+11.25+19.2 -5.5
df[df$prname=="Canada"&df$date=="2021-01-03","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-03","numdeathstoday"]+11.25+19.2
df[df$prname=="Canada"&df$date=="2021-01-04","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-04","numdeathstoday"]-(11.25*3)-(19.2*4)
#Weekend Jan23-24 stuff
#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df[df$prname=="Alberta"&df$date=="2021-01-25","numtoday"]<-362
df[df$prname=="Canada"&df$date=="2021-01-25","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-25","numtoday"] - 380
df[df$prname=="British Columbia"&df$date=="2021-01-23","numdeathstoday"]<-8.66
df[df$prname=="British Columbia"&df$date=="2021-01-24","numdeathstoday"]<-8.67
df[df$prname=="British Columbia"&df$date=="2021-01-25","numdeathstoday"]<-8.67
df[df$prname=="Canada"&df$date=="2021-01-23","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-23","numdeathstoday"]+ 8.66
df[df$prname=="Canada"&df$date=="2021-01-24","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-24","numdeathstoday"]+ 8.67
df[df$prname=="Canada"&df$date=="2021-01-25","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-25","numdeathstoday"]- 8.66 - 8.67
#Weekend Jan30-31stuff
df[df$prname=="British Columbia"&df$date=="2021-01-30","numdeathstoday"]<-7
df[df$prname=="British Columbia"&df$date=="2021-01-31","numdeathstoday"]<-7
df[df$prname=="British Columbia"&df$date=="2021-02-01","numdeathstoday"]<-7
df[df$prname=="Canada"&df$date=="2021-01-30","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-30","numdeathstoday"]+ 7
df[df$prname=="Canada"&df$date=="2021-01-31","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-01-31","numdeathstoday"]+ 7
df[df$prname=="Canada"&df$date=="2021-02-01","numdeathstoday"]<-df[df$prname=="Canada"&df$date=="2021-02-01","numdeathstoday"]- 14


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
    mutate(prname = factor(prname, c("Canada", "BC", "AB", "SK", "MB", "ON", "QC","NL","NB","NS","PE","YK","NT","NU"))) %>%
    pivot_longer("hospitalized":"icu", names_to = "type", values_to = "cases") %>%
    mutate(prname = recode(prname,
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
        "NT" = "Northwest Territories",
        "NU" = "Nunavut"
    )) %>%
  filter(date <= params$date)


####################################################################################### #
########                        Get case Report Form data                        ########
####################################################################################### #

#note - we make a call to this dataset in 04.R, and 04a.R codes, in case this is set to be deleted
qry_cases_raw <- readRDS("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/CaseReportForm/trend_extract.rds") %>%
  mutate(onsetdate = as.Date(onsetdate),
         episodedate=as.Date(episodedate),
         earliestlabcollectiondate = as.Date(earliestlabcollectiondate))


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
    mutate(prname = pt) %>%
    mutate(prname = recode(prname,
        "bc" = "British Columbia",
        "ab" = "Alberta",
        "sk" = "Saskatchewan",
        "mb" = "Manitoba",
        "on" = "Ontario",
        "qc" = "Quebec"
    )) %>%
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
  