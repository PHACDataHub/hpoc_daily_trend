# For the cases and deaths data; this gets updated around 7:30 PM EST everyday =======
df <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)

#hard-coded manual corrections

# cases over Xmas 2020
df[df$prname=="Quebec"&df$date=="2020-12-25","numtoday"]<-2246
df[df$prname=="Quebec"&df$date=="2020-12-26","numtoday"]<-2246
df[df$prname=="Manitoba"&df$date=="2020-12-25","numtoday"]<-173.66
df[df$prname=="Manitoba"&df$date=="2020-12-26","numtoday"]<-173.66
df[df$prname=="Manitoba"&df$date=="2020-12-27","numtoday"]<-173.66

df[df$prname=="Canada"&df$date=="2020-12-25","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-25","numtoday"]+2246+173.66 #4092+2246+173.66 = 6511.66
df[df$prname=="Canada"&df$date=="2020-12-26","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-26","numtoday"]-2246+173.66 #8129-2246+173.66 = 6056.66 
df[df$prname=="Canada"&df$date=="2020-12-27","numtoday"]<-df[df$prname=="Canada"&df$date=="2020-12-27","numtoday"]-(173.66*2)  #5903-173.66-173.66 = 5555.66 

# cases over NY 2020
df[df$prname=="Manitoba"&df$date=="2021-01-01","numtoday"]<-163
df[df$prname=="Manitoba"&df$date=="2020-01-02","numtoday"]<-163

df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]+163
df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-01","numtoday"]-163

#deaths over Xmas 2020
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

#deaths over NY 2020
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



# For the international comparison data; this gets updated once daily =======
df_int <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
  dplyr::rename(date = dateRep) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date <= params$date)

# Get provincial population data from StatsCan
pt_pop_raw <- get_cansim("17-10-0005-01")

# Extract population data by age group 20
pt_pop20 <- read_excel("Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\_Source Data\\Population data\\FPT_AgePop20.xlsx")

# Get the hospitalization and ICU data =======
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

# Get case level data for age breakdown from the network drive ======

# Import the latest xlsx file as a dataframe; using the Python script to identify the latest RDS file
qry_cases_raw <- readRDS(latest_file) %>%
  mutate(onsetdate = as.Date(onsetdate)) %>% # getting an error with the DISCOVER file without this line
  mutate(earliestlabcollectiondate = as.Date(earliestlabcollectiondate))

qry_canada <- qry_cases_raw %>%
  clean_names() %>%
  select(phacid, pt, onsetdate, age, agegroup10, agegroup20) %>%
  mutate(onsetdate = as.Date((onsetdate))) %>%
  filter(!is.na(age)) %>%
  group_by(onsetdate, agegroup20) %>%
  tally() %>%
  mutate(prname = "Canada") %>%
  filter(!is.na(onsetdate))

qry_cases <- qry_cases_raw %>%
    clean_names() %>%
    select(phacid, pt, onsetdate, age, agegroup10, agegroup20) %>%
    mutate(prname = pt) %>%
    mutate(prname = recode(prname,
        "bc" = "British Columbia",
        "ab" = "Alberta",
        "sk" = "Saskatchewan",
        "mb" = "Manitoba",
        "on" = "Ontario",
        "qc" = "Quebec"
    )) %>%
    group_by(onsetdate, agegroup20, prname) %>%
    tally() %>%
    filter(!is.na(onsetdate)) %>%
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

# Get SALT lab data from the network drive ======
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")
  