# For the cases and deaths data; this gets updated around 7:30 PM EST everyday =======
df <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)

df_corrected<-df %>%
  select(-numtotal,-numdeaths)

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

#hard-coded manual corrections
# Idea - instead of hardcoding corrections in the PTs, and for Canada, we just hardcode for the PTs and recalculate Canada at the end, similar to how hosp/icu is being calculated.

# #hard-coding of a long-standing error (line 56 in 01a.SAS; first time COVID19 df is created in program)
# df$numtotal[df$prname=="Saskatchewan" & df$date=="2020-05-20"] = 620
# #data dump of deaths in Ontario on October 2-4
# df[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-02","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-02"]-74
# df[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-03","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-03"]-111
# df[df$prname %in% c("Canada","Ontario") & df$date>="2020-10-04","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date>="2020-10-04"]-114
# #Thanksgiving long weekend 202 reporting
# df$numtotal[df$prname=="British Columbia" & df$date=="2020-10-10"] = 10355
# df$numtotal[df$prname=="British Columbia" & df$date=="2020-10-11"] = 10514
# df$numtotal[df$prname=="British Columbia" & df$date=="2020-10-12"] = 10633
# df$numtotal[df$prname=="Alberta" & df$date=="2020-10-10"] = 20231
# df$numtotal[df$prname=="Alberta" & df$date=="2020-10-11"] = 20490
# df$numtotal[df$prname=="Alberta" & df$date=="2020-10-12"] = 20736
# df$numtotal[df$prname=="Ontario" & df$date=="2020-10-12"] = 59946
# df$numtotal[df$prname=="Canada" & df$date=="2020-10-10"] = 180585
# df$numtotal[df$prname=="Canada" & df$date=="2020-10-11"] = 182688
# df$numtotal[df$prname=="Canada" & df$date=="2020-10-12"] = 184835

# #data dump of deaths in Ontario on October 2-4
# df[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-02","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-02"]-74
# df[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-03","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date=="2020-10-03"]-111
# df[df$prname %in% c("Canada","Ontario") & df$date>="2020-10-04","numdeaths"]=df$numdeaths[df$prname %in% c("Canada","Ontario") & df$date>="2020-10-04"]-114
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-02",corrected_value = 2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-03",corrected_value = 4)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-04",corrected_value = 4)



# cases over Xmas 2020
df_corrected<-correct_df(metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-25",corrected_value = 2246)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-26",corrected_value = 2246)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-25",corrected_value = 173.66)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-26",corrected_value = 173.67)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-27",corrected_value = 173.67)

# cases over NY 2020
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-01",corrected_value = 163)
df_corrected<-correct_df(metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-02",corrected_value = 163)


#deaths over Xmas 2020
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",correction_date = "2020-12-25",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",correction_date = "2020-12-26",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",correction_date = "2020-12-27",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Nova Scotia",correction_date = "2020-12-28",corrected_value = 3.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-12-25",corrected_value = 40.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-12-26",corrected_value = 40.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",correction_date = "2020-12-25",corrected_value = 9.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",correction_date = "2020-12-26",corrected_value = 9.33)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",correction_date = "2020-12-27",corrected_value = 9.34)

#deaths over NY 2020
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-01",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-02",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-03",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-04",corrected_value = 11.25)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",correction_date = "2020-12-31",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",correction_date = "2021-01-01",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",correction_date = "2021-01-02",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",correction_date = "2021-01-03",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Alberta",correction_date = "2021-01-04",corrected_value = 19.2)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-01-01",corrected_value = 5.5)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-01-02",corrected_value = 5.5)

#Weekend Jan23-24 stuff
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-23",corrected_value = 8.66)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British columbia",correction_date = "2021-01-24",corrected_value = 8.67)
df_corrected<-correct_df(metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-25",corrected_value = 8.67)
#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2021-01-25",corrected_value = 360) 

#Weekend Jan30-31stuff

df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2021-01-30",corrected_value = 7)
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2021-01-31",corrected_value = 7)
df_corrected<-correct_df(metric="cases",Jurisdiction = "British Columbia",correction_date = "2021-02-01",corrected_value = 7)

#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df[df$prname=="Alberta"&df$date=="2021-01-25","numtoday"]<-362
df[df$prname=="Canada"&df$date=="2021-01-25","numtoday"]<-df[df$prname=="Canada"&df$date=="2021-01-25","numtoday"] - 380
df_corrected<-correct_df(metric="cases",Jurisdiction = "Alberta",correction_date = "2021-01-25",corrected_value = 360) 



#getting corrected values for the national number now
can_corrected_case_death<-df_corrected %>%
  filter(!prname=="Canada") %>%
  select(prname, date, numtoday, numdeathstoday) %>%
  group_by(date) %>%
  summarise(can_numtoday=sum(numtoday),
            can_deathstoday=sum(numdeathstoday)) %>%
  ungroup()

df_corrected2<-df_corrected %>%
  left_join(can_corrected_case_death, by = "date") %>%
  mutate(numtoday=ifelse(prname=="Canada", can_numtoday, numtoday),
         numdeathstoday=ifelse(prname=="Canada",can_deathstoday,numdeathstoday)) %>%
  select(-can_numtoday, -can_deathstoday)%>%
  group_by(prname)%>%
  mutate(numtotal=cumsum(numtoday),
         numdeaths=cumsum(numdeathstoday))

#For now, we can just take our corrections forward, but may want to differentiate between raw data and corrected data one day
df<-df_corrected2



# For the international comparison data; this gets updated once daily =======
df_int <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date <= params$date)

# Get provincial population data from StatsCan
pt_pop_raw <- get_cansim("17-10-0005-01")

#Filter to latest year of data
latest_can_pop <- pt_pop_raw %>%
  mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  filter(`Age group`=="All ages",REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  select(GEO,VALUE) %>%
  dplyr::rename(Population=VALUE)

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
  mutate(episodedate = as.Date(episodedate)) %>% # getting an error with the DISCOVER file without this line
  mutate(earliestlabcollectiondate = as.Date(earliestlabcollectiondate))

qry_canada <- qry_cases_raw %>%
  clean_names() %>%
  select(phacid, pt, episodedate, age, agegroup10, agegroup20) %>%
  mutate(episodedate = as.Date((episodedate))) %>%
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
  filter(episodedate >= "2020-03-01") %>%
  filter(episodedate <= (max(episodedate - days(15)))) %>%
  select(episodedate, earliestlabcollectiondate) %>%
  filter(!is.na(episodedate)) %>%
  mutate(delay = earliestlabcollectiondate - episodedate) %>%
  filter(between(delay, 0, 15)) %>% # filtering any outliers as identified in the SAS file
  group_by(episodedate) %>%
  dplyr::summarise(mean_delay = mean(delay, na.rm = TRUE),
            daily_case = n())

#export onset lab collection dataframe as csv

write.csv(qry_lab_onset, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\onset.csv')

#create dataframe for onset metrics

lab_onset_metrics <- qry_lab_onset %>%
  mutate(Date = format(as.Date(episodedate), "%b %Y")) %>%
  group_by(Date) %>%
  mutate(Avg_Onset = round(mean(mean_delay),digits = 2)) %>%
  distinct(Date, Avg_Onset, .keep_all = TRUE) %>%
  select(Date, Avg_Onset)

# Get SALT lab data from the network drive ======
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")
  