# For the cases and deaths data; this gets updated around 7:30 PM EST everyday =======
df <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  filter(date <= params$date)

# For the international comparison data; this gets updated once daily =======
df_int <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
  rename(date = dateRep) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date <= params$date)

# Get provincial population data from StatsCan
pt_pop_raw <- get_cansim("17-10-0005-01")

# Get the hospitalization and ICU data =======
# First scraped data for Alberta
ab_severity <- xml2::read_html("https://www.alberta.ca/stats/covid-19-alberta-statistics.htm") %>%
    html_nodes(xpath = "//*[@id='summary']/div[3]/script/text()") %>%
    html_text()

counts <- ab_severity %>%
    str_extract_all("((?:\\d+,)+\\d+)")

date <- ab_severity %>%
    str_extract_all("\\d{4}-\\d{2}-\\d{2}") %>%
    unlist() %>%
    as.Date() %>%
    unique()

hospitalized <- counts[[1]][15] %>% # changed element as that had changed from before
    strsplit(split = ",") %>%
    unlist() %>%
    as.numeric()

icu <- counts[[1]][7] %>% # changed element as that had changed from before
    strsplit(split = ",") %>%
    unlist() %>%
    as.numeric()

ab_all <- tibble(date, hospitalized, icu) %>%
  mutate(prname = "AB")

ab_hosp <- ab_all %>%
  select(prname, date, hospitalized)

ab_icu <- ab_all %>%
  select(prname, date, icu)

# Then import data for rest of provinces from the file that has human scraped data
# Hospitalization data, wrangling the input from the Python script
pt_hosp <- pivot_longer(pt_hosp_raw, !"P/T", names_to = "date", values_to = "hospitalized") %>%
  mutate(date = as.Date((date))) %>%
  rename("prname" = "P/T") %>%
  mutate(prname = recode(prname, "Ttl" = "Canada")) %>%
  filter(prname != "AB")

pt_hosp_filter <- bind_rows(pt_hosp, ab_hosp)

# ICU data, wrangling the input from the Python script
pt_icu <- pivot_longer(pt_icu_raw, !"P/T", names_to = "date", values_to = "icu") %>%
  mutate(date = as.Date((date))) %>%
  rename("prname" = "P/T") %>%
  mutate(prname = recode(prname, "Ttl" = "Canada")) %>%
  filter(prname != "AB")

pt_icu_filter <- bind_rows(pt_icu, ab_icu)

# combine hosp and ICU data
pt_hosp_icu <- pt_hosp_filter %>%
    left_join(pt_icu_filter, by = c("prname", "date")) %>%
    filter(prname %in% c("Canada", "BC", "AB", "SK", "MB", "ON", "QC")) %>%
    mutate(prname = factor(prname, c("Canada", "BC", "AB", "SK", "MB", "ON", "QC"))) %>%
    pivot_longer("hospitalized":"icu", names_to = "type", values_to = "cases") %>%
    mutate(prname = recode(prname,
        "BC" = "British Columbia",
        "AB" = "Alberta",
        "SK" = "Saskatchewan",
        "MB" = "Manitoba",
        "ON" = "Ontario",
        "QC" = "Quebec"
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
    rename(cases = n)

# Get SALT lab data from the network drive ======
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")
  