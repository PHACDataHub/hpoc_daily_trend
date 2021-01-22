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
