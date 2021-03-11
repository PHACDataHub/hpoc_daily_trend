#

DISCOVER_hosp <- qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, hosp) %>%
  filter(!is.na(age)&hosp=="yes") %>%
  group_by(earliestdate, agegroup20,pt) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt)))

DISCOVER_hosp_national<-DISCOVER_hosp%>%
  group_by(earliestdate, agegroup20) %>%
tally() %>%
  mutate(Jurisdiction="Canada")

DISCOVER_deaths<-qry_cases_raw  %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20, coviddeath) %>%
  filter(!is.na(age)&coviddeath=="yes") %>%
  group_by(earliestdate, agegroup20,pt) %>%
  tally() %>%
  filter(!is.na(earliestdate)) %>%
  mutate(Jurisdiction=PHACTrendR::recode_PT_names_to_big(toupper(pt)))

DISCOVER_deaths_national<-DISCOVER_deaths%>%
  ungroup() %>%
  group_by(earliestdate, agegroup20) %>%
tally() %>%
  mutate(Jurisdiction="Canada")
