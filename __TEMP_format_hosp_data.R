#Temp file to manipulate and save hosp data in tidier format

library(readr)
library(readxl)
library(lubridate)

## Remember 

hosp_raw<-read_excel(path = file.choose(), sheet = "Hospitalization (current)")%>%
  head(15)

icu_raw<-read_excel(path = file.choose(), sheet = "ICU (current)") %>%
  head(15)

hosp<-hosp_raw %>%
  pivot_longer(cols=(where(is.numeric)),names_to = "date",values_to="hospitalizations") %>%
  mutate(date=as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  rename(Jurisdiction=`P/T`,
         Date=date,
         hosp=hospitalizations)


icu<-icu_raw %>%
  pivot_longer(cols=(where(is.numeric)),names_to = "date",values_to="icu") %>%
  mutate(date=as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  rename(Jurisdiction=`P/T`,
         Date=date)

hosp_and_icu<-hosp %>%
  left_join(icu, by=c("Jurisdiction"="Jurisdiction","Date"="Date")) %>%
  filter(!Jurisdiction=="Ttl")
