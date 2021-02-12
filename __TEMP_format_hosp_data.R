#Temp file to manipulate and save hosp data in tidier format

library(readr)
library(readxl)

## Remember 

hosp<-read_excel(path = file.choose(), sheet = "Hospitalization (current)")%>%
  head(16)

icu<-read_excel(path = file.choose(), sheet = "ICU (current)") %>%
  head(16)
