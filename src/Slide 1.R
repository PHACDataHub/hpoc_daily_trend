library(dplyr)
library(tidyverse)
library(patchwork)
library(scales)
library(zoo)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(ggthemes)
library(cansim)
library(janitor)
library(xml2)
library(rvest)
library(stringr)
library(tableHTML)
library(formattable)

# For the cases and deaths data; this gets updated around 7:30 PM EST everyday =======
df <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) #%>%
#  filter(date <= params$date)

# For the international comparison data; this gets updated once daily =======
df_int <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
  rename(date = dateRep) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) #%>%
#  filter(date <= params$date)

# Get provincial population data from StatsCan
pt_pop_raw <- get_cansim("17-10-0005-01")

df_filter <- df %>%
  filter(date >= "2020-03-08")

table_nat_stat <- df_filter %>%
  rename(Jurisdiction = prname) %>%
  rename(Cases_Cumulative = numtotal) %>%
  rename(Deaths_Cumulative = numdeaths) %>%
  rename(Date = date) %>%
  group_by(Jurisdiction) %>%
  filter(Jurisdiction!="Repatriated Travellers") %>%
  mutate(Cases_Daily = Cases_Cumulative-lag(Cases_Cumulative)) %>%
  mutate(Deaths_Daily = Deaths_Cumulative-lag(Deaths_Cumulative)) %>%
  mutate(Cases_Daily_7MA = rollmean(Cases_Daily, k=7, fill=NA, align=c("right"))) %>%
  mutate(Deaths_Daily_7MA = rollmean(Deaths_Daily, k=7, fill=NA, align=c("right"))) %>%
  mutate(Weekly_Change_Cases = (Cases_Daily_7MA-lag(Cases_Daily_7MA,n=7L,default=0))/lag(Cases_Daily_7MA,n=7L,default=0)) %>%
  mutate(Weekly_Change_Deaths = (Deaths_Daily_7MA-lag(Deaths_Daily_7MA,n=7L,default=0))/lag(Deaths_Daily_7MA,n=7L,default=0)) %>%
  select(Jurisdiction,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,Deaths_Daily,Deaths_Daily_7MA,
         Weekly_Change_Deaths) %>%
  arrange(Jurisdiction,Date)
  
Canada7 <- table_nat_stat %>%
  filter(Jurisdiction=="Canada") %>%
  mutate(CanadaCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(CanadaDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,CanadaCase7,CanadaDeath7)

PT7 <- table_nat_stat %>%
  group_by(Jurisdiction) %>%
  mutate(PTCase7 = rollsum(Cases_Daily,k=7,fill=NA,align=c("right"))) %>%
  mutate(PTDeath7 = rollsum(Deaths_Daily,k=7,fill=NA,align=c("right"))) %>%
left_join(Canada7,by="Date",keep=FALSE) %>%
  mutate(National_Case_Proportion=PTCase7/CanadaCase7) %>%
  mutate(National_Death_Proportion=PTDeath7/CanadaDeath7) %>%
  select(Jurisdiction.x,Date,Cases_Daily,Cases_Daily_7MA,Weekly_Change_Cases,National_Case_Proportion,Deaths_Daily,Deaths_Daily_7MA,Weekly_Change_Deaths,National_Death_Proportion) %>%
  rename(Jurisdiction=Jurisdiction.x)

Case_Death_Stats <- PT7 %>% 
  group_by(Jurisdiction) %>% 
  filter(Date==max(Date)) %>%
  mutate(Weekly_Change_Cases=percent(Weekly_Change_Cases,accuracy=0.1)) %>%
  mutate(National_Case_Proportion=percent(National_Case_Proportion,accuracy=0.1)) %>%
  mutate(Weekly_Change_Deaths=percent(Weekly_Change_Deaths,accuracy=0.1)) %>%
  mutate(National_Death_Proportion=percent(National_Death_Proportion,accuracy=0.1)) %>%
  mutate(Cases_Daily_7MA=round(Cases_Daily_7MA)) %>%
  mutate(Deaths_Daily_7MA=round(Deaths_Daily_7MA)) 

#Table1 <- formattable(Case_Death_Stats, list(
#  Weekly_Change_Cases = formatter("span", 
#                         style = ~style(font.weight = "bold", color = 
#                                          ifelse(Weekly_Change_Cases > 0,"red",
#                                                 ifelse(Weekly_Change_Cases == 0,"black",
#                                                     ifelse(Weekly_Change_Cases <0, "green",NA))))),
#  Weekly_Change_Deaths = formatter("span", 
#                                   style = ~style(font.weight = "bold", color = 
#                                                    ifelse(Weekly_Change_Deaths > 0,"red",
#                                                           ifelse(Weekly_Change_Deaths == 0,"black",
#                                                                  ifelse(Weekly_Change_Deaths <0, "green",NA)))))
#))

Canada_pop <- pt_pop_raw %>%
  filter(`Age group`=="All ages",GEO=="Canada",REF_DATE=="2020",Sex=="Both sexes") %>%
  select(GEO,VALUE) %>%
  rename(Population=VALUE)

Case_per_100K <- PT7 %>%
  filter(Jurisdiction=="Canada") %>%
  left_join(Canada_pop,by=c("Jurisdiction"="GEO"),keep=FALSE) %>%
  mutate(Case_per_100K = (Cases_Daily/Population)*100000) %>%
  mutate(Case_per_100K_7MA = rollmean(Case_per_100K,k=7,fill=NA,align=c("right"))) %>%
  select(Jurisdiction,Date,Cases_Daily,Case_per_100K,Case_per_100K_7MA)

write.csv(Case_per_100K,"C:\\Users\\FISLAM\\Documents\\case_per_100k.csv")



# # Code for conditional colouring of the text in the code
# cols_case <- matrix("black", nrow(Case_Death_Stats), ncol(Case_Death_Stats))
# cols_case[,5] <- if_else(Case_Death_Stats[,5] > 0, "red",
#                            if_else(Case_Death_Stats[,5] < 0, "green", "black")
# )
# cols_case[,9] <- if_else(Case_Death_Stats[,9] > 0, "red",
#                          if_else(Case_Death_Stats[,9] < 0, "green", "black")
# )
# 
# Test <- gridExtra::tableGrob(Case_Death_Stats[1:10, c("Jurisdiction","Date","Cases_Daily","Cases_Daily_7MA","Weekly_Change_Cases","National_Case_Proportion","Deaths_Daily","Deaths_Daily_7MA","Weekly_Change_Deaths","National_Death_Proportion")],
#                      theme = ttheme_minimal(core = list(fg_params = list(
#                        col = cols_case,
#                        hjust = 0,
#                        x = 0.0,
#                        fontsize = 18
#                      ))),
#                      rows = NULL,
#                      cols = NULL)