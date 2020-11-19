#Load libraries
library("readxl")
library("dplyr")
library("tidyr")
library("date")
library("stringr")
library("lubridate")
library("hms")
library("zoo")
library("stringr")
library("ggplot2")
library("scales")
library("sqldf")

#Import lab testing data
#SALT <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")

#Import old lab testing data; Remove Apr 26-May 2; divide percent column by 100 to get decimals
#OldLab <- read_excel("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/Lab data pre-SALT/Old_lab_data.xlsx")
#OldLab <- OldLab %>%
#    filter(Week!="Apr  26-May  2") %>%
#    mutate(Percent_positive=Percent_positive/100) %>%
#    mutate(Percent_positive_fr=Percent_positive/100)

SALT <- salt_raw

SALT2 <- SALT %>%
    mutate(Date=as.Date(str_sub(Report.Date,1,10))) %>%
    mutate(Time=as_hms(str_sub(Report.Date,13,20))) %>%
    mutate(datetime=strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S")) %>%
    mutate(Start_of_week=floor_date(Date, "week")) %>%
    mutate(End_of_week=date(Start_of_week)+6) %>%
    mutate(Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week))) %>%
    mutate(Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
    filter(Date <= floor_date(max(Date), "week")-1) %>%
    mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
    arrange(Jurisdiction,datetime)

SALT3 <- SALT2 %>%
    group_by(Jurisdiction,Week) %>%
    filter(datetime==max(datetime))

SALT4 <- SALT3 %>%
    group_by(Jurisdiction) %>%
    mutate(Week_patients_tested = Patients.Tested-lag(Patients.Tested)) %>%
    mutate(Week_confirmed_positive = Confirmed.Positive-lag(Confirmed.Positive)) %>%
    mutate(Week_confirmed_negative = Confirmed.Negative-lag(Confirmed.Negative)) %>%
    mutate(Avg_tests_per_day = Week_patients_tested/7) %>%
    mutate(Percent_positive = Week_confirmed_positive/Week_patients_tested)

National <- SALT4 %>%
    select(Week, Start_of_week, Week_patients_tested, Week_confirmed_positive, Week_confirmed_negative) %>%
    group_by(Week, Start_of_week) %>%
    summarise(across(where(is.numeric),sum)) %>%
    mutate(Jurisdiction="Canada") %>%
    mutate(Percent_positive = Week_confirmed_positive/Week_patients_tested) %>%
    mutate(Avg_tests_per_day = Week_patients_tested/7) %>%
    arrange(Start_of_week)
  
National <- subset(National, select=-Start_of_week)

National <- National[,c(1,5,2,3,4,6,7)]

Provincial <- SALT4 %>%
    select(Week, Jurisdiction, Week_patients_tested, Week_confirmed_positive, Week_confirmed_negative, Percent_positive, Avg_tests_per_day)

Testing <- rbind(National,Provincial)

Testing <- Testing[complete.cases(Testing), ]

Testing <- Testing %>%
    mutate(Week=gsub(" -","",Week)) %>%
    mutate(Week=gsub("  to  Jan ","-",Week)) %>%
    mutate(Week=gsub("  to  Feb ","-",Week)) %>%
    mutate(Week=gsub("  to  Mar ","-",Week)) %>%
    mutate(Week=gsub("  to  Apr ","-",Week)) %>%
    mutate(Week=gsub("  to  May ","-",Week)) %>%
    mutate(Week=gsub("  to  Jun ","-",Week)) %>%
    mutate(Week=gsub("  to  Jul ","-",Week)) %>%
    mutate(Week=gsub("  to  Aug ","-",Week)) %>%
    mutate(Week=gsub("  to  Sep ","-",Week)) %>%
    mutate(Week=gsub("  to  Oct ","-",Week)) %>%
    mutate(Week=gsub("  to  Nov ","-",Week)) %>%
    mutate(Week=gsub("  to  Dec ","-",Week)) %>%
    group_by(Jurisdiction) %>%
    mutate(Week_no = 1:n()) %>%
    slice(tail(row_number(),12))
    

Testing <- Testing[,c(8,1,2,3,4,5,6,7)]

Testing2 <- Testing %>% select(Week_no, Week, Jurisdiction, Week_patients_tested, Week_confirmed_positive)

Testing2 <- gather(Testing2, Metric, Value, Week_patients_tested:Week_confirmed_positive, factor_key=TRUE)

coeff <-0.0000001

PT_List <- unique(Testing$Jurisdiction)

for(i in seq_along(PT_List))
{
plot <- ggplot() +
    geom_bar(data=subset(Testing2[order(Testing2$Metric,decreasing = T),], Jurisdiction==PT_List[i]), 
             aes(x=reorder(Week, Week_no), y=Value, fill=Metric, group=1), position="stack", stat="identity") +
    geom_line(data=subset(Testing, Jurisdiction==PT_List[i]), 
              aes(x=reorder(Week, Week_no), 
                  y=Percent_positive/coeff, group=1, linetype = "Percent Positive"), 
              stat = "identity", size = 1, colour = "red") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Tests",
    labels = comma_format(accuracy = 1),
#    limits = c(0, Testing2 %>% filter(Metric=="Week_patients_tested") %>% 
#                  select(Value) %>%
#                  max()),
#    expand = c(0,0),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Percent Positivity", labels = scales::percent)
    
  ) + 
  scale_x_discrete("Week") +
  
  scale_fill_discrete(name="", labels=c("Patients Tested", "Confirmed Positive")) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  ggtitle(paste(PT_List[i])) +
  theme(plot.title = element_text(hjust = 0.5))
print(plot)
}

