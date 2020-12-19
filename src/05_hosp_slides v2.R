jurisdiction <- if (Sys.getenv("hosp_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

# Filter province
pt_hosp_icu_filter <- pt_hosp_icu %>%
    filter(prname %in% jurisdiction) %>%
    filter(date >= "2020-07-01") %>%
    # filter(date != Sys.Date()-1) %>%
    group_by(prname) %>%
    mutate(label = if_else(date == max(date), as.character(round(cases, digits = 1)), NA_character_))

# Plot
ggplot(pt_hosp_icu_filter %>% filter(), aes(date, cases, colour = type)) +
    geom_line(size = 2) +
    geom_text_repel(aes(label = label),
        size = 6,
        nudge_y = 1,
        nudge_x = 1,
        na.rm = TRUE
    ) +
    facet_wrap(vars(prname), scales = "free_y") +
    scale_x_date(
        NULL,
        #breaks = scales::breaks_width("2 weeks"),
        labels = label_date("%d%b")
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma_format(accuracy = 1)
    ) +
    scale_color_manual(labels = c("Total hospitalizations", "Total ICU"), values = c("blue", "red")) +
    labs(caption = paste0(
        "Refreshed on: ",
        pt_hosp_icu_filter %>% filter(date == max(date)) %>% select(date) %>% distinct() %>% pull() %>% as.Date()
    )) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20)
    )

#Create table for hospitalization metrics
pt_hosp_icu_filter2 <- pt_hosp_icu %>%
  filter(prname %in% c(jurisdiction,"Canada")) %>%
  filter(date >= "2020-07-01") %>%
  group_by(prname) %>%
  mutate(label = if_else(date == max(date), as.character(round(cases, digits = 1)), NA_character_))

hosp_metrics1 <- pt_hosp_icu_filter2 %>%
    filter(type=="hospitalized") %>%
    group_by(prname) %>%
    mutate(hosp7ma=round(rollmean(cases, k=7, fill=NA, align="right"))) %>%
    mutate(delta7=(cases-lag(cases,7))/lag(cases,7)) %>%
    mutate(delta7=percent(delta7,accuracy = 0.1)) %>%
    select(prname, date, cases, hosp7ma, delta7) %>%
    rename("Jurisdiction"=prname, "Date"=date, "Hospitalizations"=cases, 
           #"7 Day MA of Hospitalizations"=hosp7ma, 
           #"Weekly Change in Hospitalizations"=delta7)
           "delta7h"=delta7)

hosp_metrics2 <- pt_hosp_icu_filter2 %>%
    filter(type=="icu") %>%
    group_by(prname) %>%
    mutate(icu7ma=round(rollmean(cases, k=7, fill=NA, align="right"))) %>%
    mutate(delta7=(cases-lag(cases,7))/lag(cases,7)) %>%
    mutate(delta7=percent(delta7,accuracy=0.1)) %>%
    select(prname, date, cases, icu7ma, delta7) %>%
    rename("Jurisdiction"=prname, "Date"=date, "ICU"=cases, 
           #"7 Day MA of ICU"=icu7ma,
           #"Weekly Change in ICU"=delta7)
           "delta7i"=delta7)

Hosp_Metrics <- hosp_metrics1 %>%
    left_join(hosp_metrics2, by=c("Jurisdiction","Date")) %>%
    filter(Date==max(Date))

metricorder <- c("Canada","British Columbia","Alberta","Ontario","Quebec","Manitoba","Saskatchewan")

Hosp_Metrics <- Hosp_Metrics %>% slice(match(metricorder,Jurisdiction))

remove(hosp_metrics1,hosp_metrics2)
