# Filter province
pt_hosp_icu_filter <- pt_hosp_icu %>%
#    filter(prname %in% jurisdiction) %>%
    filter(date >= "2020-04-01") %>%
    filter(date != max(date)) %>% # to prevent dip from AB
    group_by(prname) %>%
    mutate(label = if_else(date == max(date), as.character(round(cases, digits = 1)), NA_character_))
    
pt_hosp_icu_filter$prname <- recode(pt_hosp_icu_filter$prname, "Canada"="", "British Columbia"="BC","Alberta"="AB","Saskatchewan"="SK","Manitoba"="MB","Quebec"="QC","Ontario"="ON")


cat('\n')  
cat("# COVID-19 patients in hospital daily across Canada", "\n") 

# Plot National
ggplot(pt_hosp_icu_filter %>% filter(prname==""), aes(date, cases, colour = type)) +
    geom_line(size = 2) +
    facet_wrap(vars(prname), scales = "free_y") +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("1 month"),
        labels = label_date("%b-%y")
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma_format(accuracy = 1)
    ) +
    scale_color_manual(labels = c("Total hospitalizations", "Total ICU"), values = c("darkblue", "red")) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 26, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 26),
        legend.key.size = unit(3,"line"),
        text = element_text(size = 20),
        plot.caption = element_text(hjust = 0)) +
    labs(caption = paste0("Source: Provincial and territorial website data. \nNote: Hospitalization values are up to ", format(max(pt_hosp_icu_filter$date), "%B %d")," as AB does not report same-day hospitalizations.",
                          "\nUpdated daily (Sun-Thurs). Data as of: ",format(max(pt_hosp_icu$date), "%B %d")))

cat('\n') 

cat('\n')  
cat("# COVID-19 patients in hospital daily in selected provinces and territories", "\n") 


# Plot by PT
ggplot(pt_hosp_icu_filter %>% filter(prname %in% c("BC","AB","SK","MB","QC","ON")), aes(date, cases, colour = type)) +
  geom_line(size = 2) +
  facet_wrap(vars(prname), scales = "free") +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("3 months"),
    labels = label_date("%b-%y")
  ) +
  scale_y_continuous(
    "Number of cases",
    labels = comma_format(accuracy = 1)
  ) +
  scale_color_manual(labels = c("Total hospitalizations", "Total ICU"), values = c("darkblue", "red")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 26, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  ) +
  labs(caption = paste0("Source: Provincial and territorial website data. 
                        \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(pt_hosp_icu$date), "%B %d")))

cat('\n') 