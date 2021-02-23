# Filter province
pt_hosp_icu_filter <- all_hosp_data %>%
    filter(Date >= "2020-04-01" & !Jurisdiction=="Repatriated Travellers") %>%
    group_by(Jurisdiction) %>%
    filter(Date != max(Date)) %>% # to prevent dip from AB
    mutate(label = if_else(Date == max(Date), as.character(round(cases, digits = 1)), NA_character_),
           Jurisdiction=as.character(Jurisdiction)) %>%
    recode_PT_names_to_small() %>%
  factor_PT_west_to_east()

cat('\n')  
cat("# COVID-19 patients in hospital daily across Canada", "\n") 

# Plot National
ggplot(pt_hosp_icu_filter %>% filter(Jurisdiction=="CAN"), aes(Date, cases, colour = type)) +
    geom_line(size = 2) +
    facet_wrap(vars(Jurisdiction), scales = "free_y") +
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
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 26),
        legend.key.size = unit(3,"line"),
        text = element_text(size = 20),
        plot.caption = element_text(hjust = 0)) +
    labs(caption = paste0("Source: Provincial and territorial website data. \nNote: Hospitalization values are up to ", format(max(pt_hosp_icu_filter$Date), "%B %d")," as AB does not report same-day hospitalizations.",
                          "\nUpdated daily (Sun-Thurs). Data as of: ",format(max(all_hosp_data$Date), "%B %d")))

cat('\n') 

cat('\n')  
cat("# COVID-19 patients in hospital daily in selected provinces and territories", "\n") 


# Plot by PT
ggplot(pt_hosp_icu_filter %>% filter(Jurisdiction %in% c("BC","AB","SK","MB","QC","ON")), aes(Date, cases, colour = type)) +
  geom_line(size = 2) +
  facet_wrap(vars(Jurisdiction), scales = "free") +
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
                        \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(all_hosp_data$Date), "%B %d")))

cat('\n') 
