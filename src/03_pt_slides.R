# Create the final dataframe to be fed into ggplot
df_pt_incidence_filter <- df %>%
    mutate(date = as.Date(date)) %>%
    left_join(PHACTrendR::latest_can_pop,by ="Jurisdiction") %>%
    group_by(Jurisdiction) %>%
    mutate(case_pop_thousand = numtoday / Population * 100000) %>%
    mutate(case_pop_thousand_sdma = rollmean(numtoday, 7, na.pad = TRUE, align = "right") / Population * 100000) %>%
    recode_PT_names_to_small() %>%
    filter(Jurisdiction %in% (PHACTrendR::PTs_big6)) %>%
    factor_PT_west_to_east()%>%
    mutate(label = if_else(date == max(date), as.character(round(case_pop_thousand_sdma, digits = 1)), NA_character_))

# Plot
ggplot(df_pt_incidence_filter, aes(date, case_pop_thousand_sdma)) +
    geom_line(colour = "darkblue", size = 2) +
    facet_wrap(vars(Jurisdiction), scales = "free") +
    scale_y_continuous(
        "Number of cases per 100,000 population",
        labels = comma_format(accuracy = 1)
    ) +
    scale_x_date(
        "Date of case report",
        #breaks = scales::breaks_width("6 weeks"),
        labels = label_date("%d%b")
    ) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 26, face = "bold"),
        text = element_text(size = 20),
        plot.caption = element_text(hjust = 0)
    ) +
  
    labs(caption = paste0("Source: Provincial and territorial website data \nUpdated Daily (Sun-Thurs). Data as of: ",format(max(df_pt_incidence_filter$date),"%B %d")))
