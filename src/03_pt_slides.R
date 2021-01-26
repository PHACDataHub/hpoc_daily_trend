# Filter population data to most recent
pt_pop_filter <- pt_pop_raw %>%
    clean_names() %>%
    mutate(ref_date = as.numeric(ref_date)) %>%
    filter(sex == "Both sexes") %>%
    filter(age_group == "All ages") %>%
    filter(ref_date == max(ref_date)) %>%
    rename(prname = geo) %>%
    select(prname, value) %>%
    rename(pop = value)

# Create the final dataframe to be fed into ggplot
df_pt_incidence_filter <- df %>%
    mutate(date = as.Date(date)) %>%
    left_join(pt_pop_filter, by = "prname") %>%
    group_by(prname) %>%
    mutate(case_pop_thousand = numtoday / pop * 100000) %>%
    mutate(case_pop_thousand_sdma = rollmean(numtoday, 7, na.pad = TRUE, align = "right") / pop * 100000) %>%
    filter(prname %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")) %>%
    mutate(prname = factor(prname, c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec"))) %>%
    mutate(label = if_else(date == max(date), as.character(round(case_pop_thousand_sdma, digits = 1)), NA_character_))

df_pt_incidence_filter$prname <- recode(df_pt_incidence_filter$prname, "British Columbia"="BC","Alberta"="AB","Saskatchewan"="SK","Manitoba"="MB","Quebec"="QC","Ontario"="ON")

# Plot
ggplot(df_pt_incidence_filter, aes(date, case_pop_thousand_sdma)) +
    geom_line(colour = "blue", size = 2) +
    facet_wrap(vars(prname), scales = "free") +
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
  
    labs(caption = "Source: Provincial and territorial website data")
