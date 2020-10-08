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
        breaks = scales::breaks_width("2 weeks"),
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
