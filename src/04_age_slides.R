jurisdiction <- if(Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario","Quebec")

# Filter province
qry_cases_filter <- qry_cases %>%
  filter(prname %in% jurisdiction) %>%
  mutate(onset_date = as.Date(onset_date)) %>%
  filter(!is.na(onset_date)) %>%
  arrange(prname, agegroup20, onset_date) %>%
  group_by(prname, agegroup20) %>%
  mutate(sdma = rollmean(cases, 7, na.pad=TRUE, align = "right")) %>%
  filter(onset_date >= "2020-06-01") %>%
  filter(agegroup20 != "Unknown") %>%
  ungroup()


# Plot
ggplot(qry_cases_filter, aes(onset_date, sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(prname), scales = "free_y") +
  scale_y_continuous("Number of reported cases, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("3 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(xmin = qry_cases_filter %>% filter(onset_date == max(onset_date) - days(14)) %>% select(onset_date) %>% distinct() %>% pull() %>% as.Date(), 
                xmax = qry_cases_filter %>% filter(onset_date == max(onset_date)) %>% select(onset_date) %>% distinct() %>% pull() %>% as.Date(), 
                ymin = -Inf, 
                ymax = Inf), 
            alpha = 0.01, fill="grey", inherit.aes = FALSE) +
  #scale_color_brewer(type = "qual", palette = 1) +
  scale_colour_wsj() +
  labs(caption = paste0("Refreshed on: ", 
                        qry_cases_filter %>% filter(onset_date == max(onset_date)) %>% select(onset_date) %>% distinct() %>% pull() %>% as.Date(),
                        "\n* Shaded area represents approximate lag in reporting")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=26),
        text = element_text(size=20))
  
