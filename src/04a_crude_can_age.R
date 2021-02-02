jurisdiction <- if (Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

# Filter province
qry_crude_filter <- qry_cases %>%
  filter(prname %in% jurisdiction) %>%
  mutate(episodedate = as.Date(episodedate)) %>%
  filter(!is.na(episodedate)) %>%
  arrange(prname, agegroup20, episodedate) %>%
  group_by(prname, agegroup20) %>%
  mutate(sdma = rollmean(cases, 7, na.pad = TRUE, align = "right")) %>%
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()

qry_crude_filter$prname <- recode(qry_crude_filter$prname, "Canada"="")

# Plot Crude Cases (Canada)
ggplot(qry_crude_filter %>% filter(episodedate >= "2020-06-01"), aes(x = episodedate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(prname), scales = "free_y") +
  scale_y_continuous("Number of reported cases, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = qry_crude_filter %>% filter(episodedate == max(episodedate) - days(14)) %>% select(episodedate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = qry_crude_filter %>% filter(episodedate == max(episodedate)) %>% select(episodedate) %>% distinct() %>% pull() %>% as.Date(),
    ymin = -Inf,
    ymax = Inf
  ),
  alpha = 0.01, fill = "grey", inherit.aes = FALSE
  ) +
  scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
  #scale_colour_wsj() +
  labs(caption = paste0(
    "* Shaded area represents approximate lag in reporting
    \nUpdated Daily (Sun-Thurs). Data as of: ", format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
  )

