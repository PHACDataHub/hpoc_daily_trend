jurisdiction <- if (Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

qry_cases_raw<-PHACTrendR::import_case_report_form_data()

qry_canada <- qry_cases_raw %>%
  janitor::clean_names() %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20) %>%
  filter(!is.na(age)) %>%
  group_by(earliestdate, agegroup20) %>%
  tally() %>%
  mutate(Jurisdiction = "Canada") %>%
  filter(!is.na(earliestdate))

qry_cases <- qry_cases_raw %>%
  janitor::clean_names() %>%
  select(phacid, pt, earliestdate, age, agegroup10, agegroup20) %>%
  mutate(Jurisdiction = toupper(pt)) %>%
  recode_PT_names_to_big() %>%
  group_by(earliestdate, agegroup20, Jurisdiction) %>%
  dplyr::tally() %>%
  dplyr::filter(!is.na(earliestdate)) %>%
  dplyr::bind_rows(qry_canada) %>%
  filter(Jurisdiction %in% c("Canada", recode_PT_names_to_big(PHACTrendR::PTs_big6))) %>%
  factor_PT_west_to_east(Canada_first=TRUE, size="big") %>%
  dplyr::rename(cases = n)

# Not used in the trend report for the time being. Code preserved as may be needed in weekly report code.
# qry_lab_onset <- qry_cases_raw %>%
#   janitor::clean_names() %>%
#   filter(pt != "Repatriate") %>%
#   filter(onsetdate >= "2020-03-01") %>%
#   filter(onsetdate <= (max(onsetdate - days(15)))) %>%
#   select(onsetdate, earliestlabcollectiondate) %>%
#   filter(!is.na(onsetdate)) %>%
#   mutate(delay = earliestlabcollectiondate - onsetdate) %>%
#   filter(between(delay, 0, 15)) %>% # filtering any outliers as identified in the SAS file
#   group_by(onsetdate) %>%
#   dplyr::summarise(mean_delay = mean(delay, na.rm = TRUE),
#                    daily_case = n())

# Filter province
qry_crude_filter <- qry_cases %>%
  filter(Jurisdiction %in% jurisdiction) %>%
  mutate(earliestdate = as.Date(earliestdate)) %>%
  filter(!is.na(earliestdate)) %>%
  arrange(Jurisdiction, agegroup20, earliestdate) %>%
  group_by(Jurisdiction, agegroup20) %>%
  mutate(sdma = rollmean(cases, 7, na.pad = TRUE, align = "right")) %>%
  mutate(agegroup20 = as.character(agegroup20)) %>%
  filter(agegroup20 != "Unknown") %>%
  filter(agegroup20 != "NaN") %>%
  filter(agegroup20 != "unknown") %>%
  filter(agegroup20 != "") %>%
  ungroup()

qry_crude_filter$Jurisdiction <- recode(qry_crude_filter$Jurisdiction, "Canada"="")

# Plot Crude Cases (Canada)
ggplot(qry_crude_filter %>% filter(earliestdate >= "2020-06-01"), aes(x = earliestdate, y = sdma, colour = agegroup20)) +
  geom_line(size = 1.5) +
  facet_wrap(vars(Jurisdiction), scales = "free_y") +
  scale_y_continuous("Number of reported cases, 7 Day moving average", labels = comma_format(accuracy = 1)) +
  scale_x_date(
    "Date of illness onset",
    breaks = scales::breaks_width("6 weeks"),
    labels = label_date("%d%b")
  ) +
  geom_rect(aes(
    xmin = qry_crude_filter %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
    xmax = qry_crude_filter %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
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


