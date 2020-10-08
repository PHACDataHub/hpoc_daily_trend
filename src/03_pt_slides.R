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
  mutate(case_pop_thousand = numtoday/pop*100000) %>%
  mutate(case_pop_thousand_sdma = rollmean(numtoday, 7, na.pad=TRUE, align = "right")/pop*100000) %>%
  filter(prname %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario","Quebec")) %>%
  mutate(prname = factor(prname, c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario","Quebec"))) %>%
  mutate(label = if_else(date == max(date), as.character(round(case_pop_thousand_sdma, digits = 1)), NA_character_))

#Plot
ggplot(df_pt_incidence_filter, aes(date, case_pop_thousand_sdma)) +
  geom_line(colour = "blue", size = 2) +
  geom_text_repel(aes(label = label),
                  colour = "black",
                  segment.color = "white",
                  size = 6,
                  nudge_y = 1,
                  nudge_x = 1,
                  na.rm = TRUE) +
  facet_wrap(vars(prname), scales = "fixed") +
  scale_y_continuous(
    "Number of cases per 100,000 population",
    labels = comma_format(accuracy = 1)
  ) +
  scale_x_date(
    "Date of case report",
    breaks = scales::breaks_width("4 weeks"),
    labels = label_date("%d%b")
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))

##### Ad-hoc stuff, comment out #####

# df_pt_incidence_filter <- df %>%
#   mutate(date = as.Date(date)) %>%
#   filter(date >= "2020-03-01") %>%
#   left_join(pt_pop_filter, by = "prname") %>%
#   group_by(prname) %>%
#   mutate(case_pop_thousand = numtoday/pop*100000) %>%
#   mutate(case_pop_thousand_sdma = rollmean(numtoday, 7, na.pad=TRUE, align = "right")/pop*100000) %>%
#   filter(prname %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario","Quebec")) %>%
#   mutate(prname = factor(prname, c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario","Quebec"))) %>%
#   mutate(label = if_else(date == max(date), as.character(round(case_pop_thousand_sdma, digits = 1)), NA_character_))
# 
# #Plot
# ggplot(df_pt_incidence_filter, aes(date, case_pop_thousand_sdma)) +
#   geom_line(colour = "darkblue", size = 2) +
#   facet_wrap(vars(prname), scales = "free_x") +
#   scale_y_continuous(
#     "Number of cases per 100,000 population",
#     labels = comma_format(accuracy = 1),
#     expand = c(0, NA)
#   ) +
#   scale_x_date(
#     "Date of case report",
#     breaks = scales::breaks_width("1 month"),
#     labels = label_date("%d%b")#,
#     #expand = c(0, NA)
#   ) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, size = 22),
#         panel.spacing = unit(2, "lines"),
#         text = element_text(size=20))
  
  
  