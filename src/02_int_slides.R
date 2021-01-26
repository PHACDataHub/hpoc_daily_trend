#manually calculated 7 day MA and 7 day MA per 1,000,000 and check for discrepancy.
#no discrepancy found; using provided variables

int_cases <- df_int %>%
  filter(iso_code %in% c("AUS","CAN","DNK","FRA","DEU","IRL","ISR","ZAF","GBR","USA")) %>%
  select(date, location, population, new_cases, new_cases_smoothed, new_cases_smoothed_per_million) %>%
  filter(new_cases_smoothed_per_million != is.na(new_cases_smoothed_per_million)) %>%
  mutate(label = if_else(date == max(date), as.character(location), NA_character_))

int_deaths <- df_int %>%
  filter(iso_code %in% c("AUS","CAN","DNK","FRA","DEU","IRL","ISR","ZAF","GBR","USA")) %>%
  select(date, location, population, new_deaths, new_deaths_smoothed, new_deaths_smoothed_per_million) %>%
  filter(new_deaths_smoothed_per_million != is.na(new_deaths_smoothed_per_million)) %>%
  mutate(label = if_else(date == max(date), as.character(location), NA_character_))

cat('\n')  
cat("# Daily Cases by Country (7-day moving average, population adjusted)", "\n") 

# Plot cases
ggplot(int_cases, aes(date, new_cases_smoothed_per_million, group = location, colour = location)) +
         geom_line(size = 2) +
        # geom_text_repel(aes(label = label),
        #         size = 6,
        #         nudge_x = 1,
        #         nudge_y = 1,
        #         na.rm = TRUE
        # ) +
        scale_y_continuous("Daily cases per 1,000,000 population", expand = c(0, 0), limits = c(0, NA)) +
        scale_x_date("Date", 
                     breaks = scales::breaks_width("1 month"),
                     labels = label_date("%b-%Y")
                     ) +
        scale_colour_tableau(palette = "Tableau 10") +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.key=element_blank(),
                legend.text = element_text(size = 26),
                legend.key.size = unit(3,"line"),
                text = element_text(size = 20)
        )

cat('\n') 

cat('\n')  
cat("# Daily Deaths by Country (7-day moving average, population adjusted)", "\n") 

#Plot deaths
ggplot(int_deaths, aes(date, new_deaths_smoothed_per_million, group = location, colour = location)) +
  geom_line(size = 2) +
  # geom_text_repel(aes(label = label),
  #         size = 6,
  #         nudge_x = 1,
  #         nudge_y = 1,
  #         na.rm = TRUE
  # ) +
  scale_y_continuous("Daily deaths per 1,000,000 population", expand = c(0, 0), limits = c(0, NA)) +
  scale_x_date("Date", 
               breaks = scales::breaks_width("1 month"),
               labels = label_date("%b-%Y")
  ) +
  scale_colour_tableau(palette = "Tableau 10") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.text = element_text(size = 26),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20)
  )

cat('\n') 