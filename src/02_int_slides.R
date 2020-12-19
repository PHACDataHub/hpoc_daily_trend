# Create the "Days since 100 cases variable"; maybe move to the get_data file?
df_int_1 <- df_int %>%
        arrange(countryterritoryCode, date) %>% # had to arrange it like this to get cumsum and rollmean to work properly. Needs to be cumsum by time
        group_by(countryterritoryCode) %>%
        mutate(first_100 = if_else(cumsum(cases_weekly) >= 100, "True", "False")) %>%
        mutate(case_100 = if_else(first_100 == "True", cumsum(first_100 == "True"), false = NA_integer_)) %>%
        mutate(sdma = rollmean(cases_weekly, 7, na.pad = TRUE, align = "right")) %>%
        mutate(sdma_pop_thousand = sdma / popData2019 * 100000) %>%
        filter(sdma > 0) %>%
        mutate(countriesAndTerritories = str_replace_all(countriesAndTerritories, "_", " ")) %>%
        mutate(countriesAndTerritories = str_replace_all(countriesAndTerritories, " of America", "")) %>%
        mutate(label = if_else(date == max(date), as.character(countriesAndTerritories), NA_character_))

# Filter to only countries of interest
df_int_filter <- df_int_1 %>%
        filter(countryterritoryCode %in% c("CAN", "FRA", "BRA", "USA", "ITA", "DEU", "JPN", "KOR", "ESP", "AUS"))

# Start plotting
ggplot(df_int_filter, aes(case_100, sdma_pop_thousand, group = countryterritoryCode, colour = countryterritoryCode)) +
        geom_line(size = 2) +
        geom_text_repel(aes(label = label),
                size = 6,
                nudge_x = 1,
                nudge_y = 1,
                na.rm = TRUE
        ) +
        scale_y_continuous("Daily cases per 100,000 population", expand = c(0, 0), limits = c(0, NA)) +
        scale_x_continuous("Days since 100th reported case in country", expand = c(0, 0), limits = c(0, NA)) +
        scale_colour_tableau(palette = "Tableau 10") +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "none",
                text = element_text(size = 20)
        )
