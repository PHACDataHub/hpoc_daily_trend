
qry_cases_raw<-PHACTrendR::import_DISCOVER_data()

qry_lab_onset <- qry_cases_raw %>%
  janitor::clean_names() %>%
  filter(pt != "Repatriate") %>%
  filter(onsetdate >= "2020-03-01") %>%
  filter(onsetdate <= (max(onsetdate - days(15)))) %>%
  select(onsetdate, earliestlabcollectiondate) %>%
  filter(!is.na(onsetdate)) %>%
  mutate(delay = earliestlabcollectiondate - onsetdate) %>%
  filter(between(delay, 0, 15)) %>% # filtering any outliers as identified in the SAS file
  group_by(onsetdate) %>%
  dplyr::summarise(mean_delay = mean(delay, na.rm = TRUE),
                   daily_case = n())


# Start plotting
coeff <- 100

ggplot(qry_lab_onset, aes(x = onsetdate)) +
        geom_bar(aes(y = daily_case/coeff), stat = "identity", fill = "lightgrey", width = 0.5) +
        geom_line(aes(y = mean_delay), stat = "identity", size = 1, colour = "red") +
        scale_y_continuous(
                # Features of the first axis
                name = "Onset to lab collection (days)",
                
                # Add a second axis and specify its features
                sec.axis = sec_axis(~.*coeff, name="# of case reports")
        ) + 
        scale_x_date("Onset date",
                     breaks = scales::breaks_width("3 weeks"),
                     labels = label_date("%d%b"),
                     expand = c(0, 0)) +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "none",
                text = element_text(size = 20)
        ) +
        labs(caption = "* Red line represents the lext y-axis")
