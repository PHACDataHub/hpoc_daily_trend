jurisdiction <- if (Sys.getenv("age_prname") == "Canada") "Canada" else c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec")

# Filter province
qry_cases_filter <- qry_cases %>%
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

# Compute cases per 100K

qry_cases_per <- qry_cases_filter %>%
  left_join(pt_pop20, by=c("Jurisdiction"="Jurisdiction", "agegroup20"="AgeGroup20")) %>%
  mutate(cases_per = (cases/Population20)*100000) %>%
  mutate(sdma_per = rollmean(cases_per, 7, na.pad = TRUE, align = "right")) %>%
  filter(earliestdate >= "2020-06-01") %>%
  factor_PT_west_to_east(size="big")

qry_cases_per$Jurisdiction <- recode(qry_cases_per$Jurisdiction, "Canada"="", "British Columbia"="BC","Alberta"="AB","Saskatchewan"="SK","Manitoba"="MB","Quebec"="QC","Ontario"="ON")

# Plot
plot<-ggplot(qry_cases_per, aes(x = earliestdate, y = sdma_per, colour = agegroup20)) +
    geom_line(size = 1.5) +
    facet_wrap(~Jurisdiction, scales = "free") +
    scale_y_continuous("Number of reported cases per 100,000\n(7 Day moving average)", labels = comma_format(accuracy = 1)) +
    scale_x_date(
        "Date of illness onset",
        breaks = scales::breaks_width("6 weeks"),
        labels = label_date("%d%b")
    ) +
    geom_rect(aes(
        xmin = qry_cases_filter %>% filter(earliestdate == max(earliestdate) - days(14)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
        xmax = qry_cases_filter %>% filter(earliestdate == max(earliestdate)) %>% select(earliestdate) %>% distinct() %>% pull() %>% as.Date(),
        ymin = -Inf,
        ymax = Inf
    ),
    alpha = 0.01, fill = "grey", inherit.aes = FALSE
    ) +
    scale_color_manual(values=c("#3498DB","#E74C3C","#27AE60","#A04000","#9B59B6")) +
    #scale_colour_wsj() +
    labs(caption = paste0(
        "* Shaded area represents approximate lag in reporting
        \nUpdated Daily (Sun-Thurs). Data as of: ",format(as.Date(max(qry_cases_raw$phacreporteddate, na.rm=TRUE)),"%B %d"))) +
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
if (jurisdiction[1]=="Canada"){
  plot<-plot+theme(strip.text=element_blank())
}
plot
