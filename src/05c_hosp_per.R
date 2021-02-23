hosp_per <- all_hosp_data %>%
  filter(type=="hospitalized" & !Jurisdiction=="Repatriated travellers") %>%
  select(Jurisdiction,Date,cases)

hosp_per <- hosp_per %>%
  left_join(latest_can_pop, by="Jurisdiction")

hosp_per <- hosp_per  %>%
  mutate(hosp_per=(cases/Population)*100000) %>%
  filter(Date>"2020-03-31") %>%
  filter(Jurisdiction %in% recode_PT_names_to_big(PHACTrendR::PTs_big6))

ggplot(data=hosp_per, aes(x=Date, y=hosp_per, group=Jurisdiction, colour=Jurisdiction)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Daily hospitalizations per 100,000 population", expand = c(0, 0), limits = c(0, NA)) +
  scale_x_date("",date_breaks = ("1 months"),labels = date_format("%b-%d"),expand = c(0,0)) +
  scale_colour_manual(values = c("red","blue","purple","light blue","#FFB300","#4CAF50")) +
  labs(Jurisdiction='Jurisdiction', caption = paste0("Updated daily (Sun-Thurs). Data as of: ",format(max(hosp_per$Date), "%B %d"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key=element_blank(),
    legend.key.size = unit(3,"line"),
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 0)
  ) + guides(col = guide_legend(nrow = 1))
