hosp_per <- pt_hosp_icu %>%
  filter(type=="hospitalized") %>%
  select(prname,date,cases)

pr_pop <- pt_pop_raw %>%
  group_by(GEO) %>%
  filter(REF_DATE==max(REF_DATE)) %>%
  filter(Sex=="Both sexes") %>%
  filter(`Age group`=="All ages") %>%
  select(GEO,VALUE) %>%
  rename("prname" = "GEO") %>%
  rename("population"= "VALUE")

hosp_per <- hosp_per %>%
  left_join(pr_pop, by="prname")

hosp_per <- hosp_per  %>%
  mutate(hosp_per=(cases/population)*100000) %>%
  filter(date>"2020-03-31") %>%
  filter(prname%in%c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec"))

ggplot(data=hosp_per, aes(x=date, y=hosp_per, group=prname, colour=prname)) +
  geom_line(size = 1.5) +
  scale_y_continuous("Daily hospitalizations per 100,000 population", expand = c(0, 0), limits = c(0, NA)) +
  scale_x_date("",date_breaks = ("1 months"),labels = date_format("%b-%d"),expand = c(0,0)) +
  scale_colour_manual(values = c("red","blue","purple","light blue","#FFB300","#4CAF50")) +
  labs(prname='Jurisdiction', caption = paste0("Updated every Sun/Tues/Thurs. Last updated: ",
                                               hosp_per %>% filter(date==max(date)) %>% select(date) %>% distinct() %>% pull())) +
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
