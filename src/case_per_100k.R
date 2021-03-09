# #Temp fig to potentially replace "case_per_100k.rmd" currently written in python
# 
#PT7 is created in "Cases Death Table.R"

df_Canada_100k<-PT7 %>%
  select(Jurisdiction, Date, Cases_Daily,Cases_Daily_7MA) %>%
  filter(Jurisdiction=="Canada") %>%
  left_join(PHACTrendR::latest_can_pop, by="Jurisdiction") %>%
  mutate(cases_daily_100k=Cases_Daily/Population*100000,
         cases_daily_100k_7MA=Cases_Daily_7MA/Population*100000)


key_100k_fig_date_updated<-format(max(df_Canada_100k$Date),"%b %d %Y")
key_100k_fig_rate<- round(df_Canada_100k$cases_daily_100k_7MA[df_Canada_100k$Date==max(df_Canada_100k$Date)],digits = 1)

ggplot(data=df_Canada_100k)+
  geom_bar(aes(x=Date, y=cases_daily_100k,colour="lightblue"),stat="identity", fill="lightblue") +
  geom_line(aes(x=Date,y=cases_daily_100k_7MA, colour="darkblue"),size=1)+
  annotate(geom="curve",x = as.Date("2020-05-10"),y=6,xend= as.Date("2020-04-26"), yend=5,
           curvature=0.3, arrow=arrow(length = unit(2,"mm")),size=1)+
  annotate("text",x=as.Date("2020-05-10"),y=6,label="Spring Peak (Apr.26): 4.6",vjust=0.5,hjust=0,size=9)+
  annotate(geom="curve",x = as.Date("2020-12-20"),y=24,xend= as.Date("2021-01-10"), yend=22.25,
           curvature=-0.3, arrow=arrow(length = unit(2,"mm")),size=1)+
  annotate("text",x=as.Date("2020-12-20"),y=23.5,label="Winter Peak (Jan.10): 21.7",vjust=0,hjust=1,size=9)+
  scale_x_date(breaks = ("month"),
               labels = label_date("%b %Y"),
               expand = c(0, 0),
               limits=c(as.Date("2020-03-08"),max(df_Canada_100k$Date)))+
  scale_colour_manual(name = "",
                      values =c('lightblue'='lightblue','darkblue'='darkblue'), labels = c('Daily cases per 100,000, 7-day moving average','Daily cases per 100,000'))+
  labs(x="",
       y="Cases per 100,000 population",
       caption=paste0("Updated daily (Sun-Thurs). Data as of: ",key_100k_fig_date_updated,". Today's value: (",key_100k_fig_date_updated,"): ",key_100k_fig_rate," cases/100,000"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid=element_blank(),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = c(0.2,0.9),
        axis.text = element_text(size=20),
        axis.title = element_text(size=26),
        legend.text = element_text(size=20))

