# #Temp fig to potentially replace "case_per_100k.rmd" currently written in python
# 
# df_Canada_100k<-PT7 %>%
#         select(Jurisdiction, Date, Cases_Daily,Cases_Daily_7MA) %>%
#         filter(Jurisdiction=="Canada") %>%
#         left_join(PHACTrendR::latest_can_pop, by="Jurisdiction") %>%
#         mutate(cases_daily_100k=Cases_Daily/Population*100000,
#                cases_daily_100k_7MA=Cases_Daily_7MA/Population*100000)
# 
# 
# key_100k_fig_date_updated<-format(max(df_Canada_100k$Date),"%b %d %Y")
# key_100k_fig_rate<- round(df_Canada_100k$cases_daily_100k_7MA[df_Canada_100k$Date==max(df_Canada_100k$Date)],digits = 1)
# 
# ggplot(data=df_Canada_100k)+
#         geom_bar(aes(x=Date, y=cases_daily_100k,colour="lightblue"),stat="identity", fill="lightblue") +
#         geom_line(aes(x=Date,y=cases_daily_100k_7MA, colour="darkblue"),size=1)+
#         annotate(geom="curve",x = as.Date("2020-04-10"),y=6,xend= as.Date("2020-04-26"), yend=5,
#                  curvature=-0.3, arrow=arrow(length = unit(2,"mm")),size=1)+
#         annotate("text",x=as.Date("2020-03-10"),y=6.3,label="Spring Peak (Apr.26): 4.6",vjust=0,hjust=0,size=9)+
#         annotate(geom="curve",x = as.Date("2020-12-10"),y=23,xend= as.Date("2021-01-10"), yend=22.25,
#                  curvature=-0.3, arrow=arrow(length = unit(2,"mm")),size=1)+
#         annotate("text",x=as.Date("2020-12-10"),y=22.5,label="Winter Peak (Jan.10): 21.7",vjust=0,hjust=1,size=9)+
#         scale_x_date(breaks = ("month"),
#                 labels = label_date("%b %Y"),
#                 expand = c(0, 0),
#                 limits=c(as.Date("2020-03-08"),max(df_Canada_100k$Date)))+
#         scale_colour_manual(name = "",
#                             values =c('lightblue'='lightblue','darkblue'='darkblue'), labels = c('Daily cases per 100,000, 7-day moving average','Daily cases per 100,000'))+
#         labs(x="",
#              y="Cases per 100,000 population",
#              caption=paste0("Updated daily (Sun-Thurs). Data as of: ",key_100k_fig_date_updated,". Today's value: (",key_100k_fig_date_updated,"): ",key_100k_fig_rate," cases/100,000"))+
#         theme(panel.background = element_blank(),
#               panel.border = element_rect(colour = "black", fill=NA),
#               panel.grid=element_blank(),
#               plot.caption = element_text(hjust = 0,size=20),
#               legend.position = c(0.2,0.9),
#               axis.text = element_text(size=20),
#               axis.title = element_text(size=26),
#               legend.text = element_text(size=20))
# 
# 
# 


for (i in list_pt){
        cat('\n')  
        cat("#", i, "COVID-19 Indicators (Cases and Deaths)", "\n") 
        
        df_filter <- df_weekly_changes %>%
                filter(Jurisdiction == i) %>%
                filter(Date >= as.Date("2020-03-08"))%>%
                ungroup()

        
        table_filter_case <- data.frame(
                desc = c(paste0("Reported on ", format(max(df_filter$Date),"%B %d")), "7-day moving average (per day):", "Weekly percent change"),
                value = c(number(df_filter$Cases_Daily[df_filter$Date==max(df_filter$Date)],big.mark=","),
                        number(df_filter$Cases_Daily_7MA[df_filter$Date==max(df_filter$Date)],big.mark=","),
                        PHACTrendR::turn_num_to_percent_change(df_filter$Weekly_Change_Cases[df_filter$Date==max(df_filter$Date)])))
        
        table_filter_death <- data.frame(
                desc = c(paste0("Reported on ", format(max(df_filter$Date),"%B %d")), "7-day moving average (per day):", "Weekly percent change"),
                value = c(number(df_filter$Deaths_Daily[df_filter$Date==max(df_filter$Date)],big.mark=","),
                          number(df_filter$Deaths_Daily_7MA[df_filter$Date==max(df_filter$Date)],big.mark=","),
                          PHACTrendR::turn_num_to_percent_change(df_filter$Weekly_Change_Deaths[df_filter$Date==max(df_filter$Date)],accuracy = 0.1)))
        
        
        # Code for conditional colouring of the text in the code
        cols_case <- matrix("black", nrow(table_filter_case), ncol(table_filter_case))
        cols_case[3, 2] <- if_else(str_detect(table_filter_case[3, 2], "\\+"), "red",
                                   if_else(str_detect(table_filter_case[3, 2], "\\-"), "green3", "black")
        )
        
        cols_death <- matrix("black", nrow(table_filter_death), ncol(table_filter_death))
        cols_death[3, 2] <- if_else(str_detect(table_filter_death[3, 2], "\\+"), "red",
                                   if_else(str_detect(table_filter_death[3, 2], "\\-"), "green3", "black")
        )
        
        # Charting the plots
        p1 <- ggplot(df_filter, aes(x = Date, y = Cases_Daily)) +
                ggtitle(bquote("Reported"~bold("cases")~"by date,"~.(i))) +
                geom_col(aes(fill = "Reported cases"), width = 0.5) +
                geom_line(aes(
                        colour = "7 day moving average (7MA)",
                        y = Cases_Daily_7MA),size=1.5) +
                scale_x_date(
                        NULL,
                        breaks = scales::breaks_width("6 weeks"),
                        labels = label_date("%d%b"),
                        expand = c(0, 0),
                        limits=c(as.Date("2020-03-08"),max(df_filter$Date))
                ) +
                scale_y_continuous(
                        "Number of cases",
                        labels = comma,
                        expand = c(0, 0)
                ) +
                scale_fill_manual(name = "", values = c("Reported cases" = "lightblue")) +
                scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "darkblue")) +
                theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20)
                )
        
        p2 <- ggplot(df_filter, aes(x = Date, y = Deaths_Daily)) +
                ggtitle(bquote("Reported"~bold("deaths")~"by date,"~.(i))) +
                geom_col(aes(fill = "Reported deaths"), width = 0.5) +
                geom_line(aes(
                        colour = "7 day moving average (7MA)",
                        y = Deaths_Daily_7MA),size=1.5) +
                scale_x_date(
                        NULL,
                        breaks = scales::breaks_width("6 weeks"),
                        labels = label_date("%d%b"),
                        expand = c(0, 0),
                        limits=c(as.Date("2020-03-08"),max(df_filter$Date))
                ) +
                scale_y_continuous(
                        "Number of deaths",
                        labels = comma,
                        expand = c(0, 0)
                ) +
                scale_fill_manual(name = "", values = c("Reported deaths" = "grey")) +
                scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "black")) +
              theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20))
        
        p3 <- ggplot(df_filter, aes(x = Date, y = Cases_Daily)) +
                ggtitle(paste0("Daily cases, past 2 weeks, ", i)) +
                geom_col(aes(fill = "Reported cases"), width = 0.5) +
                geom_line(aes(
                        colour = "7 day moving average (7MA)",
                        y = Cases_Daily_7MA),size=1.5) +
                scale_x_date(
                        NULL,
                        breaks = scales::breaks_width("5 days"),
                        labels = label_date("%d%b"),
                        limits = c(
                                df_filter %>% filter(Date == max(Date)) %>% select(Date) %>% pull() - weeks(2),
                                df_filter %>% filter(Date == max(Date)) %>% select(Date) %>% pull() + 1
                        ),
                        expand = c(0, 0)
                ) +
                scale_y_continuous(
                        "Number of cases",
                        labels = comma,
                        limits = c(0, max(max(df_filter$Cases_Daily[df_filter$Date>=max(df_filter$Date)-weeks(2)]),
                                          (max(df_filter$Cases_Daily_7MA[df_filter$Date>=max(df_filter$Date)-weeks(2)])))),
                        expand = c(0, 0)
                ) +
                scale_fill_manual(name = "", values = c("Reported cases" = "lightblue")) +
                scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "darkblue")) +
                theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20)
                )
        
        p4 <- ggplot(df_filter, aes(x = Date, y = Deaths_Daily)) +
                ggtitle(paste0("Daily deaths, past 2 weeks, ", i)) +
                geom_col(aes(fill = "Reported deaths"), width = 0.5) +
                geom_line(aes(
                        colour = "7 day moving average (7MA)",
                        y = Deaths_Daily_7MA),
                        size=1.5) +
                scale_x_date(
                        NULL,
                        breaks = scales::breaks_width("5 days"),
                        labels = label_date("%d%b"),
                        limits = c(
                                df_filter %>% filter(Date == max(Date)) %>% select(Date) %>% pull() - weeks(2),
                                df_filter %>% filter(Date == max(Date)) %>% select(Date) %>% pull() + 1
                        ),
                        expand = c(0, 0)
                ) +
                scale_y_continuous(
                        "Number of cases",
                        labels = comma,
                        limits = c(0, max(max(df_filter$Deaths_Daily[df_filter$Date>=max(df_filter$Date)-weeks(2)]),
                                          (max(df_filter$Deaths_Daily_7MA[df_filter$Date>=max(df_filter$Date)-weeks(2)])))),
                        expand = c(0, 0)
                ) +
                scale_fill_manual(name = "", values = c("Reported deaths" = "grey")) +
                scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "black")) +
                theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20))
        if (i=="Canada"){
                p2 = p2 + labs(caption = paste0("Spring peak: May 6, 176.6 deaths \nWinter peak: Jan. 26, 161.2 deaths\n","Updated daily (Sun-Thurs). Data as of: ",format(max(df_filter$Date),"%B %d"))) +
                theme(plot.caption = element_text(hjust = 0))
        } else {
                p2 = p2 + labs(caption = paste0("Updated daily (Sun-Thurs). Data as of: ",format(max(df_filter$Date),"%B %d"))) +
                        theme(plot.caption = element_text(hjust = 0))
        }
        # Set the layout of the plots and tables in Patchwork syntax
        output <- p1 + p3 + gridExtra::tableGrob(table_filter_case[1:3, c("desc", "value")],
                                                 theme = ttheme_minimal(core = list(fg_params = list(
                                                         col = cols_case,
                                                         hjust = 0,
                                                         x = 0.0,
                                                         fontsize = 18
                                                 ))),
                                                 rows = NULL,
                                                 cols = NULL
        ) +
                p2 + p4 + gridExtra::tableGrob(table_filter_death[1:3, c("desc", "value")],
                                               theme = ttheme_minimal(core = list(fg_params = list(
                                                       col = cols_death,
                                                       hjust = 0,
                                                       x = 0.0,
                                                       fontsize = 18
                                               ))),
                                               rows = NULL,
                                               cols = NULL
                ) +
                plot_layout(widths = c(2, 1, 1), ncol = 3)
        print(output)
        cat('\n') 
}

