for (i in list_pt){
        cat('\n')  
        cat("#", i, "COVID-19 Indicators (Cases and Deaths)", "\n") 
        
        #df_moving_averages is the dataset that we create in Cases_Death Table - using it to keep things consistent (Can 7dMA is corrected when PT non-reports over there)
        df_filter <- df_weekly_changes %>%
                filter(Jurisdiction == i) %>%
                filter(Date >= "2020-03-08") 

        
        table_filter_case <- data.frame(
                desc = c(paste0("Reported on ", params$date), "7-day moving average (per day):", "Weekly percent change"),
                value = c(format(as.numeric(df_filter$Cases_Daily[df_filter$Date==max(df_filter$Date)],big.mark=",")),
                        format(round(as.numeric(df_filter$Cases_Daily_7MA[df_filter$Date==max(df_filter$Date)],big.mark=","))),
                        percent(df_filter$Weekly_Change_Cases[df_filter$Date==max(df_filter$Date)],accuracy = 0.1)))
        
        table_filter_death <- data.frame(
                desc = c(paste0("Reported on ", params$date), "7-day moving average (per day):", "Weekly percent change"),
                value = c(format(as.numeric(df_filter$Deaths_Daily[df_filter$Date==max(df_filter$Date)],big.mark=",")),
                          format(round(as.numeric(df_filter$Deaths_Daily_7MA[df_filter$Date==max(df_filter$Date)],big.mark=","))),
                          percent(df_filter$Weekly_Change_Deaths[df_filter$Date==max(df_filter$Date)],accuracy = 0.1)))
        
        
        # Code for conditional colouring of the text in the code
        cols_case <- matrix("black", nrow(table_filter_case), ncol(table_filter_case))
        cols_case[3, 2] <- if_else(table_filter_case[3, 2] > 0, "red",
                                   if_else(table_filter_case[3, 2] < 0, "green3", "black")
        )
        
        cols_death <- matrix("black", nrow(table_filter_death), ncol(table_filter_death))
        cols_death[3, 2] <- if_else(table_filter_death[3, 2] > 0, "red",
                                    if_else(table_filter_death[3, 2] < 0, "green3", "black")
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
                labs(caption = paste0("Updated daily (Sun-Thurs). Data as of: ",format(max(df_filter$Date),"%B %d"))) +
                theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20),
                        plot.caption = element_text(hjust = 0)
                )
        
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
                theme(
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "bottom",
                        text = element_text(size = 20)
                )
        
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
