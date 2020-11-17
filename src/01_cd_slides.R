# Defining parameters for this set of slides
jurisdiction <- if (Sys.getenv("pt") == "") "Canada" else Sys.getenv("pt")
two_weeks_ago <- params$date - weeks(2)

df_filter <- df %>%
    filter(prname == jurisdiction) %>%
    filter(date >= "2020-03-08")

table_filter_case <- data.frame(
    desc = c(paste0("Reported on ", params$date), "7-day moving average (per day):", "Weekly percent change"),
    value = c(
        df_filter %>% filter(date == max(date)) %>% select(numtoday) %>% pull(),
        df_filter %>% filter(date >= max(date) - days(6)) %>% summarise(average = mean(numtoday, na.rm = T)) %>% pull(),
        df_filter %>% mutate(sdma = rollmean(numtoday, 7, na.pad = TRUE, align = "right")) %>%
            mutate(wow = (sdma - lag(sdma, 7)) / lag(sdma, 7)) %>%
            filter(date == max(date)) %>%
            select(wow) %>%
            pull()
    )
) %>% mutate_if(is.numeric, round, digits = 2)

table_filter_death <- data.frame(
    desc = c(paste0("Reported on ", params$date), "7-day moving average (per day):", "Weekly percent change"),
    value = c(
        df_filter %>% filter(date == max(date)) %>% select(numdeathstoday) %>% pull(),
        df_filter %>% filter(date >= max(date) - days(6)) %>% summarise(average = mean(numdeathstoday, na.rm = T)) %>% pull(),
        df_filter %>% mutate(sdma = rollmean(numdeathstoday, 7, na.pad = TRUE, align = "right")) %>%
            mutate(wow = (sdma - lag(sdma, 7)) / lag(sdma, 7)) %>%
            filter(date == max(date)) %>%
            select(wow) %>%
            pull()
    )
) %>% mutate_if(is.numeric, round, digits = 2)

# Code for conditional colouring of the text in the code
cols_case <- matrix("black", nrow(table_filter_case), ncol(table_filter_case))
cols_case[3, 2] <- if_else(table_filter_case[3, 2] > 0, "red",
    if_else(table_filter_case[3, 2] < 0, "green", "black")
)

cols_death <- matrix("black", nrow(table_filter_death), ncol(table_filter_death))
cols_death[3, 2] <- if_else(table_filter_death[3, 2] > 0, "red",
    if_else(table_filter_death[3, 2] < 0, "green", "black")
)

# Charting the plots
p1 <- ggplot(df_filter, aes(x = date, y = numtoday)) +
    ggtitle(paste0("Reported COVID19 cases by date, ", jurisdiction)) +
    geom_col(aes(fill = "Reported cases"), width = 0.5) +
    geom_line(aes(
        colour = "7 day moving average (7MA)",
        y = rollmean(numtoday, 7, na.pad = TRUE, align = "right")
    )) +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("3 weeks"),
        labels = label_date("%d%b"),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma,
        expand = c(0, 0)
    ) +
    scale_fill_manual(name = "", values = c("Reported cases" = "lightblue")) +
    scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "blue")) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        text = element_text(size = 20)
    )

p2 <- ggplot(df_filter, aes(x = date, y = numdeathstoday)) +
    ggtitle(paste0("Reported COVID19 deaths by date, ", jurisdiction)) +
    geom_col(aes(fill = "Reported deaths"), width = 0.5) +
    geom_line(aes(
        colour = "7 day moving average (7MA)",
        y = rollmean(numdeathstoday, 7, na.pad = TRUE, align = "right")
    )) +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("3 weeks"),
        labels = label_date("%d%b"),
        expand = c(0, 0)
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
        text = element_text(size = 20)
    )

p3 <- ggplot(df_filter, aes(x = date, y = numtoday)) +
    ggtitle(paste0("Daily cases, past 2 weeks, ", jurisdiction)) +
    geom_col(aes(fill = "Reported cases"), width = 0.5) +
    geom_line(aes(
        colour = "7 day moving average (7MA)",
        y = rollmean(numtoday, 7, na.pad = TRUE, align = "right")
    )) +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("5 days"),
        labels = label_date("%d%b"),
        limits = c(
            df_filter %>% filter(date == max(date)) %>% select(date) %>% pull() - weeks(2),
            df_filter %>% filter(date == max(date)) %>% select(date) %>% pull() + 1
        ),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma,
        limits = c(0, df_filter %>%
            filter(date >= max(date) - weeks(2)) %>%
            select(numtoday) %>% max()),
        expand = c(0, 0)
    ) +
    scale_fill_manual(name = "", values = c("Reported cases" = "lightblue")) +
    scale_colour_manual(name = "", values = c("7 day moving average (7MA)" = "blue")) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        text = element_text(size = 20)
    )

p4 <- ggplot(df_filter, aes(x = date, y = numdeathstoday)) +
    ggtitle(paste0("Daily deaths, past 2 weeks, ", jurisdiction)) +
    geom_col(aes(fill = "Reported deaths"), width = 0.5) +
    geom_line(aes(
        colour = "7 day moving average (7MA)",
        y = rollmean(numdeathstoday, 7, na.pad = TRUE, align = "right")
    )) +
    scale_x_date(
        NULL,
        breaks = scales::breaks_width("5 days"),
        labels = label_date("%d%b"),
        limits = c(
            df_filter %>% filter(date == max(date)) %>% select(date) %>% pull() - weeks(2),
            df_filter %>% filter(date == max(date)) %>% select(date) %>% pull() + 1
        ),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        "Number of cases",
        labels = comma,
        limits = c(0, df_filter %>%
            filter(date >= max(date) - weeks(2)) %>%
            select(numdeathstoday) %>% max()),
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
p1 + p3 + gridExtra::tableGrob(table_filter_case[1:3, c("desc", "value")],
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
