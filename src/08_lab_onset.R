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
