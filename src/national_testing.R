#national testing figure attempt in R

#need to rescale percent positive to be close to tests performed in order to plot on same axis in R

#scaling factor allows us to plot percent positivity next to number of tests. increase if you want percent positive to be higher on the graph
scaling_factor<-1800000

National_Daily$percent_positive_rescaled<-National_Daily$percent_positive*scaling_factor
ggplot(data=National_Daily)+
  # geom_area(aes(x=Date, y=tests_performed_7MA,colour="lightblue"), alpha=0.8, fill="lightblue") +
  geom_bar(aes(x=Date,y=tests_performed, colour="lightblue"),stat="identity",fill="lightblue")+
  geom_line(aes(x=Date, y=percent_positive_rescaled,colour="red"),size=1.25)+
  scale_y_continuous(name = "Number of tests",
                     labels = label_number(big.mark = ","),
                     breaks=seq(0,150000,25000),
                     sec.axis = sec_axis(~./(scaling_factor/100), name = "Percent Positive (%)"))+
  scale_x_date(breaks = ("month"),
               labels = label_date("%b %Y"),
               expand = c(0, 0))+
  scale_colour_manual(name = "",
                      values =c('lightblue'='lightblue','red'='red'), labels = c('Number of tests','Percent positive'))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid=element_blank(),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "bottom",
        axis.text = element_text(size=20),
        axis.title = element_text(size=26),
        legend.text = element_text(size=20))

  