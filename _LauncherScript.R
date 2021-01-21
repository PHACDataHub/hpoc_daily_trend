#Script to generate Daily Trend Report!

today<-format(Sys.Date(), "%d%b%Y")

library("rmarkdown")

setwd("C:/rmd/")

rmarkdown::render('generate_daily_trend_report-v2.rmd',
                  output_file = paste0('DailyTrendReport_', today,'.pptx'))
