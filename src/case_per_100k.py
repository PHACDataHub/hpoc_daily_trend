# -*- coding: utf-8 -*-
"""
Created on Mon Dec  7 11:15:24 2020

@author: FISLAM
"""
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
# import pathlib

from mpl_axes_aligner import align
# from pathlib import Path

case_per_100k = pd.read_csv(r'Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/Trend Report/rmd/case_per_100k.csv', index_col=0)
case_per_100k['Date'] = pd.to_datetime(case_per_100k['Date'])

# create figure and axis objects with subplots()
fig,ax = plt.subplots()

# make a plot
ax.bar(case_per_100k.Date, case_per_100k.Cases_Daily, align='center', color="lightblue", alpha=1.0, label="Number of daily cases")
# set x-axis label
ax.set_xlabel("Date",fontsize=36)
# set format of x-axis ticks
ax.xaxis.set_major_formatter(mpl.ticker.StrMethodFormatter(("%Y-%m")))
# format x axis ticks
ax.tick_params(axis='x',labelsize=24)
# determine frequency of ticks
ax.xaxis.set_major_locator(mdates.MonthLocator())
# remove whitespace from plot
ax.margins(x=0)
# set y-axis label
ax.set_ylabel("Number of Cases",color="black",fontsize=28)
# format y axis ticks
ax.tick_params(axis='y',labelsize=24)
# set format of y-axis ticks
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
    
# twin object for two different y-axis on the sample plot
ax2=ax.twinx()
# make a plot with different y-axis using second axis object
ax2.plot(case_per_100k.Date, case_per_100k["Case_per_100K_7MA"],color="blue",marker="",label="Daily cases per 100,000, 7 day moving average")
ax2.set_ylabel("Cases per 100,000 Population\n7 Day Moving Average",color="black",fontsize=28)
# align secondary y-axis with primary y-axis
align.yaxes(ax, 0, ax2, 0)
# set fontsize of ticks
ax2.tick_params(axis='y',labelsize=24)

# remove top line
ax.spines['top'].set_visible(False)

# add legend
lines, labels = ax.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax2.legend(lines + lines2, labels + labels2, loc="upper left", prop=dict(size=18))
    
    # add data labels
#    for x,y in zip(SALT_PT.Week,SALT_PT.Percent_positive):

 #       label = "{:.1%}".format(y)
    
  #      plt.annotate(label, # this is the text
   #                  (x,y), # this is the point to label
    #                 textcoords="offset points", # how to position the text
     #                xytext=(0,10), # distance from text to points (x,y)
      #               ha='center') # horizontal alignment can be left, right or center
   

    
#plt.show()
    
#Add caption - now adding in .rmd code chunk
# date_updated= max((case_per_100k['Date'])).strftime('%B %d')
# current_rate= str(case_per_100k[case_per_100k.Date == max((case_per_100k['Date']))].Case_per_100K_7MA.round(2).item())
# text="Spring peak: April 26, 4.55 cases/100k  \nWinter peak: January 10, 21.74 cases/100k \nToday's value ("+date_updated+"): "+current_rate+" cases/100k \nUpdated daily (Sun-Thurs). Data as of: "+date_updated
# fig.text(0, -0.1, text, ha='left',  size=20)

#set dimensions of plot
fig.set_size_inches(24, 10.5, forward=True)

# save the plot as a file

# # getting to "output" directory
# main_folder_string=str(Path(Path.cwd()).parents[0]) 
# fig.savefig(main_folder_string+'./output/Canada_case_per_100k.jpg',
#    format='jpeg',
# dpi=300,
# bbox_inches='tight')


fig.savefig('Canada_case_per_100k.jpg',
   format='jpeg',
dpi=300,
bbox_inches='tight')

plt.clf()
