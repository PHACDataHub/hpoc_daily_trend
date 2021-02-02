# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 22:48:00 2020

@author: FISLAM
"""

import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import matplotlib.dates as mdates

National_Daily = pd.read_csv(r'Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/Trend Report/rmd/testing_daily.csv', index_col=0)
National_Daily['Date'] = pd.to_datetime(National_Daily['Date'])

# create figure and axis objects with subplots()
fig,ax = plt.subplots()

# give plot a title
plt.title("Number of People Tested and Percent Positivity Across Canada\n7 Day Moving Average", fontsize=36)
# make a plot
ax.bar(National_Daily.Date, National_Daily.tests_performed, align='center', alpha=0.5, label="Number of Tests")
# set x-axis label
ax.set_xlabel("Date",fontsize=36)
# set format of x-axis ticks
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%y'))
# format x axis ticks
ax.xaxis.set_major_locator(mdates.MonthLocator(bymonthday=1))
# remove whitespace from graph
ax.margins(x=0)
# increase padding between tick marks and x axis
ax.tick_params(axis='x', which='major', pad=10)
# format y axis ticks
ax.tick_params(axis='y',labelsize=28)
# set y-axis label
ax.set_ylabel("Number of Tests",color="black",fontsize=36)
# set format of y-axis ticks
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
    
# twin object for two different y-axis on the sample plot
ax2=ax.twinx()
# make a plot with different y-axis using second axis object
ax2.plot(National_Daily.Date, National_Daily["percent_positive"],color="red",marker="",markersize=12,label="Percent Positive")
ax2.set_ylabel("Percent Positive",color="black",fontsize=36)
ax2.yaxis.set_major_formatter(mtick.PercentFormatter(1,decimals=0))
ax2.tick_params(axis='y',labelsize=28)

# add legend; put it outside the plot
box = ax.get_position()
ax.set_position([box.x0, box.y0 + box.height * 0.1,
             box.width, box.height * 0.9])
ax.legend(loc='lower right', bbox_to_anchor=(0.5, -0.60),
      fancybox=True, shadow=True, ncol=1,prop=dict(size=20))
ax2.legend(loc='lower left', bbox_to_anchor=(0.5, -0.60),
      fancybox=True, shadow=True, ncol=1,prop=dict(size=20))

# remove top line
ax.spines['top'].set_visible(False)

#plt.show()

#set dimensions of plot
fig.set_size_inches(20, 8, forward=True)

# save the plot as a file
fig.savefig('National_Testing.jpg',
   format='jpeg',
dpi=100,
bbox_inches='tight')

plt.clf()