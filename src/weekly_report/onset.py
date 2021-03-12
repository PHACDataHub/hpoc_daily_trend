# -*- coding: utf-8 -*-
"""
Created on Thu Jan 21 15:44:06 2021

@author: FISLAM
"""

import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import matplotlib.dates as mdates

Onset = pd.read_csv(r'Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/Trend Report/rmd/onset.csv', index_col=0)
Onset['onsetdate'] = pd.to_datetime(Onset['onsetdate'])

# create figure and axis objects with subplots()
fig,ax = plt.subplots()

# make a plot
ax.bar(Onset.onsetdate, Onset.daily_case, align='center', alpha=0.5, color = "dimgray", label="No. of cases")
# set x-axis label
ax.set_xlabel("Onset Date",fontsize=28)
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
ax.set_ylabel("No. of cases",color="black",fontsize=30)
# set format of y-axis ticks
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
    
# twin object for two different y-axis on the sample plot
ax2=ax.twinx()
# make a plot with different y-axis using second axis object
ax2.plot(Onset.onsetdate, Onset["mean_delay"],color="red",marker="",markersize=12,label="Mean no. of days")
ax2.set_ylabel("Onset to lab collection (days)",color="black",fontsize=30)
#ax2.yaxis.set_major_formatter(mtick.PercentFormatter(1,decimals=0))
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
fig.savefig('Onset.jpg',
   format='jpeg',
dpi=100,
bbox_inches='tight')

plt.clf()