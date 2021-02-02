# -*- coding: utf-8 -*-
"""
Created on Wed Nov 25 09:40:23 2020

@author: FISLAM
"""

import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np

SALT = pd.read_csv(r'Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/Trend Report/rmd/testing.csv', index_col=0)


for i in ["Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec"]:   
    # subset data by PT
    SALT_PT = SALT[SALT.Jurisdiction==i]
    
    # create figure and axis objects with subplots()
    fig,ax = plt.subplots()

    # give plot a title
    plt.title(i, fontsize=36)
    # make a plot
    ax.bar(SALT_PT.Week, SALT_PT.week_tests_performed, align='center', alpha=0.5, label="Number of Tests Performed")
    # set x-axis label
    ax.set_xlabel("Week",fontsize=36)
    # format x axis ticks
    ax.tick_params(axis='x',labelsize=32,rotation=45)
    # format y axis ticks
    ax.tick_params(axis='y',labelsize=28)
    # set y-axis label
    ax.set_ylabel("Number of Tests",color="black",fontsize=36)
    # set format of y-axis ticks
    ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
        
    # twin object for two different y-axis on the sample plot
    ax2=ax.twinx()
    # make a plot with different y-axis using second axis object
    ax2.plot(SALT_PT.Week, SALT_PT["percent_positive"],color="red",marker="o",markersize=12,label="Percent Positive")
    ax2.set_ylabel("Percent Positive",color="black",fontsize=36)
    ax2.yaxis.set_major_formatter(mtick.PercentFormatter(1,decimals=0))
    ax2.tick_params(axis='y',labelsize=28)
    
    # add legend; put it outside the plot
    #fig.legend(loc="lower center")
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.1,
                 box.width, box.height * 0.9])
    ax.legend(loc='lower right', bbox_to_anchor=(0.5, -0.60),
          fancybox=True, shadow=True, ncol=1,prop=dict(size=20))
    ax2.legend(loc='lower left', bbox_to_anchor=(0.5, -0.60),
          fancybox=True, shadow=True, ncol=1,prop=dict(size=20))
    
    # remove top line
    ax.spines['top'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['right'].set_visible(False)
    
    # add data labels
    for x,y in zip(SALT_PT.Week,SALT_PT.percent_positive):

        label = "{:.1%}".format(y)
    
        plt.annotate(label, # this is the text
                     (x,y), # this is the point to label
                     textcoords="offset points", # how to position the text
                     xytext=(0,10), # distance from text to points (x,y)
                     ha='center') # horizontal alignment can be left, right or center
   

    
    #plt.show()
    
    #set dimensions of plot
    fig.set_size_inches(20, 8, forward=True)
    
    # save the plot as a file
    fig.savefig(i+'.jpg',
       format='jpeg',
    dpi=100,
    bbox_inches='tight')
            
    
    plt.clf()