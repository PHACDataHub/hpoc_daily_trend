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

SALT = pd.read_csv(r'C:/Users/FISLAM/Documents/testing.csv', index_col=0)


for i in ["Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec"]:   
   # subset data by PT
    SALT_PT = SALT[SALT.Jurisdiction==i]
    
    # create figure and axis objects with subplots()
    fig,ax = plt.subplots()
    # give plot a title
    plt.title(i)
    # make a plot
    ax.bar(SALT_PT.Week, SALT_PT.Week_patients_tested, align='center', alpha=0.5)
    # set x-axis label
    ax.set_xlabel("Week",fontsize=14)
    # set y-axis label
    ax.set_ylabel("Number of Tests",color="black",fontsize=14)
    # set format of y-axis ticks
    ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
        
    # twin object for two different y-axis on the sample plot
    ax2=ax.twinx()
    # make a plot with different y-axis using second axis object
    ax2.plot(SALT_PT.Week, SALT_PT["Percent_positive"],color="blue",marker="o")
    ax2.set_ylabel("Percent Positive",color="black",fontsize=14)
    ax2.yaxis.set_major_formatter(mtick.PercentFormatter(1))
    
    # add data labels
    for x,y in zip(SALT_PT.Week,SALT_PT.Percent_positive):

        label = "{:.1%}".format(y)
    
        plt.annotate(label, # this is the text
                     (x,y), # this is the point to label
                     textcoords="offset points", # how to position the text
                     xytext=(0,10), # distance from text to points (x,y)
                     ha='center') # horizontal alignment can be left, right or center
    
    # remove top line
    ax.spines['top'].set_visible(False)
    
    plt.show()
    
    print(i)

# save the plot as a file
#fig.savefig('two_different_y_axis_for_single_python_plot_with_twinx.jpg',
#            format='jpeg',
#            dpi=100,
#            bbox_inches='tight')








# create figure and axis objects with subplots()
#fig,ax = plt.subplots()
# make a plot
#ax.plot(SALT_PT.Week, SALT_PT.Week_patients_tested, color="red", marker="o")
# set x-axis label
#ax.set_xlabel("Week",fontsize=14)
# set y-axis label
#ax.set_ylabel("Tests",color="red",fontsize=14)

# twin object for two different y-axis on the sample plot
#ax2=ax.twinx()
# make a plot with different y-axis using second axis object
#ax2.plot(SALT_PT.Week, SALT_PT["Percent_positive"],color="blue",marker="o")
#ax2.set_ylabel("Percent_positive",color="blue",fontsize=14)
#plt.show()
