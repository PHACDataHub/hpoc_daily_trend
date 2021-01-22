# -*- coding: utf-8 -*-
"""
Created on Fri Jan 22 18:05:37 2021

@author: FISLAM
"""

import pandas
import glob
import os
import re

# Import the hospitalization data from the scrapped file

## Read the hospitalization data
pt_hosp_raw = pandas.read_excel('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\Web Scraping\Trend analysis\COVID-19 historical trends.xlsx', 
                    sheet_name='Hospitalization (current)',
                    nrows = 15)

## Read the ICU data
pt_icu_raw = pandas.read_excel('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\Web Scraping\Trend analysis\COVID-19 historical trends.xlsx', 
                    sheet_name='ICU (current)',
                    nrows = 15)