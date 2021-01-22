# -*- coding: utf-8 -*-
"""
Created on Fri Jan 22 18:06:21 2021

@author: FISLAM
"""

import pandas
import glob
import os
import re

# Import the all cases data
list_of_files = glob.glob('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\SAS_Analysis\Domestic data\*.rds')
latest_file = max(list_of_files, key=os.path.getmtime)