import pandas
import glob
import os
import re

# Import the hospitalization data from the scrapped file

## Read the hospitalization data
pt_hosp_raw = pandas.read_excel('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\Web Scraping\Trend analysis\COVID-19 historical archive_CURRENT.xlsx', 
                    sheet_name='Hospitalization (current)',
                    nrows = 15)

## Read the ICU data
pt_icu_raw = pandas.read_excel('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\Web Scraping\Trend analysis\COVID-19 historical archive_CURRENT.xlsx', 
                    sheet_name='ICU (current)',
                    nrows = 15)

# Import the all cases data
list_of_files = glob.glob('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\SAS_Analysis\Domestic data\*.rds')
files_filter = [file for file in list_of_files if 'NEW' and '$' not in file]
latest_file = max(files_filter, key=os.path.getmtime)
