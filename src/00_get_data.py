# Import the hospitalization data from the scrapped file

import pandas as pd

pt_hosp_raw = pd.read_excel('Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\DATA AND ANALYSIS\Web Scraping\Trend analysis\COVID-19 historical archive_CURRENT.xlsx', 
                    sheet_name='Hospitalization (current)',
                    nrows = 15)
