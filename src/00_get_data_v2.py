# Import the hospitalization data from the scrapped file
from selenium import webdriver 
from selenium.webdriver.common.keys import Keys 
import pandas
import numpy
import glob
import os
import re
import datetime
import time
import os
import sys


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


#Use Scott's code to get SALT data

''' 
TODO: 
Make an executable with args e.g. Myexe.exe -u nick -p 123456 —enddate 2020-05-05
Let users define start and end dates
'''

driver_path = ('chromedriver.exe')

download_path = os.getcwd()

if not os.path.exists(download_path):
    os.makedirs(download_path)

prefs = {"download.default_directory": download_path}
chromeOptions = webdriver.ChromeOptions()
chromeOptions.add_experimental_option('prefs', prefs)
chromeOptions.add_argument('--no-sandbox')
chromeOptions.add_argument('window-size=1420,1080')
chromeOptions.add_argument('--headless')
chromeOptions.add_argument('--disable-gpu')

driver = webdriver.Chrome(executable_path=driver_path, options=chromeOptions)

now = datetime.datetime.now()

newfilename = now.strftime("%Y-%m-%d_%H.%M.%S") + '-SubmittedReport.csv'

try: 
    os.rename(r'Submitted+Reports.csv', newfilename)
except:
    pass

def get_salt_data(user:str ,pwd:str) -> None:
    driver.get("https://www.cnphi-rcrsp.ca/cnphi/faces/login.xhtml?lang=en&jftfdi=&jffi=%2Flogin.xhtml")
    username = driver.find_element_by_id("inputUserName") 
    username.send_keys(user)
    password = driver.find_element_by_id("inputUserPassword") 
    password.send_keys(pwd)

    password.submit() 

    driver.get("https://www.cnphi-rcrsp.ca/salt/listings/listings.xhtml")
    driver.find_element_by_link_text("Export to csv").click()

if __name__ == "__main__":
    user = sys.argv[1]
    pwd = sys.argv[2]
    get_salt_data(user, pwd)

time.sleep(5)

driver.quit()