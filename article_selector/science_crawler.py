import pandas as pd
import requests
import re
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
import time



def transforma_nome(x):
    x = x.replace(',', '').replace(':', '')
    return x.replace(' ', '%20')

if __name__ == '__main__':

    site_base = 'https://www.sciencedirect.com/'
    artigos = pd.read_excel(r'..\Downloads\Artigos relevantes sobre Qualidade percebida.xlsx')
    artigos_emerald = artigos[artigos['Base'].str.strip() == 'Science']
    site_final = [site_base + 'search?qs=' + transforma_nome(x) + '&show=500&sortBy=relevance'
                  for x in artigos_emerald.Título]
    print(site_final)
    options = Options()
    options.add_experimental_option('prefs', {
        'download.default_directory': r'C:\Users\B3830082\bib_crawler\download'
    })
    for i, x in enumerate(site_final):
        print(i)
        driver = webdriver.Chrome('chromedriver.exe', chrome_options=options)
        driver.get(x)
        time.sleep(20)
        driver.find_element_by_xpath('/html/body/div[3]/div/div/div/button').click()
        driver.find_element_by_css_selector('#aa-srp-result-list-title-1 > a').click()
        driver.find_element_by_css_selector('#popover-trigger-export-citation-popover > button').click()
        driver.find_element_by_css_selector('#popover-content-export-citation-popover > div > ul > li:nth-child(4) > form > button').click()
        time.sleep(5)
        driver.close()