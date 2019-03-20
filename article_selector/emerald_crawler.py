import pandas as pd
import requests
import re
import io
from bs4 import BeautifulSoup


def transforma_nome(x):
    x = x.replace(',', '').replace(':', '')
    return x.replace(' ', '+')

if __name__ == '__main__':

    site_base = 'https://www.emeraldinsight.com/'
    artigos = pd.read_excel(r'..\Downloads\Artigos relevantes sobre Qualidade percebida.xlsx')
    artigos_emerald = artigos[artigos['Base'] == 'Emerald']
    site_final = [site_base + 'action/doSearch?startPage=0&AllField=' + transforma_nome(x) + '&content=articlesChapters'
                  for x in artigos_emerald.Título]
    print(site_final)


    for art in site_final:
        myheader = {'content-type': 'application/x-www-form-urlencoded',
                    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36',
                    'referer': 'https://www.emeraldinsight.com/action/showCitFormats?doi=10.1108%2F14636690210453406'}



        s = requests.Session()

        s.headers = myheader
        site = s.get(art)
        soup = BeautifulSoup(site.content)
        div_content = soup.find_all('div', class_='search-result-col2')
        ref = [x.find('a', class_='ref nowrap')['href'] for x in div_content]
        site_cit = site_base + 'action/showCitFormats?doi=' + ref[0].split('/')[-2] + '%2F' + ref[0].split('/')[-1]
        session_cit = s.get(site_cit)

        cit_dict = {'doi': ref[0].split('/')[-2] + '/' + ref[0].split('/')[-1],
                    'downloadFileName': 'emerald_info4_50',
                    'include': 'abs',
                    'format': 'bibtex',
                    'direct': '',
                    'submit': 'Download article citation data'}

        post = s.post('https://www.emeraldinsight.com/action/downloadCitation', data=cit_dict)

        with io.open(r'download\\' + ref[0].split('/')[-1].replace('.', '') + '.bib', 'w', encoding='utf-8') as f:
            f.write(post.content.decode('utf8'))
        print(post.content)
