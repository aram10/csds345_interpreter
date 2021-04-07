from os import close
from bs4 import BeautifulSoup

f = open("part3tests.html")
soup = BeautifulSoup(f, 'html.parser')

for i, text in enumerate(soup.find_all('pre')): 
    with open(f'{i}.txt', 'w+') as test_file:
        test_file.write(text.string[1::])

f.close()