#!/usr/bin/env python
import requests
from bs4 import BeautifulSoup

# Koren names
# url = "https://wikipedia.org/wiki/List_of_Korean_given_names"

# names = set()

# res = requests.get(url)
# soup = BeautifulSoup(res.text,"lxml")
# nameTables = soup.find_all(class_="wikitable")[6:10]

# for table in nameTables:
#     for row in table.find_all("tr"):
#         data = row.find("td")
#         if data is not None:
#             names.add(data.get_text(strip=True))

# with open("data/names/korean.txt", 'w') as f:
#     sortedNames = list(names)
#     namesJoined = ('\n, '.join(map(lambda n: '"' + n + '"', sortedNames)))
#     f.write('[ ' + namesJoined + '\n]')


# Klingon names
# url = "http://klingon.wiki/En/Names"

# names = set()

# res = requests.get(url)
# soup = BeautifulSoup(res.text,"lxml")
# nameTable = soup.find_all(name="table", class_="foswikiTable")[1]

# # for table in nameTables:
# for row in nameTable.find_all("tr"):
#     columns = row.find_all("td")
#     if columns and columns[1] is not None:
#         names.add(columns[1].get_text(strip=True))

# with open("data/names/klingon.txt", 'w') as f:
#     sortedNames = list(names)
#     namesJoined = ('\n, '.join(map(lambda n: '"' + n + '"', sortedNames)))
#     f.write('[ ' + namesJoined + '\n]')


# Afghan names
# url = "https://en.m.wikipedia.org/wiki/Afghan_name"

# names = set()

# res = requests.get(url)
# soup = BeautifulSoup(res.text,"lxml")
# nameTables = soup.find_all(class_="wikitable")[:2]

# for table in nameTables:
#     for row in table.find_all("tr"):
#         columns = row.find_all("td")
#         if columns and columns[1] is not None:
#             names.add(columns[1].get_text(strip=True))

# with open("data/names/afghan.txt", 'w') as f:
#     sortedNames = list(names)
#     namesJoined = ('\n, '.join(map(lambda n: '"' + n + '"', sortedNames)))
#     f.write('[ ' + namesJoined + '\n]')


# Algonquian names
# url = "https://en.wikipedia.org/wiki/List_of_Algonquian_personal_names"

# names = set()

# res = requests.get(url)
# soup = BeautifulSoup(res.text, "lxml")
# nameGroups = soup.find_all(name="ul")[1:19]

# for nameGroup in nameGroups:
#     for nameEl in nameGroup.find_all(name="li"):
#         linkEl = nameEl.find("a")
#         if linkEl is not None:
#             names.add(linkEl.get_text(strip=True))

# with open("data/names/algonquian.txt", "w") as f:
#     sortedNames = list(names)
#     namesJoined = "\n, ".join(map(lambda n: '"' + n + '"', sortedNames))
#     f.write("[ " + namesJoined + "\n]")


# Afghan provinces
url = "https://en.wikipedia.org/wiki/Provinces_of_Afghanistan"

names = set()

res = requests.get(url)
soup = BeautifulSoup(res.text, "lxml")
nameTable = soup.find(class_="wikitable")

for row in nameTable.find_all("tr"):
    data = row.find("td")
    if data is not None:
        names.add(data.get_text(strip=True))

with open("data/names/afghan_provinces.txt", "w") as f:
    sortedNames = list(names)
    namesJoined = "\n, ".join(map(lambda n: '"' + n + '"', sortedNames))
    f.write("[ " + namesJoined + "\n]")
