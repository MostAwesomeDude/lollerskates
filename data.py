#!/usr/bin/env python

from collections import defaultdict

from lxml.html import fromstring
import requests

url = "http://leagueoflegends.wikia.com/wiki/Base_champion_statistics"
request = requests.get(url)
html = request.content
document = fromstring(html)

table = document.xpath("//table")[1]
# In the future, if you ever need the magic incantation to get the table
# headers, try this: [i[0].text if i else i.text for i in table[0]]
headers = [
    "health", "healthg",
    "hregen", "hregeng",
    "mana", "manag",
    "mregen", "mregeng",
    "ad", "adg",
    "as", "asg",
    "armor", "armorg",
    "mr", "mrg",
    "ms",
    "range",
]

champions = []

for row in table[1:]:
    d = {}
    # Champ name rule: Remove spaces and punctuation, and capitalize by hand.
    champ = row[0][0][1][0].text.strip()
    champ = champ.replace(" ", "").replace(".", "").replace("'", "")
    champ = champ[0].upper() + champ[1:]
    d["champ"] = champ
    for label, cell in zip(headers, row[1:]):
        data = cell.text.strip()
        # AS per level is measured in percents.
        if data.endswith("%"):
            data = float(data[:-1]) / 100.0
        else:
            data = float(data)
        d[label] = data
    champions.append(d)

champions.sort(key=lambda d: d["champ"])

url = "http://leagueoflegends.wikia.com/wiki/Template:Items"
request = requests.get(url)
html = request.content
document = fromstring(html)

trs = document.xpath("//table/tr")[1:-3]
l = [x.text for tr in trs for x in tr.xpath("td/span/a/span")]

handle = open("champs.pl", "wb")

handle.write("""/* set filetype=prolog syntax=prolog */
/* This module is autogenerated by champs.py in the top-level directory.
 * To regenerate, be online, then $ python champs.py > champs.pl
 * If you hand-edit this, be sure to explain your reasoning.
 * All of the champion quirks are tracked in champs.py, not in this file.
 * ~ C. */

""")

# Prolog doesn't like discontiguous clauses. Sort before printing.
clauses = defaultdict(list)

for i, champion in enumerate(champions):
    name = champion["champ"].lower()
    clauses["name"].append("champ(%d, %s)." % (i, name))
    for stat in champion:
        if stat == "champ":
            continue
        clauses[stat].append("champ_%s(%d, %f)." % (stat, i, champion[stat]))

for clause in clauses.itervalues():
    for datum in clause:
        handle.write(datum)
        handle.write("\n")
    handle.write("\n")

handle.write("champ_max(%d).\n\n" % (len(champions) - 1))