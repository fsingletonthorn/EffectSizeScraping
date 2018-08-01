import csv
import requests
import xml.etree.ElementTree as ET
from pathlib import Path
import string

# this is built from https://docs.python.org/3/library/xml.etree.elementtree.html

# saving folder of papers to extract
example_papers = 'C:/Users/fsingletonthorn/Documents/PhD/Effect size scraping paper/EarlyTestMaterials/ExamplePapers/'

# saving tree 
tree =   ET.parse(Path(example_papers + '4547492.xml'))

# saving root
root = tree.getroot()

# to print all children of root
for child in root:
    print(child.tag, child.attrib)

# to print full text:
print(ET.tostring(root, encoding='utf8').decode('utf8'))


for neighbor in root.findall('p'):
    print(neighbor.attrib)



rank = neighbor.find('rank').text
    name = neighbor.get('name')
    print(name, rank)


# to save without tags 
notags = ET.tostring(tree.getroot(), encoding='utf-8',method='text')


