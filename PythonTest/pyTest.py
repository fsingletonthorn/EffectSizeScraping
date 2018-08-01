import csv
import requests
import xml.etree.ElementTree as ET
from pathlib import Path

example_papers = 'C:/Users/fsingletonthorn/Documents/PhD/Effect size scraping paper/EarlyTestMaterials/ExamplePapers/'

tree =   ET.parse(Path(example_papers + '4547492.xml'))

root = tree.getroot()

root.findall("./GetRecord")

