# table extraction 
table_to_df <- function(x){

  }

# extracting tables
table_node <- xml_find_all(paper, '//body//sec//table-wrap')
# Only doing the rest if tables were detected
if(length(table_node)>0) {
  # extracting table labels (e.g., "Table 1")
  tableLab_node <- xml_find_all(paper, '//body//sec//table-wrap//label')
  # extracting table captions
  if(length(xml_find_all(paper, '//body//sec//table-wrap//caption/p'))> 0) {
    tableCaption_node <- xml_find_all(paper, '//body//sec//table-wrap//caption/p')
  } else {
    tableCaption_node <- xml_find_all(paper, '//body//sec//table-wrap//caption/title')
  }
  # extracting table content
  tableContent_node <- xml_find_all(paper, '//body//sec//table-wrap//table')
  
  nTables <- length(tableContent_node)
  # initialising a list to store data 
  tables <- vector("list", nTables) 
  
  for(i in length(table_node)) {
  tableLocation <- xml_path(xml_find_all(paper, '//body//sec//table-wrap//table')[i])
  tableHeaders <- xml_text(xml_find_all(paper, paste0(tableLocation, "//th")))
  nRows <- length(xml_find_all(paper, paste0(tableLocation, "//tr")))
  nCols <- length(tableHeaders)
  table <- matrix(NA, nCols, nrow = nRows)
  for(j in 1:nRows-1) {
     table[j+1,]<- xml_text(xml_find_all(paper, paste0(tableLocation, "//tr", "[",j,"]/","td")))[1:nCols]
  }
  table[1,] <- tableHeaders
  tables[i] <- table
  }
  
  # counting number of tables
  # extracting table headers  
  tables[[1]]
  
  tableHeaders <- xml_text(xml_find_all(tableContent_node, '//table-wrap//table//tr'))
  # extracting table content
  tableContent <- xml_text(xml_find_all(paper, '//body//sec//table-wrap//table//tr/td'))

  
  
  
  # constructing each table
  
  
xml_structure(tableContent_node[2])

}
  
  if table_tree is not None:
    table_xml = etree.tostring(table_tree)
  columns, row_values = table_to_df(table_xml)
  if row_values is not None:
    table_dict = {'pmid': pmid,
      'pmc': pmc,
      'label': label,
      'caption': caption,
      'table_columns': columns,
      'table_values': row_values}
  if return_xml:
    table_dict['table_xml'] = table_xml
  table_dicts.append(table_dict)
  if len(table_dicts) >= 1:
    return table_dicts
  else:
    return None











}  


def parse_pubmed_table(path, return_xml=True):
  """
Parse table from given Pubmed Open-Access XML file
"""
tree = read_xml(path)

# parse table
tables = tree.xpath('//body//sec//table-wrap')
table_dicts = list()
for table in tables:
  if table.find('label') is not None:
  label = unidecode(table.find('label').text or '')
else:
  label = ''



# table caption
if table.find('caption/p') is not None:
  caption_node = table.find('caption/p')
elif table.find('caption/title') is not None:
  caption_node = table.find('caption/title')
else:
  caption_node = None
if caption_node is not None:
  caption = unidecode(stringify_children(caption_node).strip())
else:
  caption = ''

# table content
if table.find('table') is not None:
  table_tree = table.find('table')
elif table.find('alternatives/table') is not None:
  table_tree = table.find('alternatives/table')
else:
  table_tree = None

if table_tree is not None:
  table_xml = etree.tostring(table_tree)
columns, row_values = table_to_df(table_xml)
if row_values is not None:
  table_dict = {'pmid': pmid,
    'pmc': pmc,
    'label': label,
    'caption': caption,
    'table_columns': columns,
    'table_values': row_values}
if return_xml:
  table_dict['table_xml'] = table_xml
table_dicts.append(table_dict)
if len(table_dicts) >= 1:
  return table_dicts
else:
  return None





def table_to_df(table_text):
  """
Function to transform plain xml text to list of row values and
columns
"""
table_tree = etree.fromstring(table_text)
columns = []
for tr in table_tree.xpath('thead/tr'):
  for c in tr.getchildren():
  columns.append(unidecode(stringify_children(c)))

row_values = []
len_rows = []
for tr in table_tree.findall('tbody/tr'):
  es = tr.xpath('td')
row_value = [unidecode(stringify_children(e)) for e in es]
len_rows.append(len(es))
row_values.append(row_value)
if len(len_rows) >= 1:
  len_row = max(set(len_rows), key=len_rows.count)
row_values = [r for r in row_values if len(r) == len_row] # remove row with different length
return columns, row_values
else:
  return None, None