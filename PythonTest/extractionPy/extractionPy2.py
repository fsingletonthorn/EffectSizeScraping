import os
import pubmed_parser as pp
from pyspark.sql import Row

# Requires pubmed_parser from https://github.com/titipata/pubmed_parser

#
# Can use parse all below to index all files in a group - will be useful later
#  path_all = pp.list_xml_path('C:/Users/fsingletonthorn/Documents/PhD/Effect size scraping paper/EarlyTestMaterials/ExamplePapers/PMC5794850.nxml')
path_all ='C:/Users/fsingletonthorn/Documents/PhD/Effect size scraping paper/EarlyTestMaterials/ExamplePapers/PMC5794850.nxml'
parse_results_rdd = pubmed_oa_rdd.map(lambda x: Row(file_name=os.path.basename(x), **pp.parse_pubmed_xml(x)))
pubmed_oa_df = parse_results_rdd.toDF()
pubmed_oa_df_sel = pubmed_oa_df[['full_title', 'abstract', 'doi', 
                                 'file_name', 'pmc', 'pmid', 
                                 'publication_year', 'publisher_id', 'journal', 'subjects']]
pubmed_oa_df_sel.write.parquet('pubmed_oa.parquet')



paras = pp.parse_pubmed_paragraph('C:/Users/fsingletonthorn/Documents/PhD/Effect size scraping paper/EarlyTestMaterials/ExamplePapers/PMC5963120.nxml')
paras[6]

