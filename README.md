[![codecov.io](https://codecov.io/gh/fsingletonthorn/EffectSizeScraping/branch/master/graph/badge.svg)](https://codecov.io/gh/fsingletonthorn/EffectSizeScraping?branch=master)

# EffectSizeScraping
This is the project page for a tool that will be used to scrape effect size, test statistics, and reporting practices from papers indexed in the PMC open access subset.

A schematic of the underlying workflow is (this will be updated with clearer diagrams at some point): 
scrapePMC(pmcCall, ftpCall) {
	pullPMC(pmcCall) # {pulls PMC text, dates, metadata 
	  if(there is no text) {
		extractPDF(on any pdfs found in ftpCall) %>%
		# If the text is not extracted, it downloads the full pubmed from the ftp server, extracts the .tar file, looks for .pdf files, and then runs processPDF() on the pdf file - which extracts all text with associated section headings. The metadata is always on the PMC call, but this lets us scrape any papers which have not been converted to full XML text on the PMC server.
	  }
	}
	processText(pulledPMC$text) { # This function runs extract test stats on each text chunk
	  lapply(pulledPMC$text, extractTestStats) {
		# Extract test stats	extracts T, F, correlation coefficients, chi square tests, cohen's ds, eta (partial, generalised), hazard ratios, odds ratios using regex patterns. P values are extracted with each of these if reported. Test statistics without degrees of freedom are currently not extracted.
	  }
	}
	apply(searchFunctions(pulledPMC$text) {
	# The search functions are also run on each of the text chunks 
	# This searches for CIs, for sample sizes, and later for power analysis
	}
}

Important notes:

Confidence interval detection accepts either a fully written out "confidence interval" as a hit, or "[2 digits]% CI", or "CI" followed by just two numbers in parentheses "(" or square "[" brackets (ignores whitespace). 
