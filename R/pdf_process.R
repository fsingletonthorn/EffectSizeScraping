# pdf extract
library(pdftools)
library(stringr)
# Split pdf by column 

test <- pdftools::pdf_data("data_/examplePaper.pdf")
# test <- pdftools::pdf_text("data_/examplePaper.pdf")

# "C:\Program Files (x86)\xpdf-tools-win-4.00\xpdf-tools-win-4.00\bin64\pdftotext.exe" C:\Users\fsingletonthorn\Documents\PhD\EffectSizeScrapingPaper\data_\examplePaper.pdf -layout  C:\Users\fsingletonthorn\Documents\PhD\EffectSizeScrapingPaper\data_\examplePaper.txt

page <- 1
locations <- data.frame(test[[page]]$x, test[[page]]$y)
columnTotals <- data.frame(table(test[[page]]$x), stringsAsFactors = F)
orderedCol <- columnTotals[order(columnTotals$Freq,  decreasing = T),]
hclust_avg <- hclust(distances)
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 3)

distances <- dist(locations, method = "maximum")
columnXs <- as.numeric(as.character(orderedCol[1:5, 1]))
rect.hclust(hclust_avg , k = 3, border = 2:6)

# Selecting the line closest to the middle of the page -- Later build in that if it is more than 25% we will not use 
splitVal <- columnXs[which.min(abs(columnXs - (max(test[[page]]$x) + min(test[[page]]$x)  )/2))]

# plot
plot(test[[page]]$x, test[[page]]$y, col = cut_avg)
abline(v = splitVal - 1)
abline(v = (max(test[[page]]$x) + min(test[[page]]$x)  )/2, col = "blue")

# Check that the break is near the middle 

# Check that the break goes near the top to near the bottom

# remove duplicates 

