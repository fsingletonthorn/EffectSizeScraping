# pdf extract
library(pdftools)
# Split pdf by column 


test <- pdftools::pdf_data("data_/examplePaper.pdf")

page <- 3
locations <- data.frame(test[[page]]$x, test[[page]]$y)
columnTotals <- data.frame(table(test[[page]]$x), stringsAsFactors = F)
orderedCol <- columnTotals[order(columnTotals$Freq,  decreasing = T),]

distances <- dist(locations, method = "euclidian")
hclust_avg <- hclust(distances)
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = sum(hclust_avg$height > mean(hclust_avg$height)*15))

columnXs <- as.numeric(as.character(orderedCol[1:2, 1]))
rect.hclust(hclust_avg , k = 3, border = 2:6)

#  creating binary representation 
# out<-array(0, dim=c(max(test[[page]]$x), max(test[[page]]$y),1,1))

out <- matrix(0, nrow=max(test[[page]]$x), ncol=max(test[[page]]$y))
out[test[[page]]$x, test[[page]]$y] <- 1
A <- out
# converting to a binary representation 

sobel <- function(A) {
  # https://en.wikipedia.org/wiki/Sobel_operator
  
  Gx <- array(c(-1, 0, 1,-2, 0, 2,-1, 0, 1), dim = c(3, 3))
  Gy <- array(c(-1,-2,-1, 0, 0, 0, 1, 2, 1), dim = c(3, 3))
  
  rows <- dim(A)[1]
  columns <- dim(A)[2]
  mag <- matrix(0, rows, columns)
  S1 <- numeric()
  S2 <- numeric()
  
  for (i in 1:(rows - 2)) {
    for (j in 1:(columns - 2)) {
      S1 <- sum(sum(Gx %*%  A[i:(i + 2), j:(j + 2)]))
      S2 <- sum(sum(Gy  %*% A[i:(i + 2), j:(j + 2)]))
      
      mag[i + 1, j + 1] <- sqrt(S1 ^ 2 + S2 ^ 2)
    }
  }
  
  
  threshold <- 4 #  %varies for application [0 255]
  output_image <- ifelse(mag > threshold , mag, 0)
  output_image[output_image == 0] <- 0
return(output_image)
}

 output_image <- sobel(A)
 
 dim(output_image)

outputBinary <- which(output_image >= .9, arr.ind = T)

plot(outputBinary)




# plot(as.data.frame(table(test[[page]]$x)))


# plot

plot(test[[page]]$x, test[[page]]$y, col = cut_avg)

abline(v = columnXs)

# First just cluster by the second place with a massive spike in numbers 

image_blank(width = length(test[[page]]$x), height = length(test[[page]]$y))
  image_draw(locations)




plot(distances)




edges <- edge.detect(matrixImage)

plot(arrayInd(edges, .dim = dim(edges)))

View(test)

processedTest <- processHTML(test)

extractTestStats(processedTest[4])
