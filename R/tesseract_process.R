# Example paper 5 has equals that are read as 5's using readpdf

tesseract <- function(text) {
eng <- tesseract::tesseract("eng")
pngfile <- pdftools::pdf_convert("data_/examplePaper_5.pdf", pages = 3,dpi = 600)
text <- tesseract::ocr(pngfile)

statcheck::statcheck(text)
}