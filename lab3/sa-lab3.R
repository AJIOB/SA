#set working directory
setwd("D:/labs/SA/lab3/")

# Install from CRAN
library(imager)

jpegValueScale = 255
histogramStep = 10

#img1_path <- "Bad.jpg"
img1_path <- "Tower_Bridge_640.jpg"
img2_path <- "Zima_640.jpg"
#img1_path <- "Tower_Bridge_4K.jpg"
#img2_path <- "Zima_4K.jpg"

customShow <- function(img, title){
  plot(img, main = title)
}

img1_color <- load.image(img1_path)
customShow(img1_color, "Image 1: color")
img1_gray <- grayscale(img1_color)
customShow(img1_gray, "Image 1: grayscale")

img1_gray_scaled <- img1_gray * jpegValueScale ^ 2
hist(img1_gray_scaled, main="Grayscale histogram of image 1", 
     xlab = "", xlim = c(0, jpegValueScale),
     breaks = seq(0, jpegValueScale + histogramStep, by=histogramStep))
