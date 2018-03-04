#set working directory
setwd("D:/labs/SA/lab3/")

message("Code requires `jpeg` package")
library(jpeg)

readJPEG_scale = 256

img1_path <- "Tower_Bridge_640.jpg"
img2_path <- "Zima_640.jpg"
#img1_path <- "Tower_Bridge_4K.jpg"
#img2_path <- "Zima_4K.jpg"

customShow <- function(img, title){
  plot(1:2, type='n', main = title)
  rasterImage(img, 1, 1, 2, 2)
}

img1_color <- readJPEG(img1_path)
customShow(img1_color, "Image 1: color")
