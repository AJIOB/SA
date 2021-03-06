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

img_convertion <- function(image_num, img_path) {
  img_color <- load.image(img_path)
  customShow(img_color, sprintf("Image %d: color", image_num))
  img_gray <- grayscale(img_color)
  customShow(img_gray, sprintf("Image %d: grayscale", image_num))
  
  # Histogram
  img_gray_scaled <- img_gray * jpegValueScale ^ 2
  histogram_res <- 
    hist(img_gray_scaled, main=sprintf("Grayscale histogram of image %d", image_num), 
         xlab = "", xlim = c(0, jpegValueScale),
         breaks = seq(0, jpegValueScale + histogramStep, by=histogramStep))
  
  list(gray = img_gray_scaled, hist = histogram_res)
}

main_calc <- function(image_num, histogram_res) {
  #hist_x <- histogram_res$breaks
  hist_y <- histogram_res$counts
  
  message("Image ", image_num, " mean = ", mean(hist_y))
  message("Image ", image_num, " square derivation = ", sd(hist_y))
  message("Image ", image_num, " median = ", median(hist_y))
  hist_y.t <- table(hist_y)
  message("Image ", image_num, " mode = ", sort(unique(hist_y))[which.max(hist_y.t)])
}

message("Made by AJIOB, 2018")

conv1 <- img_convertion(1, img1_path)
main_calc(1, conv1$hist)
conv2 <- img_convertion(2, img2_path)
main_calc(2, conv2$hist)

message("Histogram correlation = ", cor(conv1$hist$counts, conv2$hist$counts))
message("Grayscale image correlation = ", cor(conv1$gray, conv2$gray))

chi_test <- function(dots) {
  interval <- dots$hist$breaks
  len <- length(interval)
  interval[1] <- (-Inf)
  interval[len] <- (+Inf)
  p_for_dots <- pnorm(interval, mean=mean(dots$gray), sd=sd(dots$gray))
  p_for_dots <- (p_for_dots[2:len] - p_for_dots[1:(len - 1)])
  
  chisq.test(dots$hist$counts, p = p_for_dots)
}

chi1 <- chi_test(conv1)
message("Chi square for image 1: ", as.numeric(chi1$statistic), "; p = ", chi1$p.value)
chi2 <- chi_test(conv2)
message("Chi square for image 2: ", as.numeric(chi2$statistic), "; p = ", chi2$p.value)
