# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:

#'Ctrl + Shift + T'

sampleX <- function(f, lower_bound, upper_bound, interval = 0.01) {
 return(data.frame(x = seq(lower_bound, upper_bound, interval), y = f(x)))
}

gx <- function(x){return(4*x)}
sampleX(f = gx(x), lower_bound = 0, upper_bound = sqrt(.5))




