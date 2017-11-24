#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a sample from a given pdf
#' @param f should be provided by user
#' @param lower_bound provided by user for closed specific sample
#' @param upper_bound provided by user for closed specific sample
#' @param interval Default value provided but can be altered if necessary
#' @import
#' @return Specific Rejection Sample from provided PDF with given Lower/Upper Bounds
#'
#' @export

sampleX <- function(f, lower_bound, upper_bound, interval = 0.01) {
Sample1 <- mutate(data.frame(x = seq(lower_bound, upper_bound, interval), y = f(x)))
return(Sample1)
}

X <- seq(lower_bound, upper_bound, .001)
Y <- gx(x)
C <- max(Y) + 10


gx <- function(x){return(4*x)}
SpecificSample <- mutate(data.frame(x = seq(0, sqrt(.5), .01)), y = gx(x))

library (dplyr)
install.packages("dplyr")




