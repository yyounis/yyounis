#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a sample from a given pdf
#' @param PDF should be provided by user
#' @param Lower Bounds and Upper Bounds should additional be provided by user
#' @param Optional: Specific interval may be inputed if available
#' @import
#' @return Specific Rejection Sample from provided PDF with given Lower/Upper Bounds

sampleX <- function(f, lower_bound, upper_bound, interval = 0.01) {
 return(data.frame(x = seq(lower_bound, upper_bound, interval), y = f(x)))
}

gx <- function(x){return(4*x)}
sampleX(f = gx(x), lower_bound = 0, upper_bound = sqrt(.5))




