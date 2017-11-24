#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a sample from a given pdf
#' @param f should be provided by user
#' @param lower_bound provided by user for closed specific sample
#' @param upper_bound provided by user for closed specific sample
#' @param interval Default value provided but can be altered if necessary
#' @import dplyr
#' @return Specific Rejection Sample from provided PDF with given Lower/Upper Bounds
#'
#' @export

#Function Provides 1 Sample from PDF
sampleX <- function(f, lower_bound, upper_bound, interval = 0.01) {
Sample1 <- mutate(data.frame(x = seq(lower_bound, upper_bound, interval), y = f(x)))
return(Sample1)
}

#Function Providing "C" Value for Rejection Sampling
max_c <- function(lower_bound, upper_bound, y = gx)
{X <- seq(lower_bound, upper_bound, .001)
Y <- gx(X)
C <- max(Y) + 10
return(C)}

#Function to check if sample falls within range
ApproveReject <- function(lower_bound, upper_bound, y = gx){
PotentialSample <- runif(1, lower_bound, upper_bound)
Result <- ifelse(runif(1, 0, max_c(lower_bound, upper_bound, y = gx)) < gx(PotentialSample), PotentialSample, NA)
return(Result)}


#---------------------------------------------------------------------

#Specific Example
gx <- function(x){return(4*x)}
SpecificSample <- mutate(data.frame(x = seq(0, sqrt(.5), .01)), y = gx(x))

max_c(0, sqrt(.5), y = 4*x)

ApproveReject(0, sqrt(.5), y = gx(x))


library (dplyr)
install.packages("dplyr")




