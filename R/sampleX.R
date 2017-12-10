#' @title SamplePDF
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a sample from a given pdf
#' @param f should be provided by user
#' @param lower_bound provided by user for closed specific sample
#' @param upper_bound provided by user for closed specific sample
#' @param interval Default value provided but can be altered if necessary
#' @param Rep the amount of replication times the user wants, default value is 10000
#' @examples gx <- function(x){return(4*x)}
#' SpecificSample <- mutate(data.frame(x = seq(0, sqrt(.5), .01)), y = gx(x))
#' max_c(0, sqrt(.5), y = 4*x)
#' ApproveReject(0, sqrt(.5), y = gx(x))
#' head(Replication(0, sqrt(.5), y = gx(x)))
#' @import dplyr
#' @return BigSample data frame for provided PDF
#'
#'

#Function Provides 1 Sample from PDF
sampleX <- function(f, lower_bound, upper_bound, interval = 0.01) {
Sample1 <- mutate(data.frame(x = seq(lower_bound, upper_bound, interval), y = f(x)))
return(Sample1)
}

#Function Providing "C" Value for Rejection Sampling
#' @export
max_c <- function(lower_bound, upper_bound, y = gx)
{X <- seq(lower_bound, upper_bound, .001)
Y <- gx(X)
C <- max(Y) + 10
return(C)}

#Function to check if sample falls within range
#' @export
ApproveReject <- function(lower_bound, upper_bound, y = gx){
PotentialSample <- runif(1, lower_bound, upper_bound)
Result <- ifelse(runif(1, lower_bound, max_c(lower_bound, upper_bound, y = gx)) < gx(PotentialSample), PotentialSample, NA)
return(Result)}

#' @export
Replication <- function(lower_bound, upper_bound, y = gx, Rep = 10000){
PotentialSample <- runif(1, lower_bound, upper_bound)
Result <- ifelse(runif(1, lower_bound, max_c(lower_bound, upper_bound, y = gx)) < gx(PotentialSample), PotentialSample, NA)
BigSample <- data.frame(x = replicate(Rep, {PotentialSample <- runif(1, lower_bound, upper_bound);
ifelse(runif(1, lower_bound, max_c(lower_bound, upper_bound, y = gx)) < gx(PotentialSample), PotentialSample, NA)}))
return(BigSample)}


