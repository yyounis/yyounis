#' @title Probability Related to X or X and Y
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a Probability of an X or an X and Y
#' @param f the PDF provided by the user
#' @param Rep the amount of replication times the user wants, default value is 10000
#' @param variable provided by user to suggest which variable they want to use between "X" or "XY"
#' @param upper_boundx The maximum value for the X Variable
#' @param upper_boundy The maximum value for the Y Variable
#' @param upper_boundF The maximum value of the probability distribution (Either a PDF or a JDF)
#' @param upper_boundP The upperbound assigned by the user to calculate the probabilities
#' @param lower_boundF The minimum value of the probability distribution (Either a PDF or a JDF)
#' @param lower_boundx The minimum value for the X Variable
#' @param lower_boundy The minimum value for the Y variable
#' @param lower_boundP The lowerbound assigned by the suer to calculate the probabilities
#' @import dplyr
#' @return A probability related to X or X and Y
#' @examples \dontrun {
#' Example1
#' f = function(x) {return(4*x)}
#' Prob(f, variable = "X", upper_boundx = .5, upper_boundP = .3, lower_boundx = 0, lower_boundP = 0, Rep = 10000)
#'
#' ExampleSpeegle
#' f = function(x) {return(1/2/pi *(sin(x) + 1))}
#' Prob(f, variable = "X", upper_boundx = 2*pi, upper_boundP = 2*pi, lower_boundP = pi, lower_boundx = 0, Rep = 10000)
#'
#' Example3
#' f = function(x,y) {return(x+y)}
#' Prob(f, variable = "XY", upper_boundx = 1, upper_boundy = 1, upper_boundF = 2, upper_boundP = 1, lower_boundx = 0, lower_boundy = 0, lower_boundF = 0, Rep = 10000)
#' }
#'
#' @export

Prob <- function(f, variable, upper_boundx, upper_boundy, upper_boundF, upper_boundP, lower_boundx, lower_boundy, lower_boundF, lower_boundP, Rep = 10000) {
  if (variable == "X") {
    #User is ONLY looking at the probability of X
    max_c <- function(lower_bound, upper_bound, y = f){
      X <- seq(lower_bound, upper_bound, .001)
      Y <- f(X)
      C <- max(Y) + 10
      return(C)}
    PotentialSample <- runif(1, lower_boundx, upper_boundx)
    Result <- ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)
    BigSample <- data.frame(x = replicate(Rep, {PotentialSample <- runif(1, lower_boundx, upper_boundx);
    ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)}))
    if(hasArg(upper_boundP)){
      return(mean(BigSample$x < upper_boundP, na.rm = TRUE))
    }else{
      return(mean(BigSample$x > lower_boundP, na.rm = TRUE))}
  } else {
    #User is looking at BOTH X and Y probability combined
    SamData <- replicate(Rep, {pSx <- runif(1, lower_boundx, upper_boundx); pSy <- runif(1, lower_boundy, upper_boundy); if(runif(1,lower_boundF, upper_boundF) < f(x = pSx, y = pSy)) {c(x = pSx, y = pSy)} else c(x = NA,y = NA)})
    SamData <- data.frame(t(SamData))
    SamData <- SamData[complete.cases(SamData),]
    if (hasArg(upper_boundP) & hasArg(lower_boundP)){
      mean((SamData$x + SamData$y < upper_boundP & SamData$x + SamData$y > lower_boundP), na.rm = TRUE)
    }
    else if (hasArg(upper_boundP) & !hasArg(lower_boundP)){
      mean(SamData$x + SamData$y < upper_boundP, na.rm = TRUE)
    }
    else if(hasArg(lower_boundP) & !hasArg(upper_boundP)){
      mean(SamData$x + SamData$y > lower_boundP, na.rm = TRUE)}
  }
}


#EXAMPLE
f = function(x) {return(4*x)}
Prob(f, variable = "X", upper_boundx = .5, upper_boundP = .3, lower_boundx = 0, lower_boundP = 0, Rep = 10000)

#ExampleSpeegle
f = function(x) {return(1/2/pi *(sin(x) + 1))}
Prob(f, variable = "X", upper_boundx = 2*pi, lower_boundP = pi, lower_boundx = 0, Rep = 10000)

#Example3
f = function(x,y) {return(x+y)}
Prob(f, variable = "XY", upper_boundx = 1, upper_boundy = 1, upper_boundF = 2, upper_boundP = 1, lower_boundx = 0, lower_boundy = 0, lower_boundP = 0, lower_boundF = 0, Rep = 10000)
