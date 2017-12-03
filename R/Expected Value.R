#' @title ExpectedValue
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce an Expected Value of a f(x) or f(x,y) given a function f
#' @param Function g should be provided for
#' @param
#' @param
#' @import dplyr
#' @return An Expected Value of a g(x) or g(x,y)
#'
#' @export


ExpectedValue <- function(f, variable, upper_boundx, upper_boundy, upper_boundF, lower_boundx, lower_boundy, lower_boundF, Rep = 10000){
  if(variable == "X") {
    #User is ONLY looking for the Expected Value of "X"
    max_c <- function(lower_bound, upper_bound, y = f)
    {X <- seq(lower_bound, upper_bound, .001)
    Y <- f(X)
    C <- max(Y) + 10
    return(C)}
    PotentialSample <- runif(1, lower_boundx, upper_boundx)
    Result <- ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)
    BigSample <- data.frame(x = replicate(Rep, {PotentialSample <- runif(1, lower_boundx, upper_boundx);
    ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)}))
    return(mean(BigSample$x, na.rm = TRUE))}
   else(variable) }



#Example
f = function(x) {return(1/2/pi *(sin(x) + 1))}
ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 2* pi)
