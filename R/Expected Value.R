#' @title ExpectedValue
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce an Expected Value of a f(x) or f(x,y) given a function f
#' @param f the PDF provided by the user
#' @param Rep the amount of replication times the user wants, default value is 10000
#' @param variable provided by user to suggest which variable they want to use between "X" or "XY"
#' @param upper_boundx The maximum value for the X Variable
#' @param upper_boundy The maximum value for the Y Variable
#' @param upper_boundF The maximum value of the probability distribution (Either a PDF or a JDF)
#' @param lower_boundF The minimum value of the probability distribution (Either a PDF or a JDF)
#' @param lower_boundx The minimum value for the X Variable
#' @param lower_boundy The minimum value for the Y variable
#' @examples \dontrun {
#'
#' Example1
#' f = function(x) {return(1/2/pi *(sin(x) + 1))}
#' ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 2* pi)
#'
#' Example2
#' f = function(x,y) {return(x+y)}
#' ExpectedValue(f, variable = "XY", upper_boundx = 1, upper_boundy = 1, upper_boundF = 2,
#' lower_boundx = 0, lower_boundy = 0, lower_boundF = 0)}
#'
#' @import dplyr
#' @return An Expected Value of a f(x) or f(x,y)
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
   else{
    #User is looking for Expected Value of "X" & "Y"
     SamData <- replicate(Rep, {pSx <- runif(1, lower_boundx, upper_boundx); pSy <- runif(1, lower_boundy, upper_boundy); if(runif(1,lower_boundF, upper_boundF) < f(x = pSx, y = pSy)) {c(x = pSx, y = pSy)} else c(x = NA,y = NA)})
     SamData <- data.frame(t(SamData))
     return(mean(SamData$x + SamData$y, na.rm = TRUE))
   }}



#Example
f = function(x) {return(1/2/pi *(sin(x) + 1))}
ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 2* pi)

#Example2
f = function(x,y) {return(x+y)}
ExpectedValue(f, variable = "XY", upper_boundx = 1, upper_boundy = 1, upper_boundF = 2, lower_boundx = 0, lower_boundy = 0, lower_boundF = 0)

#ExampleQuestion
f = function(x) {return(2*x)}
ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 1)
