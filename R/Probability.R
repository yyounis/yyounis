#' @title Probability Related to X or X and Y
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a Probability of an X or an X and Y
#' @param f the PDF provided by the user
#' @param Rep the amount of replication times the user wants, default value is 10000
#' @param variable provided by user to suggest which variable they want to use between "X" or "XY"
#' @param upper_boundx The maximum value for the X Variable
#' @param upper_boundy The maximum value for the Y Variable
#' @param upper_boundJDF The maximum value of the probability distribution
#' @param upper_boundP The upperbound assigned by the user to calculate the probabilities
#' @param lower_boundJDF The minimum value of the probability distribution
#' @param lower_boundx The minimum value for the X Variable
#' @param lower_boundy The minimum value for the Y variable
#' @param lower_boundP The lowerbound assigned by the suer to calculate the probabilities
#' @import dplyr
#' @return A probability related to X or X and Y
#'
#' @export

Prob <- function(f, variable, upper_boundx, upper_boundy, upper_boundJDF, upper_boundP, lower_boundx, lower_boundy, lower_boundJDF, lower_boundP, Rep = 10000) {
if (variable == "X") {
  #User is ONLY looking at the probability of X
  PotentialSample <- runif(1, lower_boundx, upper_boundx)
  Result <- ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)
  BigSample <- data.frame(x = replicate(Rep, {PotentialSample <- runif(1, lower_boundx, upper_boundx);
  ifelse(runif(1, lower_boundx, max_c(lower_boundx, upper_boundx, y = f)) < f(PotentialSample), PotentialSample, NA)}))
  if (exists(upper_boundx)){
    return(mean(BigSample$x < upper_boundx, na.rm = TRUE))

  }
  else {
    return(mean(BigSample$x > lower_boundx, na.rm = TRUE))
  }



} else {
  #User is looking at BOTH X and Y probability combined
  }
}
