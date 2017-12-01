#' @title SampleJDF
#' @author Youssef Younis  <email: youssef.younis@slu.edu>
#' @description This file aims to produce a sample from a given Joint Density Function
#' @param Joint Density Function should be provided by user
#' @param lower_boundx provided by user to calculate minimum boundary of "X"
#' @param lower_boundy provided by user to calculate minimum boundary of "Y"
#' @param lower_boundJDF provided by user to calculate minimum boundary of "JDF"
#' @param upper_boundx provided by user to calculate maximum boundary of "X"
#' @param upper_boundy provided by user to calculate maximum boundary of "Y"
#' @param upper_boundJDF provided by user to calculate maximum boundary of "JDF"
#' @param Rep the amount of replication times the user wants, default value is 10000
#' @examples gx = function(x,y){
#' return (x+y)}
#' SampleJDF(0, 0, 1, 1, 0, 2, gx)
#' @import dplyr
#' @return Specific Rejection Sample from provided JDF
#'
#' @export
SampleJDF <- function(lower_boundx, lower_boundy, upper_boundx, upper_boundy, lower_boundJDF, upper_boundJDF, f = f, Rep = 10000){

SamData <- replicate(Rep, {pSx <- runif(1, lower_boundx, upper_boundx); pSy <- runif(1, lower_boundy, upper_boundy); if(runif(1,lower_boundJDF, upper_boundJDF) < f(x = pSx, y = pSy)) {c(x = pSx, y = pSy)} else c(x = NA,y = NA)})
SamData <- data.frame(t(SamData))
return(SamData)
}
