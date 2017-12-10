## ------------------------------------------------------------------------
set.seed(123)
library(dplyr)
gx <- function(x){return(4*x)}
SpecificSample <- mutate(data.frame( x = seq(0, sqrt(.5), .01)), y = gx(x))
max_c(0, sqrt(.5), y = 4*x)

## ------------------------------------------------------------------------
ApproveReject(0, sqrt(.5), y = gx(x))

## ------------------------------------------------------------------------
head(Replication(0, sqrt(.5), y = gx(x))) %>% rename("Random Sample" = "x")

## ------------------------------------------------------------------------
gx = function(x,y){
return (x+y)}
head(SampleJDF(0, 0, 1, 1, 0, 2, gx)) %>% rename("Random Sample X-Coordinate" = "x", "Random Sample Y-Coordinate" = "y")

## ------------------------------------------------------------------------
f = function(x) {return(2*x)}
ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 1)

## ------------------------------------------------------------------------
f = function(x) {return(1/2/pi *(sin(x) + 1))}
ExpectedValue(f, "X", lower_boundx = 0, upper_boundx = 2* pi)

## ------------------------------------------------------------------------
f = function(x,y) {return(x+y)}
ExpectedValue(f, variable = "XY", upper_boundx = 1, upper_boundy = 1, upper_boundF = 2, lower_boundx = 0, lower_boundy = 0, lower_boundF = 0)

