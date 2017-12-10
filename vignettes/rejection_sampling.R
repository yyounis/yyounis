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

