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

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

