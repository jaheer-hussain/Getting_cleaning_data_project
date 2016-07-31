add2 <- function(x,y) {
  x+y
}

above <- function(x,n=10){
   sub_v <- x > n
   x[sub_v]
}

column_op <- function(y , removeNA=TRUE
                      
                      ) {
    nc <- ncol(y)  ## number of columns
    means <- numeric(nc) ## initialize
    for (i in 1:nc) {
        means[i] <- mean(y[,i])
       }
    means
}

make.power <- function(n)
{
  pow <- function(x) {n^x}
  pow
}

y<-20
f<- function(x) {
  y <-2
  y^2+g(x)
}
g <- function(x) {
  x*y
}