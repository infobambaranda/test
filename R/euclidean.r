#1.1.1
euclidean <- function(a,b){
  while (b != 0){
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}
