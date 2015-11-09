# encapsulate into a function inputting the factorial number
# reference http://stackoverflow.com/questions/15014719/factorial-for-loop
fact <- function(n){
  f <- 1
  # i is the iteration, n is the top range of vector
  for(i in 1:n)
  {
    f <- f * i
  }
  return(f)
}

fact(3)
fact(4)
fact(5)
fact(12)

ncr <- function(n, r){
  c <- 0
  {
    # formula for n choose r
    c <- fact(n) / (fact(r) * fact(n - r))
  }
  return(c)
}

ncr(5, 1)
ncr(5, 2)
ncr(5, 3)
ncr(5, 4)
ncr(5, 5)

get_mean <- function(x){
  running_sum <- 0
  n <- length(x)
  for(i in 1:n)
  {
    running_sum <- running_sum + x[i]
  }
  m <- running_sum / n
  return(m)
}

x <- c(1, 3, 15)
get_mean(x)
x <- c(1, 11, 15)
get_mean(x)
x <- (1:10)
get_mean(x)

get_median <- function(x){
  n <- length(x)
  if(n %% 2 != 0){
    m = x[n / 2 + 1]
  }
  else {
    m = (x[n / 2] + x[(n / 2) + 1]) / 2
  }
  return(m)
}

x <- c(1, 3, 15)
get_median(x)
x <- c(1, 11, 15)
get_median(x)
x <- (1:10)
get_median(x)

run.mean <- function(x, func = get_mean){
  do.call(func, args = list(x))
}

run.mean(x)

run.median <- function(x, func = get_median){
  do.call(func, args = list(x))
}

run.median(x)
