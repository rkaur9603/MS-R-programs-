#Rashmeet Kaur (14401012022)
#PRACTICAL-1
#Trapezoidal Rule
f<-function(x){
  return(x*sin(x))
}
trapezoid<- function(f,a,b)
{
  if(is.function(f)==FALSE)
  {
    stop('f must be a function with one parameter')
  }
  h<- b-a
  approx <- (h/2) * (f(a) + f(b))
  return (approx)
}
trapezoid (f, 0, pi/4)

#Rashmeet Kaur (14401012022)
#PRACTICAL-2
#COMPOSITE TRAPEZOID RULE

f<-function(x){
  return (x*sin(x))
}
composite.trapezoid= function(f,a,b,n){
  if(is.function(f)==FALSE){
    stop('f must be a function with one parameter')
  }
  h= (b-a)/n
  j= 1:(n-1)
  xj= a+j*h
  approximate= (h/2) * (f(a)+2*sum(f(xj))+f(b))
  return (approximate)
}

composite.trapezoid(f,1,2,10)


#Rashmeet Kaur (14401012022)
#Practice Assigment-1    Ques1
f <- function(x) {
  1/x
}

a <- 1
b <- 2
h <- 0.1

x_values <- seq(a, b, by = h)

n <- length(x_values)
trapezoidal_sum <- f(x_values[1]) + f(x_values[n]) + 2 * sum(f(x_values[2:(n-1)]))
result <- (h / 2) * trapezoidal_sum

cat("Approximate value of the integral using the Trapezoidal Rule:", result, "\n")


#Rashmeet Kaur (14401012022)
#Practice Assignment-1   Ques2
f <- function(x) {
  x^2
}

a <- 0
b <- 3
n <- 6

h <- (b - a) / n

x_values <- seq(a, b, by = h)

n <- length(x_values)
trapezoidal_sum <- f(x_values[1]) + f(x_values[n]) + 2 * sum(f(x_values[2:(n-1)]))
result <- (h / 2) * trapezoidal_sum

cat("Approximate area using the Trapezoidal Rule:", result, "\n")

