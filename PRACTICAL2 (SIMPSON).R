#Rashmeet Kaur (14401012022)
#PRACTICAL-3
#SIMPSON'S 1/3 RULE

Simpson_rule_13 <- function(a,b,n) {
  h<- (b-a)/n
  s<- f(a) + f(b)
  for(i in 1:(n-1))
  {
    x_i<- a+i*h
    if(i %% 2==0){
      s<-s+2*f(x_i)
    }
    else{
      s<-s+4*f(x_i)
    }
  }
  result <- (h/3)*s
  
  return (result)
}

Simpson_rule_13(1,2,10)

#Rashmeet Kaur (14401012022)
#PRACTICAL-4
#SIMPSON'S 3/8 RULE

f <- function(x) {
  return(x^2)
}

Simpson_rule_38 <- function(a, b, n) {
  h <- (b - a) / n
  sum <- f(a) + f(b)
  
  for (i in 1:(n-1)) {
    x_i <- a + i * h
    if (i %% 3 == 0) {
      sum <- sum + 2 * f(x_i)
    } else {
      sum <- sum + 3 * f(x_i)
    }
  }
  
  result1 <- (3 * h / 8) * sum
  return(result1)
}

Simpson_rule_38(1, 2, 10)

#Rashmeet Kaur (14401012022)
#Practice Assignment-2   Ques1
f <- function(x) {
  1/x
}

a <- 1
b <- 2
h <- 0.1

x_values <- seq(a, b, by = h)

# Simpson's 1/3 Rule
n <- length(x_values)
simpson_13_sum <- f(x_values[1]) + f(x_values[n]) +
  4 * sum(f(x_values[seq(2, n-1, by = 2)])) + 
  2 * sum(f(x_values[seq(3, n-2, by = 2)]))

simpson_13_result <- (h / 3) * simpson_13_sum

# Simpson's 3/8 Rule
simpson_38_sum <- f(x_values[1]) + f(x_values[n]) +
  3 * sum(f(x_values[seq(2, n-1, by = 3)])) +
  2 * sum(f(x_values[seq(4, n-2, by = 3)]))

simpson_38_result <- (3 * h / 8) * simpson_38_sum

cat("Approximate value using Simpson's 1/3 Rule:", simpson_13_result, "\n")
cat("Approximate value using Simpson's 3/8 Rule:", simpson_38_result, "\n")



#Rashmeet Kaur (14401012022)
#Practice Assignment-2   Ques2
f <- function(x) {
  x^2
}

a <- 0
b <- 3
n <- 6

h <- (b - a) / n

x_values <- seq(a, b, by = h)

# Simpson's 1/3 Rule
n <- length(x_values)
simpson_13_sum <- f(x_values[1]) + f(x_values[n]) +
  4 * sum(f(x_values[seq(2, n-1, by = 2)])) + 
  2 * sum(f(x_values[seq(3, n-2, by = 2)]))

simpson_13_result <- (h / 3) * simpson_13_sum

# Simpson's 3/8 Rule
simpson_38_sum <- f(x_values[1]) + f(x_values[n]) +
  3 * sum(f(x_values[seq(2, n-1, by = 3)])) +
  2 * sum(f(x_values[seq(4, n-2, by = 3)]))

simpson_38_result <- (3 * h / 8) * simpson_38_sum

cat("Approximate area using Simpson's 1/3 Rule:", simpson_13_result, "\n")
cat("Approximate area using Simpson's 3/8 Rule:", simpson_38_result, "\n")

