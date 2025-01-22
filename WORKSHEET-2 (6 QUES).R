#RASHMEET KAUR(14401012022)
#QUESTION-1

f <- function(x) {
  return(x^3 + 1)
}

# Trapezoidal Rule Function
trapezoidal_rule <- function(a, b, n) {
  h <- (b - a) / n  # Calculate step size
  x <- seq(a, b, length.out = n + 1)  
  y <- f(x)  # Calculate f(x) values
  
  # Apply the trapezoidal rule formula
  area <- h * (0.5 * y[1] + sum(y[2:n]) + 0.5 * y[n + 1])
  
  return(area)
}

a <- 0  # Lower limit
b <- 4  # Upper limit
n <- 4  # Number of intervals

# Calculate the area
area <- trapezoidal_rule(a, b, n)
cat("The area under the curve is:", area, "\n")

#RASHMEET KAUR(14401012022)
#QUESTION-2
f <- function(x, y) {
  return(x * y)
}

# Runge-Kutta 4th Order Method
runge_kutta_4th <- function(x0, y0, x, h) {
  n <- (x - x0) / h 
  for (i in 1:n) {
    k1 <- h * f(x0, y0)
    k2 <- h * f(x0 + 0.5 * h, y0 + 0.5 * k1)
    k3 <- h * f(x0 + 0.5 * h, y0 + 0.5 * k2)
    k4 <- h * f(x0 + h, y0 + k3)

    y0 <- y0 + (1 / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    x0 <- x0 + h
  }
  
  return(y0)
}

x0 <- 1     # Initial value of x
y0 <- 2     # Initial value of y, y(1) = 2
x <- 1.2    # The value of x at which y(x) is to be found
h <- 0.1    # Step size

# Calculate the value of y at x = 1.2 using Runge-Kutta method
result <- runge_kutta_4th(x0, y0, x, h)
cat("The value of y at x =", x, "is:", result, "\n")

#RASHMEET KAUR(14401012022)
#QUESTION-3
f <- function(x, y) {
  return(log(x + y))
}

# Euler's Method function
euler_method <- function(x0, y0, h, steps) {
  for (i in 1:steps) {
    y0 <- y0 + h * f(x0, y0)
    x0 <- x0 + h
  }
  return(y0)
}

x0 <- 0     # Initial value of x
y0 <- 2     # Initial value of y, y(0) = 2
h <- 0.2    # Step size
steps1 <- (1.2 - x0) / h  # Steps to reach x = 1.2
steps2 <- (1.4 - 1.2) / h # Steps to reach x = 1.4 from x = 1.2

# Calculate y at x = 1.2
y_at_1_2 <- euler_method(x0, y0, h, steps1)
cat("The value of y at x = 1.2 is:", y_at_1_2, "\n")

# Calculate y at x = 1.4
y_at_1_4 <- euler_method(1.2, y_at_1_2, h, steps2)
cat("The value of y at x = 1.4 is:", y_at_1_4, "\n")

#RASHMEET KAUR(14401012022)
#QUESTION-4
# Define the function f(x) = log(x) * cos(x)
f = function(x) {
  return(log(x) * cos(x))
}

# Simpson's 3/8 rule function
Simpson_rule_38 = function(a, b, h) {
  n = (b - a) / h  # Calculate the number of intervals
  sum = f(a) + f(b)
  
  for (i in 1:(n - 1)) {
    x_i = a + i * h
    if (i %% 3 == 0) {
      sum = sum + 2 * f(x_i)
    } else {
      sum = sum + 3 * f(x_i)
    }
  }
  
  result = (3 * h / 8) * sum
  return(result)
}

# Parameters
a = 0
b = 1
h = 0.2

# Calculate the integral
result = Simpson_rule_38(a, b, h)
print(result)

#RASHMEET KAUR(14401012022)
#QUESTION-5
# Function to calculate the integrand
integrand <- function(x) {
  exp(-x) * sin(x)
}

# Set the sample size and integration limits
n <- 1000
a <- 0
b <- 2 * pi

# Generate random numbers uniformly distributed between a and b
x <- runif(n, a, b)

# Calculate the function values at the random points
fx <- integrand(x)

# Estimate the integral using Monte Carlo integration
integral_estimate <- (b - a) * mean(fx)

# Print the result
cat("Estimated integral:", integral_estimate, "\n")

#RASHMEET KAUR(14401012022)
#QUESTION-6
# Set the parameters for the linear congruential method
x0 <- 123457
a <- 7^5
c <- 0
m <- 2^31

# Initialize a vector to store the random numbers
random_numbers <- vector(length = 10)

# Generate the first random number
random_numbers[1] <- x0

# Generate the remaining 9 random numbers using the linear congruential method
for (i in 2:10) {
  random_numbers[i] <- (a * random_numbers[i - 1] + c) %% m
}

# Print the generated random numbers
print(random_numbers)