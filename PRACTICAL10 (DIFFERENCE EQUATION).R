#Rashmeet Kaur(14401012022)
#PRACTICAL-10
#SOLUTION OF DIFFERENCE EQUATION

#SOLUTION OF HOMOGENEOUS DIFFERENCE EQUATION
# Function to solve a second-order homogeneous linear difference equation
solve_difference_equation <- function(p, q, f, initial_values, n_terms) {
  # Extract initial conditions
  x_0 <- initial_values[1]
  x_1 <- initial_values[2]
  
  # Initialize sequence
  x <- numeric(n_terms)
  x[1] <- x_0
  x[2] <- x_1
  
  # Compute the sequence iteratively
  for (n in 3:n_terms) {
    x[n] <- p * x[n - 1] - q * x[n - 2] + f(n)
  }
  return(x)
}
# Parameters for the difference equation
p <- 1   # Coefficient of x_n+1
q <- -2  # Coefficient of x_n
# Define the non-homogeneous function f(n) (change to `function(n) 0` for homogeneous)
f <- function(n) { 0 }  # Homogeneous case
# Example of non-homogeneous: f <- function(n) { n }
initial_values <- c(1, 1)  # Initial conditions x_0 and x_1
n_terms <- 10  # Number of terms to compute
# Solve the difference equation
solution <- solve_difference_equation(p, q, f, initial_values, n_terms)
# Display the solution
cat("The solution is:\n", solution, "\n")
# Plot the solution
plot(0:(n_terms - 1), solution, type = "o", col = "blue", pch = 19,
     xlab = "n", ylab = "x_n", main = "Solution of Difference Equation")

#Rashmeet Kaur(14401012022)
#SOLUTION OF NON-HOMOGENEOUS DIFFERENCE EQUATION
# Function to solve a second-order non homogeneous linear difference equation
solve_difference_equation <- function(p, q, f, initial_values, n_terms) {
  # Extract initial conditions
  x_0 <- initial_values[1]
  x_1 <- initial_values[2]
  
  # Initialize sequence
  x <- numeric(n_terms)
  x[1] <- x_0
  x[2] <- x_1
  
  # Compute the sequence iteratively
  for (n in 3:n_terms) {
    x[n] <- p * x[n - 1] - q * x[n - 2] + f(n - 1)
  }
  return(x)
}

# Parameters for the difference equation
p <- 1   # Coefficient of x_n+1
q <- -2  # Coefficient of x_n

# Define the non-homogeneous function f(n), Example of non-homogeneous:f<-function(n){n}
f <- function(n) { n }  # Non- Homogeneous case

initial_values <- c(1, 1)  # Initial conditions x_0 and x_1
n_terms <- 10  # Number of terms to compute

# Solve the difference equation
solution <- solve_difference_equation(p, q, f, initial_values, n_terms)

# Display the solution
cat("The solution is:\n", solution, "\n")

# Plot the solution
plot(0:(n_terms - 1), solution, type = "o", col = "blue", pch = 19,
     xlab = "n", ylab = "x_n", main = "Solution of Difference Equation")

#Rashmeet Kaur(14401012022)
#SOLUTION OF SYSTEM OF TWO DIFFERENCE EQUATION
# Function to solve a system of linear difference equations
solve_system_difference <- function(a, b, c, d, f1, f2, initial_values, n_terms) {
  # Extract initial values
  x_0 <- initial_values[1]
  y_0 <- initial_values[2]
  # Initialize sequences
  x <- numeric(n_terms)
  y <- numeric(n_terms)
  x[1] <- x_0
  y[1] <- y_0
  for (n in 2:n_terms) {
    x[n] <- a * x[n - 1] + b * y[n - 1] + f1(n - 1)
    y[n] <- c * x[n - 1] + d * y[n - 1] + f2(n - 1)
  }
  return(list(x = x, y = y))
}
a <- 1   # Coefficient of x_n in the first equation
b <- 1   # Coefficient of y_n in the first equation
c <- 1   # Coefficient of x_n in the second equation
d <- 0.5 # Coefficient of y_n in the second equation
# Define the non-homogeneous functions (set to zero for homogeneous case)
f1 <- function(n) { 0 }  # Non-homogeneous term for x
f2 <- function(n) { 0 }  # Non-homogeneous term for y
# Example non-homogeneous terms: f1 <- function(n) { n }; f2 <- function(n) { n^2 }
# Initial values and number of terms
initial_values <- c(1, 2)  # x_0 and y_0
n_terms <- 15              # Number of terms to compute
# Solve the system of difference equations
solution <- solve_system_difference(a, b, c, d, f1, f2, initial_values, n_terms)
# Display the solutions
cat("Solution for x_n:\n", solution$x, "\n")
cat("Solution for y_n:\n", solution$y, "\n")
# Plot the solutions
plot(0:(n_terms - 1), solution$x, type = "o", col = "blue", pch = 19,
     xlab = "n", ylab = "Value", main = "Solution of System of Difference Equations")
lines(0:(n_terms - 1), solution$y, type = "o", col = "red", pch = 17)
legend("topright", legend = c("x_n", "y_n"), col = c("blue", "red"), pch = c(19, 17))



