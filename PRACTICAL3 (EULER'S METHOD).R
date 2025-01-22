#Rashmeet Kaur (14401012022)
#PRACTICAL-5
#Euler's Method Function
euler= function(f, xn, yn, h, N, n=0){
  df= NULL
  An=f(xn,yn)
  while(n<N){
    df= rbind(df, data.frame(n=n, Xn=xn, Yn=yn, An=An, hAn=h*An)) #Combine into a data frame for tabular output
    An= f(xn,yn)
    xn= xn+h
    yn= yn+h*An
    n=n+1
  }
  print(df)
}

#Define the derivative function f(x,y)
f= function(x,y){
  y-x
}
df= euler(f,0,2,0.1,5)


#Rashmeet Kaur (14401012022)
#Practice Assignment-3   Question-1
# Euler's method function
euler_method <- function(f, x0, y0, h, steps) {
  x_values <- numeric(steps + 1)
  y_values <- numeric(steps + 1)
  
  x_values[1] <- x0
  y_values[1] <- y0
  
  for (n in 1:steps) {
    y_values[n + 1] <- y_values[n] + h * f(x_values[n], y_values[n])
    x_values[n + 1] <- x_values[n] + h
  }
  
  return(data.frame(X = x_values, Y = y_values))
}

f <- function(x, y) {
  return(x^2 - y)
}

# Initial conditions and parameters
x0 <- 0   # initial x
y0 <- 2   # initial y
h <- 0.2  # step size
steps <- 2  # number of steps (to reach x = 0.4)

# Apply Euler's method
result <- euler_method(f, x0, y0, h, steps)

print(result)


#Rashmeet Kaur (14401012022)
#Practice Assignment-3   Question-2
#Euler's method function
euler_method <- function(f, x0, y0, h, steps) {
  x_values <- numeric(steps + 1)
  y_values <- numeric(steps + 1)
  
  x_values[1] <- x0
  y_values[1] <- y0
  
  for (n in 1:steps) {
    y_values[n + 1] <- y_values[n] + h * f(x_values[n], y_values[n])
    x_values[n + 1] <- x_values[n] + h
  }
  
  return(data.frame(X = x_values, Y = y_values))
}

# Define the differential equation dy/dx = y - x^2
f <- function(x, y) {
  return(y - x^2)
}

# Initial conditions and parameters
x0 <- 0   # initial x
y0 <- 0.5 # initial y
h <- 0.05 # step size
steps <- 3  # number of steps (to calculate y at x = 0.15)

# Apply Euler's method
result <- euler_method(f, x0, y0, h, steps)

# Print the result
print(result)



#Rashmeet Kaur (14401012022)
#Practice Assignment-3   Question-3
# Euler's method function
euler_method <- function(f, x0, y0, h, steps) {
  x_values <- numeric(steps + 1)
  y_values <- numeric(steps + 1)
  
  x_values[1] <- x0
  y_values[1] <- y0
  
  for (n in 1:steps) {
    y_values[n + 1] <- y_values[n] + h * f(x_values[n], y_values[n])
    x_values[n + 1] <- x_values[n] + h
  }
  
  return(data.frame(X = x_values, Y = y_values))
}

# Define the differential equation dy/dx = 3x + y
f <- function(x, y) {
  return(3*x + y)
}

# Initial conditions and parameters
x0 <- 0.5  # initial x
y0 <- 1    # initial y
h <- 0.1   # step size
steps <- 3  # number of steps (to calculate y at x = 0.8)

# Apply Euler's method
result <- euler_method(f, x0, y0, h, steps)

# Print the result
print(result)

