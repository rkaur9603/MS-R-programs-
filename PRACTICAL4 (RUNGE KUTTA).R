#Rashmeet Kaur (14401012022)
#PRACTICAL-5
#Runge Kutta Method 2nd order
rk2= function(f, xn, yn, h, N, n=0){
  df= NULL
  K=f(xn,yn)
  while(n<N){
    df=rbind(df,data.frame(n=n, Xn=xn, Yn=yn, K=K))
    K1= h*f(xn,yn)
    K2= h*f(xn+h,yn+K1)
    K=(K1+K2)/2
    xn= xn+h
    yn= yn+K
    n=n+1
  }
  print(df)
}
#Define the derivative function f(x,y)
f= function(x,y){
  x+y
}
#Run RK 2nd Order method
df= rk2(f,0,1,0.1,3)


#Rashmeet Kaur (14401012022)
#Practice Assignment-4   Question-1
#Runga-Kutta Method of 2nd Order
# Define the function f(x, y)
f <- function(x, y) {
  exp(x) - y
}

# Initial conditions
x0 <- 0
y0 <- 1
h <- 0.1

# First step: from x = 0 to x = 0.1
k1 <- f(x0, y0)
k2 <- f(x0 + h, y0 + h * k1)
y1 <- y0 + (h / 2) * (k1 + k2)

# Second step: from x = 0.1 to x = 0.2
x1 <- x0 + h
k1 <- f(x1, y1)
k2 <- f(x1 + h, y1 + h * k1)
y2 <- y1 + (h / 2) * (k1 + k2)

# Output the result
cat("The approximate value of y at x = 0.2 is:", y2, "\n")


#Rashmeet Kaur (14401012022)
#Practice Assignment-4   Question-2
#Runga-Kutta Method of 2nd Order
# Define the function f(x, y)
f <- function(x, y) {
  sin(x) + y^2
}

# Initial conditions
x0 <- 0
y0 <- 1
h <- 0.1

# First step: from x = 0 to x = 0.1
k1 <- f(x0, y0)
k2 <- f(x0 + h, y0 + h * k1)
y1 <- y0 + (h / 2) * (k1 + k2)

# Output y at x = 0.1
cat("The approximate value of y at x = 0.1 is:", y1, "\n")

# Second step: from x = 0.1 to x = 0.2
x1 <- x0 + h
k1 <- f(x1, y1)
k2 <- f(x1 + h, y1 + h * k1)
y2 <- y1 + (h / 2) * (k1 + k2)

# Output y at x = 0.2
cat("The approximate value of y at x = 0.2 is:", y2, "\n")


#Rashmeet Kaur (14401012022)
#Practice Assignment-4   Question-3
#Runga-Kutta Method of 2nd Order
# Define the function f(x, y)
f <- function(x, y) {
  3 * x^2 + 2 * y
}

# Initial conditions
x0 <- 0
y0 <- -1
h <- 0.05

# First step: from x = 0 to x = 0.05
k1 <- f(x0, y0)
k2 <- f(x0 + h, y0 + h * k1)
y1 <- y0 + (h / 2) * (k1 + k2)

# Second step: from x = 0.05 to x = 0.1
x1 <- x0 + h
k1 <- f(x1, y1)
k2 <- f(x1 + h, y1 + h * k1)
y2 <- y1 + (h / 2) * (k1 + k2)

# Output the result
cat("The approximate value of y at x = 0.1 is:", y2, "\n")

#---------------------------------------------------------------------------------------------------------------------
#Rashmeet Kaur (14401012022)
#PRACTICAL-7
#Runga-Kutta Method of 4th Order
# Define the Runge-Kutta 4th Order Method
rk4 = function(f, xn, yn, h, N, n = 0) {
  df = NULL
  while (n < N) {
    K1 = h * f(xn, yn)
    K2 = h * f(xn + h / 2, yn + K1 / 2)
    K3 = h * f(xn + h / 2, yn + K2 / 2)
    K4 = h * f(xn + h, yn + K3)
    K = (K1 + 2 * K2 + 2 * K3 + K4) / 6
    
    # Storing the results in a data frame for each iteration
    df = rbind(df, data.frame(n = n, Xn = xn, Yn = yn, K = K))
    
    # Updating the next values of xn and yn
    xn = xn + h
    yn = yn + K
    n = n + 1
  }
  
  # Print the resulting data frame
  print(df)
}

# Define the derivative function f(x, y)
f = function(x, y) {
  return(x + y * y)
}

# Run the RK 4th order method
df = rk4(f, 0, 1, 0.2, 3)


#Rashmeet Kaur (14401012022)
#Practice Assignment-5   Question-1
#Runga-Kutta Method of 4th Order
# Define the function f(x, y)
f <- function(x, y) {
  (x * y) / (1 + x^2)
}

# Initial conditions
x0 <- 1
y0 <- 2
h <- 0.1

# First step: from x = 1 to x = 1.1
k1 <- h * f(x0, y0)
k2 <- h * f(x0 + h/2, y0 + k1/2)
k3 <- h * f(x0 + h/2, y0 + k2/2)
k4 <- h * f(x0 + h, y0 + k3)
y1 <- y0 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)

# Second step: from x = 1.1 to x = 1.2
x1 <- x0 + h
k1 <- h * f(x1, y1)
k2 <- h * f(x1 + h/2, y1 + k1/2)
k3 <- h * f(x1 + h/2, y1 + k2/2)
k4 <- h * f(x1 + h, y1 + k3)
y2 <- y1 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)

# Output the result
cat("The approximate value of y at x = 1.2 is:", y2, "\n")


#Rashmeet Kaur (14401012022)
#Practice Assignment-5   Question-2
#Runga-Kutta Method of 4th Order
# Define the function f(x, y)
f <- function(x, y) {
  exp(x) - 2 * y
}

# Initial conditions
x0 <- 1
y0 <- 1
h <- 0.05

# First step: from x = 1 to x = 1.05
k1 <- h * f(x0, y0)
k2 <- h * f(x0 + h/2, y0 + k1/2)
k3 <- h * f(x0 + h/2, y0 + k2/2)
k4 <- h * f(x0 + h, y0 + k3)
y1 <- y0 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)

# Second step: from x = 1.05 to x = 1.1
x1 <- x0 + h
k1 <- h * f(x1, y1)
k2 <- h * f(x1 + h/2, y1 + k1/2)
k3 <- h * f(x1 + h/2, y1 + k2/2)
k4 <- h * f(x1 + h, y1 + k3)
y2 <- y1 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)

# Output the result
cat("The approximate value of y at x = 1.1 is:", y2, "\n")


#Rashmeet Kaur (14401012022)
#Practice Assignment-5   Question-3
#Runga-Kutta Method of 4th Order
# Define the function f(x, y)
f <- function(x, y) {
  sin(x) + cos(y)
}
x0 <- 0
y0 <- 0
h <- 0.1
# First step: from x = 0 to x = 0.1
k1 <- h * f(x0, y0)
k2 <- h * f(x0 + h/2, y0 + k1/2)
k3 <- h * f(x0 + h/2, y0 + k2/2)
k4 <- h * f(x0 + h, y0 + k3)
y1 <- y0 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
# Second step: from x = 0.1 to x = 0.2
x1 <- x0 + h
k1 <- h * f(x1, y1)
k2 <- h * f(x1 + h/2, y1 + k1/2)
k3 <- h * f(x1 + h/2, y1 + k2/2)
k4 <- h * f(x1 + h, y1 + k3)
y2 <- y1 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
# Third step: from x = 0.2 to x = 0.3
x2 <- x1 + h
k1 <- h * f(x2, y2)
k2 <- h * f(x2 + h/2, y2 + k1/2)
k3 <- h * f(x2 + h/2, y2 + k2/2)
k4 <- h * f(x2 + h, y2 + k3)
y3 <- y2 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
# Fourth step: from x = 0.3 to x = 0.4
x3 <- x2 + h
k1 <- h * f(x3, y3)
k2 <- h * f(x3 + h/2, y3 + k1/2)
k3 <- h * f(x3 + h/2, y3 + k2/2)
k4 <- h * f(x3 + h, y3 + k3)
y4 <- y3 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
# Fifth step: from x = 0.4 to x = 0.5
x4 <- x3 + h
k1 <- h * f(x4, y4)
k2 <- h * f(x4 + h/2, y4 + k1/2)
k3 <- h * f(x4 + h/2, y4 + k2/2)
k4 <- h * f(x4 + h, y4 + k3)
y5 <- y4 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)

cat("The approximate value of y at x = 0.5 is:", y5, "\n")
