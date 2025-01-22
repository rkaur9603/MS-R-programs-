#Rashmeet Kaur(14401012022)
#PRACTICAL-8
#Monte Carlo Integration
nums_dots= 1000
dots_in_circle= 0
for(i in 1:nums_dots){
  x= runif(n=1, min= -1, max=1)
  y= runif(n=1, min= -1, max=1)
  if(x^2 + y^2 <= 1){
    dots_in_circle= dots_in_circle + 1
  }
}
print (4*(dots_in_circle/nums_dots))

#Rashmeet Kaur(14401012022)
nums_dots= 100
dots_in_circle= 0
x= runif(n= nums_dots, min=-1, max=1)
y= runif(n= nums_dots, min=-1, max=1)
for(i in 1:50){
  plot(x[1:i],y[1:i], xlim= c(1,1), ylim=c(-1,1))
  Sys.sleep(0.5)
}

#Rashmeet Kaur(14401012022)
f=function(x)
{
  x^2
}
curve(f, from= -3, to=3)
ran_points= runif(1000, min=-3, max=3)
int= 6*(mean(f(ran_points)))
print(int)
int_estimate= vector(length=10000)
for(i in 1:10000){
  ran_nums= runif(i, min=-3, max=3)
  int_estimate[i]= 6*(mean(f(ran_nums)))
}
plot(int_estimate)
abline(h=18, col="yellow", lwd=3)

#MONTE CARLO PRACTICE QUESTIONS
#practiceques 1
#Rashmeet Kaur(14401012022)
# Define the function to integrate
f <- function(x) {
  exp(-x^2 / 2)
}

# Plot the function over the range 0 to 1
curve(f, from= 0, to=1, col="blue")

# Generate random points in the interval [0, 1]
ran_points = runif(1000, min=0, max=1)

# Estimate the integral using Monte Carlo method
int = 1 * (mean(f(ran_points)))  # Since the interval length is 1
print(int)

# Calculate the integral estimate for increasing numbers of random samples
int_estimate = vector(length=10000)
for (i in 1:10000) {
  ran_nums = runif(i, min=0, max=1)
  int_estimate[i] = 1 * (mean(f(ran_nums)))  # Interval length is 1
}

# Plot the convergence of the integral estimate
plot(int_estimate, type="l", col="red", xlab="Number of samples", ylab="Integral Estimate")
abline(h=sqrt(pi/2) * erf(1/sqrt(2)), col="yellow", lwd=3)  # Exact value for reference


#practice ques2
#Rashmeet Kaur(14401012022)
# Define the function to integrate
f <- function(x) {
  x^3 * sin(x)
}

# Plot the function over the range 0 to 1
curve(f, from= 0, to=1, col="blue")

# Generate random points in the interval [0, 1]
ran_points = runif(1000, min=0, max=1)

# Estimate the integral using Monte Carlo method
int = 1 * (mean(f(ran_points)))  # Since the interval length is 1
print(int)

# Calculate the integral estimate for increasing numbers of random samples
int_estimate = vector(length=10000)
for (i in 1:10000) {
  ran_nums = runif(i, min=0, max=1)
  int_estimate[i] = 1 * (mean(f(ran_nums)))  # Interval length is 1
}

# Plot the convergence of the integral estimate
plot(int_estimate, type="l", col="red", xlab="Number of samples", ylab="Integral Estimate")
abline(h=-3 * sin(1) + 3 * cos(1) + 6, col="yellow", lwd=3)  # Exact value for reference
