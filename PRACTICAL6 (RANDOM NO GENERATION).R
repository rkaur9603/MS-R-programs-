#Rashmeet Kaur (14401012022)
#PRACTICAL-6
#RANDOM NUMBER GENERATION

#Generate sequence of numbers with : operator
c(1:10, 20:30)

-5:5

#GENERATING RANDOM NUMBERS FROM UNIFORM DISTRIBUTION
#Five Random numbers between 0 and 1
runif(5)

#Generate 5 random numbers between 10 and 20
runif(5, min=10, max=20)

set.seed(123)
runif(5)

#GENERATING RANDOM NUMBERS FROM NORMAL DISTRIBUTION
#Generating normally distributed numbers
rnorm(5, mean=0, sd=1)
r= rnorm(10, mean=10, sd=2)

#GENERATING SAMPLE FROM A SEQUENCE OF NUMBERS
sample(0:40, 10, replace=T)
sample(0:40, 10, replace=F)

#GENERATING RANDOM NUMBERS FROM A BINOMIAL DISTRIBUTION
rbinom(5, size=10, prob=0.5)

#GENERATING RANDOM NUMBERS FROM A POISSON DISTRIBUTION
#Generate 5 random numbers from a Poisson distribution with lamba= 3
rpois(5, lambda = 3)

#GENERATING UNIFORMLY DISTRIBUTED RANDOM NUMBERS USING LINEAR CONGRUENTIAL METHOD
#Linear Congruential generator
lcg= function(a,c,m,run.length, seed){
  x=NULL
  x[1]= seed
  for(i in 1:(run.length-1)){
    x[i+1]<-(a*x[i]+c)%%m
  }
  U<- x/m
  return(list(x=x, U=U))
}
lcg(6,7,23,20,5)

#RANDOM NO PRACTICE QUES
#Rashmeet Kaur (14401012022)
#PRACTICE QUES-1
# Set mean and standard deviation
mean_value <- 50
sd_value <- 10

# Generate 20 random numbers from normal distribution
random_numbers <- rnorm(20, mean = mean_value, sd = sd_value)

# Display the result
print(random_numbers)

#Rashmeet Kaur (14401012022)
#PRACTICE QUES-2
# Define numbers and their corresponding probabilities
numbers <- 1:4
probabilities <- c(0.1, 0.2, 0.3, 0.4)

# Generate a random sample of 10 numbers with the specified probabilities
sample_numbers <- sample(numbers, 10, replace = TRUE, prob = probabilities)

# Display the result
print(sample_numbers)

#Rashmeet Kaur (14401012022)
#PRACTICE QUES-3
# Set number of rows
n <- 10

# Generate random integers between 1 and 10
integers <- sample(1:10, n, replace = TRUE)

# Generate random normal values with mean 0 and standard deviation 1
normal_values <- rnorm(n, mean = 0, sd = 1)

# Generate random binary values (0 or 1)
binary_values <- sample(0:1, n, replace = TRUE)

# Create a data frame
df <- data.frame(Integers = integers, NormalValues = normal_values, BinaryValues = binary_values)

# Display the data frame
print(df)

#Rashmeet Kaur (14401012022)
#PRACTICE QUES-4
# Parameters for the linear congruential generator
a <- 5
c <- 3
m <- 16
n <- 10  # Number of random numbers to generate

# Randomly choose a seed between 0 and 15
X <- sample(0:15, 1)
random_numbers <- numeric(n)

# Generate the sequence of pseudo-random numbers
for (i in 1:n) {
  X <- (a * X + c) %% m
  random_numbers[i] <- X
}

# Display the generated numbers
print(random_numbers)

# Adjust margins and visualize the results
par(mar = c(2, 2, 2, 2))  # Set smaller margins
plot(random_numbers, type = "o", col = "blue", main = "Linear Congruential Generator Results",
     xlab = "Index", ylab = "Generated Numbers")
