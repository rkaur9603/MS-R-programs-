#Rashmeet Kaur(14401012022)
#PRACTICAL-8
#Predator-Prey Model

prey_predator_eq = function(x_0, y_0, alpha, beta, gamma, delta, t){
  x = x_0
  y = y_0
  for(n in 1:t){
    x[n+1] = (1 + alpha) * x[n] - beta * y[n]
    y[n+1] = (1 - gamma) * y[n] + delta * x[n]
  }
  return(list(D = x, T = y))
}
#parameters
alpha = 0.1 # growth rate of prey
beta = 0.05 # predation rate
delta = 0.4 # rate ate which predators increase by consuming prey
gamma = 0.5 # mortality rate of predators
t = 30 # number of time steps

#initial populations
x_0 = 200 #initial prey population
y_0 = 400 #initial predator population

result = prey_predator_eq(x_0, y_0, alpha, beta, gamma, delta, t)

#plot the populations
plot(0:t, result$D, type = "l", col ="blue", 
     xlab = "Time step", ylab = "Population", main = "Predator-Prey Model Dynamics")
lines(0:t, result$T, col = "red")
legend("topright", legend = c("Prey", "Predator"), col = c("blue", "red"), lty = 1)

#analyze the dynamics
cat("Final prey population:" ,result$D[t+1], "\n")
cat("Final predator population:" ,result$T[t+1], "\n")


#-------------------------------------------------------------------------------------------------------------------
#Rashmeet Kaur(14401012022)
#PRACTICE ASSIGNMENT [Q1]
# Parameters
alpha <- 0.5  # Growth rate of oak trees
beta <- 0.02  # Caterpillar effect on oak trees
gamma <- 0.1  # Caterpillar decay rate
delta <- 0.1  # Oak tree effect on caterpillars

# Initial conditions
O0 <- 1000  # Initial oak tree population
C0_values <- c(200, 500, 800)  # Initial caterpillar populations
n_years <- 50  # Number of years to simulate

# Function to simulate population dynamics
simulate_population <- function(O0, C0, alpha, beta, gamma, 
                                delta, n_years) {
  # Initialize vectors to store populations over time
  O <- numeric(n_years)
  C <- numeric(n_years)
  
  # Set initial conditions
  O[1] <- O0
  C[1] <- C0
  
  # Simulate over time
  for (n in 1:(n_years - 1)) {
    O[n + 1] <- (1 + alpha) * O[n] - beta * C[n]
    C[n + 1] <- (1 - gamma) * C[n] + delta * O[n]
  }
  
  return(list(O = O, C = C))
}

# Simulate for each initial caterpillar population
results <- lapply(C0_values, function(C0) {
  simulate_population(O0, C0, alpha, beta, gamma, delta, n_years)
})

# Plot results
par(mfrow = c(1, 3))  # Create a 1x3 plotting area
for (i in seq_along(C0_values)) {
  C0 <- C0_values[i]
  O <- results[[i]]$O
  C <- results[[i]]$C
  
  plot(1:n_years, O, type = "l", col = "forestgreen", lwd = 2, ylim = range(c(O, C)),
       xlab = "Years", ylab = "Population", main = paste("C0 =", C0))
  lines(1:n_years, C, col = "red", lwd = 2)
  legend("topright", legend = c("Oak Trees", "Caterpillars"), 
         col = c("forestgreen", "red"), lty = 1, lwd = 2)
}
