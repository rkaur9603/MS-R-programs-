#Rashmeet Kaur(14401012022)
#PRACTICAL-7
#Growth and Decay Model

#GROWTH:-
solve_eq = function(x_0, a, b, n) {
  x = numeric(n + 1)       # Initialize vector to store values
  x[1] = x_0               # Set initial value
  for(t in 1:n) {
    x[t + 1] = a * x[t] + b
  }
  return(x)
}

# Set initial conditions and parameters
result = solve_eq(10, 1.2, 3, 10)
print(result)

# Plot the population over time (from day 0 to day 10)
plot(0:10, result, type="o", col="blue", xlab="Days", ylab="Population Size", 
     main="Population Growth Over Time", pch=16, lwd=2)


#DECAY:-
result = solve_eq(150, 0.97, 0, 15)
print(result)

# Plot the population decay over time (from day 0 to day 15)
plot(0:15, result, type="o", col="blue", xlab="Days", ylab="Population Size", 
     main="Population Decay Over Time", pch=16, lwd=2)

#---------------------------------------------------------------------------------------------------------------------
# Rashmeet Kaur(14401012022)
# PRACTICE ASSIGNMENT-7 [ques-1]

# Initial population
initial_population <- 150000

# Annual growth rate (2%)
growth_rate <- 0.02

# Annual immigration
immigration <- 3000

# Number of years
years <- 10

# Initialize vectors to store year and population data
year <- 0:years
population <- numeric(length(year))

# Set initial population
population[1] <- initial_population

# Calculate population for each year
for (i in 2:(years + 1)) {
  population[i] <- population[i - 1] + (population[i - 1] * growth_rate) + immigration
}

# Final population after 10 years
cat("Final population after 10 years:", population[years + 1], "units\n")

# Plot the graph
plot(year, population, type = "o", col = "blue", lwd = 2, pch = 16,
     xlab = "Year", ylab = "Population",
     main = "Population Growth Over 10 Years")
grid()


#Rashmeet Kaur(14401012022)
#PRACTICE ASSIGNMENT-7 [ques-2]

# Room temperature
T_room <- 70

# Observed temperatures
T_1 <- 45  # at t = 0.5 hours
T_2 <- 55  # at t = 1 hour

# Time intervals
t1 <- 0.5
t2 <- 1

# Solve for cooling constant (k) and initial temperature
solve_refrigerator_temp <- function(T_1, T_2, T_room, t1, t2) {
  # Calculate cooling constant k
  k <- log((T_room - T_2) / (T_room - T_1)) / (t1 - t2)
  
  # Calculate initial temperature T_initial
  T_initial <- T_room - (T_room - T_1) * exp(-k * t1)
  
  return(list(k = k, T_initial = T_initial))
}

# Solve for k and initial temperature
result <- solve_refrigerator_temp(T_1, T_2, T_room, t1, t2)
k <- result$k
T_initial <- result$T_initial

# Generate temperature data for plotting
time <- seq(0, 1.5, by = 0.1)  # Time range from 0 to 1.5 hours
temperature <- T_room + (T_initial - T_room) * exp(-k * time)

# Plot the graph
plot(time, temperature, type = "l", col = "blue", lwd = 2,
     xlab = "Time (hours)", ylab = "Temperature (°F)",
     main = "Cooling of Soda Can")
points(c(0.5, 1), c(T_1, T_2), col = "red", pch = 16)  # Mark the given points
grid()

# Annotate the graph
text(0.5, T_1, labels = paste0("T(0.5) = ", T_1, "°F"), pos = 4, col = "red")
text(1, T_2, labels = paste0("T(1) = ", T_2, "°F"), pos = 4, col = "red")

#Rashmeet Kaur(14401012022)
#PRACTICE ASSIGNMENT-7 [ques-3]

# Parameters
P0 <- 2000  # Initial amount of substance
K <- 100    # Carrying capacity
r <- 0.03   # Decay rate
time_steps <- 50  # Total time steps

# Logistic decay function
logistic_decay <- function(P0, K, r, t) {
  K * P0 / (P0 + (K - P0) * exp(-r * t))
}

# Calculate population over time
time <- 0:time_steps
population <- logistic_decay(P0, K, r, time)

# Population after 50 time steps
cat("Population after 50 time steps:", round(population[time_steps + 1]), "units\n")

# Plot the population over time
plot(time, population, type = "l", col = "blue", lwd = 2,
     xlab = "Time Steps", ylab = "Population",
     main = "Logistic Decay of Chemical Substance")
grid()

