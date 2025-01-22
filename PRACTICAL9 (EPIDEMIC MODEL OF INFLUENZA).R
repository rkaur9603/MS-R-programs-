# Rashmeet Kaur(14401012022)
# EPIDEMIC MODEL OF INFLUENZA

install.packages("deSolve")
library(deSolve)
#Parameters
N=1000       #Total population
beta= 0.001  #Transmission rate
gamma= 0.1   #Recovery rate (1/10 days)
I0= 0.01*N   #Initial infected (1% of population)
S0= N-I0     #Initial susceptible (99% of population)
R0= 0        #Initial recovered individuals

#Parameters for the model
parameters= c(beta= beta, gamma= gamma)
#Initial state vector (S,I,R)
state= c(S= S0, I= I0, R=R0)

#Define the SIR model as a set of differential equations
sir_model= function(t, state, parameters){
  #Extract the state variables (S,I,R)
  with(as.list(c(state, parameters)),{
    #Differential equations
    dS= -beta * S * I
    dI= beta * S * I - gamma *I
    dR= gamma * I
    #Return the rate of change
    return(list(c(dS, dI, dR)))
  })
}
#Time vector (simulate for 40 days)
time= seq(0, 40, by= .1)
#Solve the system of ODEs using the ode function from deSolve
output= ode(y= state, times= time, func= sir_model, parms= parameters)
#Convert the output into a data frame for easier handling
output_df= as.data.frame(output)

#Plot the results
plot(output_df$time, output_df$S, type="l", col= "blue",
     xlab= "Time(days)", ylab= "Population", lwd=2, ylim=c(0,N),
     main="Epidemic Progression (SIR Model)")
lines(output_df$time, output_df$I, col="red", lwd=2)
lines(output_df$time, output_df$R, col="green", lwd=2)
legend("right", legend=c("Susceptible", "Infected", "Recovered"),
       col= c("blue", "red", "green"), lwd=2)

#----------------------------------------------------------------------------------------------------------------------------
#EPIDEMIC MODEL PRACTICE QUES
#Rashmeet Kaur (14401012022)
#PRACTICE ASSIGNMENT-9 [ques1]

# Load required library
library(deSolve)
# Define the SIR model
SIR <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}
# Initial parameters
initial_state <- c(S = 499, I = 1, R = 0)
parameters <- c(beta = 0.002, gamma = 1/7)
time <- seq(0, 20, by = 0.1)
# Solve the SIR model
output <- ode(y = initial_state, times = time, func = SIR, parms = parameters)
output <- as.data.frame(output)
# Plot the time-dependent solution
#plot(output$time, output$S, type = "l", col = "blue", ylim = c(0, 500), 
#    ylab = "Population", xlab = "Time (days)", lwd = 2, main = "SIR Model")
#lines(output$time, output$I, col = "red", lwd = 2)
#lines(output$time, output$R, col = "green", lwd = 2)
#legend("right", legend = c("Susceptible", "Infected", "Recovered"), 
#       col = c("blue", "red", "green"), lwd = 2)

# (a) Number of susceptible who never get infected
S_infinity <- min(output$S)
cat("Number of susceptible who never get infected:", S_infinity, "\n")

# (b) Effect of S(0) = 100
initial_state_100 <- c(S = 100, I = 1, R = 0)
output_100 <- ode(y = initial_state_100, times = time, func = SIR, parms = parameters)
output_100 <- as.data.frame(output_100)

plot(output_100$time, output_100$S, type = "l", col = "blue", ylim = c(0, 100), 
     ylab = "Population", xlab = "Time (days)", lwd = 2, main = "SIR Model with S(0) = 100")
lines(output_100$time, output_100$I, col = "red", lwd = 2)
lines(output_100$time, output_100$R, col = "green", lwd = 2)
legend("right", legend = c("Susceptible", "Infected", "Recovered"), 
       col = c("blue", "red", "green"), lwd = 2)

# (c) Doubling the transmission coefficient
parameters_beta_doubled <- c(beta = 0.004, gamma = 1/7)
output_beta_doubled <- ode(y = initial_state, times = time, func = SIR, parms = parameters_beta_doubled)
output_beta_doubled <- as.data.frame(output_beta_doubled)

plot(output_beta_doubled$time, output_beta_doubled$I, type = "l", col = "red", 
     ylab = "Infected Population", xlab = "Time (days)", lwd = 2, 
     main = "Effect of Doubling Transmission Coefficient")
lines(output$time, output$I, col = "blue", lwd = 2)
legend("right", legend = c("Original Beta", "Doubled Beta"), col = c("blue", "red"), lwd = 2)

cat("Maximum number of infected individuals with original beta:", max(output$I), "\n")
cat("Maximum number of infected individuals with doubled beta:", max(output_beta_doubled$I), "\n")

