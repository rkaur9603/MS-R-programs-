#Rashmeet Kaur(14401012022)
#PRACTICAL-11
#QUEUING SYSTEM

# Parameters
lambda <- 2 # Arrival rate (customers per unit time)
mu <- 3 # Service rate (customers per unit time)
n <- 100 # Number of customers to simulate

# Generate interarrival and service times
set.seed(123) # For reproducibility
interarrival_times <- rexp(n, rate = lambda) # Exponential interarrival times
service_times <- rexp(n, rate = mu) # Exponential service times

# Initialize variables
arrival_times <- cumsum(interarrival_times) # Arrival times
start_service_times <- numeric(n) # When service starts
departure_times <- numeric(n) # Departure times

# Simulate the queue
for (i in 1:n) {
  if (i == 1) {
    start_service_times[i] <- arrival_times[i]
  } else {
    start_service_times[i] <- max(arrival_times[i], departure_times[i - 1])
  }
  departure_times[i] <- start_service_times[i] + service_times[i]
}

# Compute performance metrics
waiting_times <- start_service_times - arrival_times # Time spent waiting
time_in_system <- departure_times - arrival_times # Total time in the system
server_utilization <- sum(service_times) / max(departure_times)

# Results
cat("Average Waiting Time:", mean(waiting_times), "\n")
cat("Average Time in System:", mean(time_in_system), "\n")
cat("Server Utilization:", server_utilization, "\n")

# Optional: Create a data frame for visualization
queue_data <- data.frame(
  Customer = 1:n,
  Arrival_Time = arrival_times,
  Start_Service_Time = start_service_times,
  Departure_Time = departure_times,
  Waiting_Time = waiting_times,
  Time_in_System = time_in_system
)

# Print first few rows of the data
print(head(queue_data))

#-----------------------------------------------------------------------------------------------------------------------------
#Rashmeet Kaur(14401012022)
# PRACTICE ASSIGNMENT-11 [ques-1]
# Parameters
lambda <- 3   # Arrival rate
mu <- 4       # Service rate

# Server utilization (ρ)
rho <- lambda / mu
cat("Server Utilization (ρ):", rho, "\n")

# Average waiting time (Wq)
Wq <- rho / (mu * (1 - rho))
cat("Average Waiting Time (Wq):", Wq, "time units\n")

# Average time in the system (W)
W <- 1 / (mu - lambda)
cat("Average Time in the System (W):", W, "time units\n")

