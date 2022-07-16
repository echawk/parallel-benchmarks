
data <- read.csv('output.csv')

time_ran = data[1, "time"]

plot(x = data$num_processes,
     y = data$num_primes,
     xlab = "Number of Processes",
     ylab = "Number of Primes",
     main = paste0("Primes vs Processes (", time_ran, "s)") )
