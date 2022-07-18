
hostnames_arr <- c("ada-cluster", "boole-cluster", "cabbage-cluster")
my_hostname <- system("hostname", intern = TRUE)

if (my_hostname %in% hostnames_arr) {
    hostnames_arr[hostnames_arr == my_hostname] <- "localhost"
}

my_max_processes <- strtoi(system("nproc", intern = TRUE))

df = data.frame("hosts"=c(), "time"=c(), "num_processes"=c(), "num_primes"=c())

TIME <- 5

for (process_n in 1:my_max_processes) {
    NUM_PROCESSES <- process_n
    HOSTS <- ""
    for (host in hostnames_arr) {
        if (HOSTS == "") {
            HOSTS <- paste0(host, ":", NUM_PROCESSES)
        } else {
            HOSTS <- paste0(HOSTS, ",", host, ":", NUM_PROCESSES)
        }
    }
    NUM_PRIMES <- strtoi(system(paste0("timeout -s 2 ", TIME, " mpiexec -host ", HOSTS, " ./primes-c/primes-mpi | wc -l")))
    df = rbind(df, data.frame("hosts"=HOSTS,
                              "time" = TIME,
                              "num_processes" = NUM_PROCESSES,
                              "num_primes" = NUM_PRIMES))
}

write.csv(df, "output.csv")

data <- read.csv('output.csv')

time_ran = data[1, "time"]

plot(x = data$num_processes,
     y = data$num_primes,
     xlab = "Number of Processes",
     ylab = "Number of Primes",
     main = paste0("Primes vs Processes (", time_ran, "s)"))
