# TODO: make this script run for multiple time intervals, and not just one

hostnames_arr <- c("ada-cluster", "boole-cluster", "cabbage-cluster")
my_hostname <- system("hostname", intern = TRUE)

## Might as well do this replacement since it saves us some time routing.
if (my_hostname %in% hostnames_arr) {
    hostnames_arr[hostnames_arr == my_hostname] <- "localhost"
}

my_max_processes <- strtoi(system("nproc", intern = TRUE))

## Function takes the following arguments:
## Time -> time in seconds the command will run
## Max_Processes -> the maximum number of processes for each host to use
## Hosts -> List of the hostnames to use
make_output_csv <- function(Time, Max_Processes, Hosts) {
    ## Create an empty dataframe to store all of the results in.
    df = data.frame("hosts"=c(),
                    "time"=c(),
                    "num_processes"=c(),
                    "num_primes"=c())

    for (NUM_PROCESSES in 1:Max_Processes) {
        ## We need to properly format the argument for mpiexec, since mpiexec
        ## expects the argument to be of the form:
        ## <host>:<process_num>(,<host>:<process_num>)*
        HOSTS <- ""
        for (host in Hosts) {
            if (HOSTS == "") {
                HOSTS <- paste0(host, ":", NUM_PROCESSES)
            } else {
                HOSTS <- paste0(HOSTS, ",", host, ":", NUM_PROCESSES)
            }
        }
        NUM_PRIMES <- strtoi(system(paste0("timeout -s 2 ", Time,
                                           " mpiexec -host ", HOSTS,
                                           " ./primes-c/primes-mpi | wc -l"),
                                    intern = TRUE))
        df = rbind(df, data.frame("hosts"=HOSTS,
                                  "time" = Time,
                                  "num_processes" = NUM_PROCESSES,
                                  "num_primes" = NUM_PRIMES))
    }
    outputcsv <- paste0("output", Time, "seconds.csv")
    write.csv(df, outputcsv)
    return(outputcsv)
}

times <- c(5, 10, 20, 40, 60, 120)
for (TIME in times) {
    # Create the dataset.
    data_csv <- make_output_csv(TIME, my_max_processes, hostnames_arr)
    data <- read.csv(data_csv)
    # Ensure that we use the time that is marked in the dataset.
    time_ran = data[1, "time"]

    #Create a picture of the plot, and save it to an image.
    png(file = paste0("primes_vs_processes_", time_ran, "seconds.png"),
        width = 800,
        height = 600)
    plot(x = data$num_processes,
         y = data$num_primes,
         xlab = "Number of Processes",
         ylab = "Number of Primes",
         main = paste0("Primes vs Processes (", time_ran, "s)"))

    ## Source: https://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r
    ## Plot a best fit line, so we can easily compare the different images.
    loess_fit <- loess(data$num_primes ~ data$num_processes, data)
    lines(data$num_processes, predict(loess_fit), col = "blue")
    dev.off()
}
