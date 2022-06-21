#include <mpi.h>
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

long long toss (long long num_process_tosses, int my_rank);
void get_input(int argc, char** argv, int my_rank, long long* totalNumTosses_p);

int main(int argc, char** argv) {
    int my_rank, num_procs;
    long long total_num_tosses, num_process_tosses, process_number_in_circle, total_number_in_circle;
    double start, finish, loc_elapsed, elapsed, pi_estimate;
    double PI_25 = 3.141592653589793238462643;         // Pi to 25 digits

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);


    get_input(argc, argv, my_rank, &total_num_tosses);  // Read total number of tosses from command line

    num_process_tosses = total_num_tosses/num_procs;

    MPI_Barrier(MPI_COMM_WORLD);
    start = MPI_Wtime();
    process_number_in_circle = toss(num_process_tosses, my_rank);
    finish = MPI_Wtime();
    loc_elapsed = finish-start;

    MPI_Reduce(&loc_elapsed, &elapsed, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    MPI_Reduce(&process_number_in_circle, &total_number_in_circle, 1, MPI_LONG, MPI_SUM, 0, MPI_COMM_WORLD);

    if(my_rank == 0){
        pi_estimate = (4 * total_number_in_circle)/((double)total_num_tosses);
        printf("Elapsed time = %f seconds \n", elapsed);
        printf("Pi is approximately %.16f, Error is %.16f\n", pi_estimate, fabs(pi_estimate - PI_25));
    }
    MPI_Finalize();
    return 0;
}

// Function gets input from command line for total_num_tosses
void get_input(int argc, char** argv, int my_rank, long long* totalNumTosses_p){
    if(my_rank == 0){
        if(argc != 2){
            fprintf(stderr, "usage: mpirun -np <N> %s <number of tosses> \n", argv[0]);
            fflush(stderr);
            *totalNumTosses_p = 0;
        }
        else
            *totalNumTosses_p = atoi(argv[1]);
    }
    // Broadcasts value of total_num_tosses to each process
    MPI_Bcast(totalNumTosses_p, 1, MPI_LONG, 0, MPI_COMM_WORLD);

    // 0 total_num_tosses ends the program
    if(*totalNumTosses_p == 0){
        MPI_Finalize();
        exit(-1);
    }
}

// Function implements Monte Carlo version of tossing darts at a board
long long toss (long long processTosses, int my_rank){
    long long toss, numberInCircle = 0;
    double x, y;
    unsigned int seed = (unsigned)time(NULL);
    srand(seed + my_rank);
    for(toss = 0; toss < processTosses; toss++){
        x = rand_r(&seed)/(double)RAND_MAX;
        y = rand_r(&seed)/(double)RAND_MAX;
        if(((x*x) + (y*y)) <= 1.0)
            numberInCircle++;
    }
    return numberInCircle;
}
