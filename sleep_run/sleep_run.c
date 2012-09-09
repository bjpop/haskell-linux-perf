/* Execute a command with a call to nanosleep at the start. 
** This is useful for recording linux perf events for GHC compiled
** programs, where we want to synchronise the timestamps of perf
** events and GHC RTS events. The call to nanosleep at the start
** can be seen in the linux perf event trace. We can line up the
** (linux perf) timestamp of that event with the start of the 
** Haskell program, and thus determine its approximate GHC RTS
** event timestamp.
**
** Use like so:
**
** sleep_run command [command_args ...]
**
** eg:
**
** sleep_run ./Factorial 10
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

int main (int argc, char **argv)
{
    char *command; 
    char **command_argv;
    struct timespec t;


    if (argc < 2)
    {
        fprintf(stderr, "%s: You did not supply a command to execute\n", argv[0]);
        fprintf(stderr, "Usage: %s command [command_args ...]\n", argv[0]);
        exit(1);
    }

    // The name of the command to run is the first argument of this program.
    command = argv[1];

    // The argv for the command to run is the same as the argv for this program
    // shifted down by one.
    command_argv = &(argv[1]);

    // nanosleep for 3 nanoseconds.
    t.tv_sec = 0;
    t.tv_nsec = 3;

    if (nanosleep(&t, NULL) != 0)   
    {
        printf("%s: error, nanosleep failed!\n", argv[0]);
        exit(2);
    }

    // Run the command.
    execve(command, command_argv, NULL);

    // We should never get here, but we include the statement to make C happy.
    return 0;
}
