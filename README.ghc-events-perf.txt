Crash-course of the ghc-events-perf tool:

ghc --make -eventlog -rtsopts -threaded MyProgram.hs

ghc-events-perf record MyProgram --RTS +RTS -N2 -l-g-p

ghc-events-perf convert MyProgram

threadscope MyProgram.total.eventlog


A longer example, pointing out some common pitfalls and using the test
data files from the test/ directory of the library's github repository.
The Haskell program test/ParFib.hs is compiled with GHC >= 7.8 as follows

ghc --make -eventlog -rtsopts -threaded ParFib.hs

Test data files corresponding to test/ParFib.perf.data and test/ParFib.eventlog
can be created with

sudo path-to/ghc-events-perf record +GhcEventsPerf -o ParFib.perf.data -GhcEventsPerf ./ParFib --RTS +RTS -N2 -la

At this point, one can change the owner or permissions of ParFib.perf.data
and parse and view it with the standard 'perf script' command or with
the 'dump-perf' tool that uses our Haskell library for parsing perf data.

The perf data can be transformed to an eventlog and synchronized
and merged with the standard eventlog using

ghc-events-perf convert ParFib ParFib.total.eventlog ParFib.perf.data ParFib.eventlog

The resulting big eventlog can be displayed with

ghc-events show ParFib.total.eventlog | less

but it's best viewed in ThreadScope with

threadscope ParFib.total.eventlog

in the Timeline main pane, with Instant Events selected in the Traces tab
and with the View/Event_Labels option on. If no perf events show up
in the Instant Events traces, your GHC is probably 7.6 or older.

Note: in this version of haskell-linux-perf we can only obtain
the perf data by running 'ghc-events-perf record'. Neither 'perf record'
nor 'perf script record' suffice, because we need to insert
special synchronizing events into the perf data.

Note: this version of the code controls the execution of the Haskell
program to be profiled and of the perf-record tool by launching them,
passing around a PID and waiting for termination or terminating,
as appropriate. If needed, this can be extended to temporarily suspending
the Haskell program, e.g., to make a synchronizing syscall more often
than just at the start of execution.

Apart of that, the code uses the linux-perf library to parse the perf
data file and the nanosleep syscall to synchronize perf events
with the standard eventlog. (See the start of the ghc-events-perf-record.hs
file for more details.) This approach has its benefits and drawbacks
compared to the usage of perf-script and to starting the Haskell program
by the perf-record tool. Which of the approaches proves better depends
on the evolution of the perf toolset  (we are waiting for a patchset
that makes perf clock trustworthy and externally meaningful, as well as
for a patch that fixes the '-p' option). It also depends on the future needs
of Haskell users of perf events. In particular, the '-p' option seems
to produce much more perf events, even though in its present state it ignores
all tasks spawned after the Haskell program is started. Whether the extra
events are useful requires further investigation. For now, we retain
this version on a branch.
