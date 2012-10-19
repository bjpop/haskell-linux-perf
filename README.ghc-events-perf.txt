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

Note: this version of the code uses the linux-perf library to parse
the perf data file and an artificially inserted nanosleep syscall
to synchronize perf events and the standard eventlog events.
This approach has its benefits and drawbacks compared to the usage
of perf-script for both tasks. Which of the approaches proves better depends
on the evolution of the perf toolset (we are waiting for a patchset
that makes perf clock trustworthy and externally meaningful) and on the
future needs of Haskell users of perf events. For now, we retain
this version on a branch.
