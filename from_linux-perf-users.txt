We greatly benefited from the help of the perf mailing list
<linux-perf-users@vger.kernel.org>. Here is a condensed summary.

> > I'm writing a tool to parse the perf.data output of perf and was wondering about the source of the
> timestamps found in perf_sample events? Specifically I mean the u64 time field in the perf_sample struct
> in util/event.h. I believe it is in nanoseconds from a particular event (system start?), but I can't find
> any documentation about where the time comes from.
> >
> > I'm also interested in reading from this time source from a userspace program, is that possible?

> Code wise, you'll need to follow perf_clock(). I believe for x86 with a
> stable TSC you end up in native_sched_clock() in arch/x86/kernel/tsc.c.

> As for the corresponding userspace call I believe it is
> clock_gettime(CLOCK_MONOTONIC, ...).

But later it does not prove to be the case:

> > Now our main problem is relating the perf timestamps
> > to a public clock API (like posix_gettime, etc.) in order
> > to chronologically merge the perf log with our external event logs.
> > We've so far determined experimentally that even the
> > clock_gettime(CLOCK_MONOTONIC, ...) values are not identical
> > to the perf timestamps at the same places, though they are quite close.

> > We're working around right now by syncing with a timestamp
> > of a known event, but that leaves us vulnerable to a time drift.
> > If there's no way currently to generate externally meaningful
> > timestamps, could we file a feature request for the sake of the future?

> You mean like this: https://lkml.org/lkml/2011/6/7/638?

> I'm still waiting for certain changes to core perf before I try again to get it committed. I haven't forgotten; it's just not ready yet.

> And using the time-of-day patch against a newer kernel (tried 3.2, 3.4 and 3.6) I do see the offset you mentioned.

Another issue:

> > Are we missing anything? Is the "--timestamp" option of perf-record
> > related in any way? Any other tips?

> That option adds the timestamps to the samples if it were not otherwise added.

Yet another:

> > Could someone clarify the behaviour of the -p PID flag of perf-record?
> > In our experiments (3.2.0-27 #43-Ubuntu SMP x86_64),
> > it ignores events on threads spawned after perf-record is started.
> > Is this the intended behaviour, and is there any work-around?

> > Generally, what is the best (vs CPU/IO, distortion of profiling results)
> > way to record only events of a given OS process, with all its threads?

> > The version of perf-record that we use already contains the separate
> > -p PID and -t TID options, so the initial PID patch is already in.
> > It's just that the -p PID option ignores all threads spawned
> > after the process is started, AFAICT.

> AFAIK perf events aren't inherited when -p, -t or -u option is used due
> to a performance reason.  But if you start the process on the command
> line with perf record, it'll be inherited and counted properly.

> I guess your problem might be solved by using a cgroup:

>      mkdir ${cgroup_root}/mygroup
>      echo NNNN > ${cgroup_root}/mygroup/tasks
>      perf record -a -e cycles -G mygroup

Another answer:

> I see the problem now. The perf tool gets FORK events as threads are created,
> but it does not respond to them and create new counters. Some refactoring is needed to make that happen.
