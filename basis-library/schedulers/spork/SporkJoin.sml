structure SporkJoin = MkSporkJoin (val spork = Scheduler.SporkJoin.greedyWorkAmortizedSpork
                                   val fork = Scheduler.SporkJoin.fork)
