structure SporkJoin =
  MkSporkJoin
    (val spork = Scheduler.SporkJoin.spork
     val tryPromoteNow = Scheduler.SporkJoin.tryPromoteNow
     val noTokens = Scheduler.SporkJoin.noTokens)
