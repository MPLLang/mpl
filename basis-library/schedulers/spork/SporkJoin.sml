structure SporkJoin =
  MkSporkJoin
    (val sporkFair = Scheduler.SporkJoin.sporkFair
     val sporkKeep = Scheduler.SporkJoin.sporkKeep
     val sporkGive = Scheduler.SporkJoin.sporkGive
     val tryPromoteNow = Scheduler.SporkJoin.tryPromoteNow
     val noTokens = Scheduler.SporkJoin.noTokens)
