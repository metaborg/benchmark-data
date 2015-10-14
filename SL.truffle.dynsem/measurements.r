source("common.r")

runbenchmark <- function(benchmark, variant, variantrev, variantpath) {
  print(paste("Running",benchmark[1], "from path", benchmark[2], "on language", variant))
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")

  datafile.graal.rel <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk.rel <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")

  datafile.graal <- paste(getwd(), "/", datafile.graal.rel, sep="")
  datafile.jdk <- paste(getwd(), "/", datafile.jdk.rel, sep="")

  inputarg <- paste("\"", benchmark[2],"/", benchmark[1], "\"", sep="")
  graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
  jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")

  runres <- system2("./runner.sh", args=c(paste(variantpath), inputarg, graaloutarg, jdkoutarg))

  if(runres != 0){
    print(paste("Error running",benchmark[1],"on language", variant))
    quit("no", status=runres)
  }

  newrow <- data.frame(variant, variantrev, dynsem.rev, graal.rev, paste(benchmark[1]), datafile.graal.rel, datafile.jdk.rel)
  write.table(newrow, file=measurements.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",")

}

# run Oracle benchmarks
apply(benchmarks, 1, runbenchmark, "Oracle", sl.oracle.rev, sl.oracle.path)

# run DS benchmarks
#apply(benchmarks, 1, runbenchmark, "DynSem", sl.metaborg.rev)
