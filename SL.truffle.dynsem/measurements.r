source("common.r")

runbenchmark <- function(benchmark, variant, variantrev, variantpath) {
  print(paste("Running",benchmark[1], "from path", benchmark[2], "on language", variant))
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")

  datafile.graal <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")

  # runres <- system2("mvn",args=c("exec:exec", "-Dinputfile=",paste(benchmark[2]), "-Dgraaldatafile=", datafile.graal, "-Djdkdatafile=",datafile.jdk))

  newrow <- data.frame(variant, variantrev, dynsem.rev, graal.rev, paste(benchmark[1]), datafile.graal, datafile.jdk)
  write.table(newrow, file=measurements.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",")
}

# run Oracle benchmarks
measurements <- apply(benchmarks, 1, runbenchmark, "Oracle", sl.oracle.rev, sl.oracle.path)

# run DS benchmarks
#apply(benchmarks, 1, runbenchmark, "DynSem", sl.metaborg.rev)
