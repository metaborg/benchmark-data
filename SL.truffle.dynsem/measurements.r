source("common.r")

runbenchmark <- function(benchmark, variant, variantrev, variantpath) {
  print(paste("Running",benchmark[1], "from path", benchmark[2], "on language", variant))
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")

  datafile.graal <- paste(getwd(), "/data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk <- paste(getwd(), "/data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")

  inputarg <- paste("\"", benchmark[2],"/", benchmark[1], "\"", sep="")
  graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
  jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")

  runres <- system2("./runner.sh", args=c(paste(variantpath), inputarg, graaloutarg, jdkoutarg))

  if(runres != 0){
    print(paste("Error running",benchmark[1],"on language", variant))
    quit("no", status=runres)
  }
  newrow <- data.frame(variant, variantrev, dynsem.rev, graal.rev, paste(benchmark[1]), datafile.graal, datafile.jdk)
  write.table(newrow, file=measurements.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",")


  # Fibonacci-iter.sl,/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/org.metaborg.lang.sl.interp/src/test/resources
  # CallHeavy.sl,/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/org.metaborg.lang.sl.interp/src/test/resources
  # Vars.sl,/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/org.metaborg.lang.sl.interp/src/test/resources
  # BinaryTrees.sl,/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/org.metaborg.lang.sl.interp/src/test/resources
  # While.sl,/Users/vladvergu/tud/slde/projects/semantics/metaborg-sl/org.metaborg.lang.sl.interp/src/test/resources

}

# run Oracle benchmarks
apply(benchmarks, 1, runbenchmark, "Oracle", sl.oracle.rev, sl.oracle.path)

# run DS benchmarks
#apply(benchmarks, 1, runbenchmark, "DynSem", sl.metaborg.rev)
