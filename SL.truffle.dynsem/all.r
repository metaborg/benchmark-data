source("common.r")
source("library.r")
source("measurements.r")

fakeinit <- function () {
  initconfig()
  initrevs()
  return(truncatedata(loadmeasurements()))
}

# Re-run all measurements
rerunall <- function() {
  initconfig()
  initrevs()
  measurements <- loadmeasurements()
  benchmarks <- loadbenchmarks()

  rmdatafiles(measurements)
  truncatedata(measurements)
  writemeasurements(measurements)

  temp.file <- "temp.csv"
  measurements <- runexperiment(measurements, tempfile)
  writemeasurements(measurements)

  rmfile("temp.csv")
}

runexperiment <- function(datarow, temp.file) {
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")
  datafile.graal.rel <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk.rel <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.graal <- paste(getwd(), "/", datafile.graal.rel, sep="")
  datafile.jdk <- paste(getwd(), "/", datafile.jdk.rel, sep="")

  inputarg <- paste("\"", benchmarks.path,"/", datarow["BENCHMARK"], "\"", sep="")
  graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
  jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")

  switchrevisions(datarow)
  initrevs()
  compileimplementations(datarow)

  # runres <- system2("./runner.sh", args=c(paste(getvariantpath(datarow["VARIANT"])), inputarg, graaloutarg, jdkoutarg))

  datarow["GRAALDATA"] = datafile.graal.rel
  datarow["JDKDATA"] = datafile.jdk.rel

  write.table(datarow, file=temp.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",")
  return(datarow)
}

switchrevisions <- function(datarow) {
  # switch variant
  if(datarow["VARIANT"] == "Oracle") {
    # switch hg revision
    switchgitrev(sl.oracle.repo, unlist(datarow["VARIANTREV"]))
  } else if(datarow["VARIANT"] == "DynSem") {
    # switch git revision
    switchgitrev(sl.metaborg.repo, unlist(datarow["VARIANTREV"]))
  } else {
    quitonfail(-1, paste("Unknown implementation variant:", datarow["VARIANT"]))
  }
  # switch DynSem repo
  switchgitrev(dynsem.repo, unlist(datarow["DSREV"]))

  # switch Graal repo
  switchgraalrev(graal.repo, mx.repo, unlist(datarow["GRAALREV"]))
}

compileimplementations <- function(datarow) {
  # compile graal implementation
  # compilegraal()

  # compile DynSem
  # compiledynsem()

  # compile language implementation
  compilevariant(datarow["VARIANT"])
}

compilegraal <- function() {
  res = system2("mx", args=c("-p", paste(graal.repo), "build")) == 0
  res = res && system2("mx", args=c("-p", paste(graal.repo), "maven-install")) == 0

  quitonfail(ifelse(res, 0, 1), "Building graal failed")
}

compiledynsem <- function() {
  quitonfail(42, "Not implemented")
}

compilevariant <- function(variant) {
  if(variant == "Oracle") {
    compilesloracle()
  } else if(variant == "DynSem") {
    compilesldynsem()
  } else {
    quitonfail(-1, paste("Unknown implementation variant:", variant))
  }
}

compilesloracle <- function() {
  res = system2("./mvn-invoke.sh", args=c(paste(sl.oracle.path), "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(paste(sl.oracle.path), "compile")) == 0

  quitonfail(ifelse(res, 0, 1), "Oracle SL compilation failed")
}

compilesldynsem <- function() {
  quitonfail(42, "Not implemented")
}
