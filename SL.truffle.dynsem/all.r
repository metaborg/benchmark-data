source("common.r")
source("library.r")
source("measurements.r")

# Re-run all measurements
rerunall <- function() {
  initconfig()
  initrevs()
  fetchdependencies()
  measurements <- loadmeasurements()
  benchmarks <- loadbenchmarks()

  rmdatafiles(measurements)
  measurements <- truncatedata(measurements)
  writemeasurements(measurements)

  temp.file <- "temp.csv"

  for(i in seq(1,nrow(measurements))) {
    measurements[i,] = runexperiment(measurements[i,], temp.file)
    writemeasurements(measurements)
  }

  rmfile("temp.csv")
}

createoraclemeasurement <- function() {
  initconfig()
  initrevs()
  measurements <- loadmeasurements()
  benchmarks <- loadbenchmarks()

  for(i in seq(1, nrow(benchmarks))) {
    t <- nrow(measurements)
    newrow <- c("Oracle", sl.oracle.rev, dynsem.rev, graal.rev, benchmarks[i,], "", "")
    measurements <- rbind(measurements[1:nrow(measurements),], newrow)
  }
  writemeasurements(measurements)
}


createdynsemmeasurement <- function() {
  initconfig()
  initrevs()
  measurements <- loadmeasurements()
  benchmarks <- loadbenchmarks()

  for(i in seq(1, nrow(benchmarks))) {
    t <- nrow(measurements)
    newrow <- c("DynSem", sl.metaborg.rev, dynsem.rev, graal.rev, benchmarks[i,], "", "")
    measurements <- rbind(measurements[1:nrow(measurements),], newrow)
  }
  writemeasurements(measurements)
}


runpending <- function() {
  initconfig()
  initrevs()
  fetchdependencies()
  measurements <- loadmeasurements()
  benchmarks <- loadbenchmarks()

  temp.file <- "temp.csv"
  firstpass <- TRUE
  for(i in seq(1,nrow(measurements))) {
    if(unlist(measurements[i,"GRAALDATA"]) == "" && unlist(measurements[i,"JDKDATA"]) == ""){
      measurements[i,] <- runexperiment(measurements[i,], temp.file, firstpass)
      writemeasurements(measurements)
      firstpass <- FALSE
    }
  }

  rmfile("temp.csv")
}

runexperiment <- function(datarow, temp.file, forcerebuild = FALSE) {
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")
  datafile.graal.rel <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk.rel <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.graal <- paste(getwd(), "/", datafile.graal.rel, sep="")
  datafile.jdk <- paste(getwd(), "/", datafile.jdk.rel, sep="")

  inputarg <- paste("\"", benchmarks.path,"/", unlist(datarow["BENCHMARK"]), "\"", sep="")
  graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
  jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")

  preparecodebases(datarow, forcerebuild)

  # switchrevisions(datarow)
  # initrevs()
  # compileimplementations(datarow)

  runres <- system2("./runner.sh", args=c(paste(getvariantpath(datarow["VARIANT"])), inputarg, graaloutarg, jdkoutarg, "1", "65"))

  datarow["GRAALDATA"] = datafile.graal.rel
  datarow["JDKDATA"] = datafile.jdk.rel

  write.table(datarow, file=temp.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",")
  return(datarow)
}

preparecodebases <- function(datarow, forcerebuild = FALSE) {
  ds.switch.required <- getgitrev(dynsem.repo) != unlist(datarow["DSREV"])
  sl.switch.required <- getgitrev(sl.metaborg.repo) != unlist(datarow["VARIANTREV"])
  ds.build.required <- forcerebuild || ds.switch.required
  sl.build.required <- ds.build.required || sl.switch.required

  # switch versions if needed

  # switch DS version
  if(ds.switch.required) {
    switchgitrev(dynsem.repo, unlist(datarow["DSREV"]))
  }

  # switch SL version
  if(sl.switch.required) {
    switchgitrev(sl.metaborg.repo, unlist(datarow["VARIANTREV"]))
  }

  # init revisions
  initrevs()

  # compile codebases if needed

  # compile DS
  if(ds.build.required) {
    compiledynsem()
  }
  # compile SL
  if(sl.build.required) {
    compilevariant(datarow["VARIANT"])
  }

}

fetchdependencies <- function() {
  version = "2.0.0-SNAPSHOT"

  res = system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.metaborg.sunshine2", version, "jar")) == 0

  res = res && system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.metaborg.meta.lang.template", version, "spoofax-language")) == 0

  res = res && system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.metaborg.meta.lib.analysis", version, "spoofax-language")) == 0

  res = res && system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.metaborg.meta.lang.esv", version, "spoofax-language")) == 0

  quitonfail(ifelse(res, 0, 1), "Download dependencies failed")

}

compilegraal <- function() {
  res = system2("mx", args=c("-p", paste(graal.repo), "clean")) == 0
  res = res && system2("mx", args=c("-p", paste(graal.repo), "build")) == 0
  res = res && system2("mx", args=c("-p", paste(graal.repo), "maven-install")) == 0
  res = res && system2("mx", args=c("-p", paste(graal.repo, "/../truffle/", sep=""), "maven-install")) == 0

  quitonfail(ifelse(res, 0, 1), "Building graal failed")
}

compiledynsem <- function() {
  framework.dir = paste(dynsem.repo, "/org.metaborg.meta.interpreter.framework", sep="")
  res = system2("./mvn-invoke.sh", args=c(framework.dir, "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(framework.dir, "install")) == 0

  quitonfail(ifelse(res, 0, 1), "Compilation of interpreter framework failed")

  metainterp.dir = paste(dynsem.repo, "/org.metaborg.meta.lang.dynsem.interpreter", sep="")
  res = system2("./mvn-invoke.sh", args=c(metainterp.dir, "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(metainterp.dir, "install")) == 0

  quitonfail(ifelse(res, 0, 1), "Compilation of meta-interpreter failed")

  lang.dir = paste(dynsem.repo, "/dynsem", sep="")

  res = system2("./mvn-invoke.sh", args=c(lang.dir, "clean")) == 0
  quitonfail(ifelse(res, 0, 1), "Clean failed")
  res = res && system2("./mvn-invoke.sh", args=c(lang.dir, "install")) == 0

  quitonfail(ifelse(res, 0, 1), "Compilation of DynSem failed")
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
  lang.dir = paste(sl.metaborg.repo, "/org.metaborg.lang.sl", sep="")
  # interp.dir = paste(sl.metaborg.repo, "/org.metaborg.lang.sl.interp", sep="")

  res = system2("./mvn-invoke.sh", args=c(paste(lang.dir), "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(paste(lang.dir), "verify")) == 0

  quitonfail(ifelse(res, 0, 1), "Metaborg SL (language) compilation failed")

  dynsem.proj = paste(dynsem.repo, "/dynsem", sep="")
  sl.metaborg.proj = paste(sl.metaborg.repo, "/org.metaborg.lang.sl/", sep="")

  templatelang = paste("zip://", getwd(), "/target/dependency/org.metaborg.meta.lang.template.spoofax-language", sep="")
  analysislang = paste("zip://", getwd(), "/target/dependency/org.metaborg.meta.lib.analysis.spoofax-language", sep="")
  esvlang = paste("zip://", getwd(), "/target/dependency/org.metaborg.meta.lang.esv.spoofax-language", sep="")

  sunshineargs = c("transform", "-l", dynsem.proj, "-l", templatelang, "-l", analysislang, "-l", esvlang, "-p", sl.metaborg.proj , "-n", "\"Generate interpretable\"", "-i", "trans/semantics/sl.ds")
  args = c("-jar", "target/dependency/org.metaborg.sunshine2.jar", sunshineargs)

  res = system2("java", args=args) == 0
  res = res && system2("./mvn-invoke.sh", args=c(paste(sl.metaborg.repo, "/org.metaborg.lang.sl.interp/", sep=""), "compile")) == 0

  quitonfail(ifelse(res, 0, 1), "Metaborg SL (interpreter) compilation failed")
}
