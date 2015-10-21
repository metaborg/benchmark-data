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
  fetchdependencies()
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

fetchdependencies <- function() {
  version = "1.5.0-SNAPSHOT"

  # download strategoxt-min
  res = system2("./mvn-download.sh", args=c(".", "org.metaborg", "strategoxt-min-jar", version)) == 0

  # download sunshine
  res = res && system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.metaborg.sunshine", version)) == 0

  # download sdf3
  res = res && system2("./mvn-download.sh", args=c(".", "org.metaborg", "org.strategoxt.imp.editors.template", version)) == 0

  quitonfail(ifelse(res, 0, 1), "Download dependencies failed")

  res = rmfile("target/dependency/sdf3") == 0
  res = res && system2("mkdir", args=c("-p", "target/dependency/sdf3")) == 0
  res = res && system2("unzip", args=c("target/dependency/org.strategoxt.imp.editors.template-1.5.0-SNAPSHOT.jar", "-d", "target/dependency/sdf3/")) == 0

  quitonfail(ifelse(res, 0, 1), "Failed to expand SDF3 language")
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
  framework.dir = paste(dynsem.repo, "/org.metaborg.meta.interpreter.framework", sep="")
  res = system2("./mvn-invoke.sh", args=c(framework.dir, "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(framework.dir, "install")) == 0

  quitonfail(ifelse(res, 0, 1), "Compilation of interpreter framework failed")

  lang.dir = paste(dynsem.repo, "/dynsem", sep="")
  res = system2("cp", args=c("auxfiles/dynsem-pom.xml", paste(lang.dir, "/pom.xml", sep=""))) == 0
  res = res && system2("cp", args=c("auxfiles/MANIFEST.MF", paste(lang.dir, "/META-INF/MANIFEST.MF", sep=""))) == 0
  res = res && system2("cp", args=c("auxfiles/GenInterp.java", paste(lang.dir, "/editor/java/dynsem/strategies/GenInterp.java", sep=""))) == 0

  quitonfail(ifelse(res, 0, 1), "Copy failed")

  res = res && system2("./mvn-invoke.sh", args=c(lang.dir, "clean")) == 0
  quitonfail(ifelse(res, 0, 1), "Clean failed")
  res = res && system2("./mvn-invoke.sh", args=c(lang.dir, "compile")) == 0

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
  interp.dir = paste(sl.metaborg.repo, "/org.metaborg.lang.sl.interp", sep="")
  res = system2("cp", args=c("auxfiles/sl-pom.xml", paste(lang.dir, "/pom.xml", sep=""))) == 0
  res = res && system2("./mvn-invoke.sh", args=c(paste(lang.dir), "clean")) == 0
  res = res && system2("./mvn-invoke.sh", args=c(paste(lang.dir), "compile")) == 0
  quitonfail(ifelse(res, 0, 1), "Metaborg SL (language) compilation failed")

  dynsem.mainjar = paste(dynsem.repo, "/include/ds.jar", sep="")
  dynsem.javajar = paste(dynsem.repo, "/include/ds-java.jar", sep="")
  classpath = c("-cp", paste("target/dependency/strategoxt-min-jar-1.5.0.jar", dynsem.mainjar, dynsem.javajar, sep=":"))

  sl.metaborg.proj = paste(sl.metaborg.repo, "/org.metaborg.lang.sl", sep="")
  sl.metaborg.spec = paste(sl.metaborg.proj, "/trans/semantics/sl.ds", sep="")
  args = c(classpath, "dynsem.strategies.GenInterp", sl.metaborg.spec, sl.metaborg.proj)

  res = system2("java", args=args)

  quitonfail(ifelse(res, 0, 1), "Metaborg SL (interpreter) compilation failed")
}
