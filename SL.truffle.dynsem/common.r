# Measurements file
measurements.file <- "measurements.csv"

# Configuration file
config <- read.csv("configuration.csv")

# Benchmarks file
benchmarks.file <- "benchmarks.csv"

# Plots dir
plots.dir <- "plots"

source("library.r")

initconfig <- function() {
  sl.metaborg.repo <<- unlist(config[1, "METABORGSLREPO"])
  sl.oracle.repo <<- unlist(config[1, "ORACLESLREPO"])
  mx.repo <<- unlist(config[1, "MXREPO"])
  dynsem.repo <<- unlist(config[1, "DYNSEMREPO"])
  graal.repo <<- unlist(config[1, "GRAALREPO"])
  sl.metaborg.path <<- unlist(config[1, "METABORGSLPATH"])
  sl.oracle.path <<- unlist(config[1, "ORACLESLPATH"])
  benchmarks.path <<- unlist(config[1, "BECHMARKPATH"])
}

initrevs <- function() {
  sl.metaborg.rev <<- getgitrev(sl.metaborg.repo)
  sl.oracle.rev <<- getgitrev(sl.oracle.repo)
  dynsem.rev <<- getgitrev(dynsem.repo)
  graal.rev <<- gethgrev(graal.repo)
}

loadbenchmarks <- function() {
  return(read.csv(benchmarks.file))
}

loadmeasurements <- function() {
  return(read.csv(measurements.file))
}

writemeasurements <- function(measurements) {
  write.table(measurements, file=measurements.file, quote=F, append=F, row.names=F, col.names=T,  sep=",")
}

rmdatafiles <- function(measurements) {
  apply(measurements, 1, function(row) { rmfile(paste(row["GRAALDATA"])); rmfile(paste(row["JDKDATA"])) })
}

truncatedata <- function(measurements) {
  measurements["GRAALDATA"] = ""
  measurements["JDKDATA"] = ""
  return(measurements)
}

getvariantpath <- function(variant) {
  if(variant == "Oracle") {
    return(sl.oracle.path)
  } else if(variant == "DynSem") {
    return(sl.metaborg.path)
  }
}
