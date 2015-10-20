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
  dynsem.repo <<- unlist(config[1, "DYNSEMREPO"])
  graal.repo <<- unlist(config[1, "GRAALREPO"])
  sl.metaborg.path <<- unlist(config[1, "METABORGSLPATH"])
  sl.oracle.path <<- unlist(config[1, "ORACLESLPATH"])
  benchmarks.path <<- unlist(config[1, "BECHMARKPATH"])
}

getvariantpath <- function(variant) {
  if(variant == "Oracle") {
    return(sl.oracle.path)
  } else if(variant == "DynSem") {
    return(sl.metaborg.path)
  }
}

initrevs <- function() {
  sl.metaborg.rev <<- getgitrev(sl.metaborg.repo)
  sl.oracle.rev <<- getgitrev(sl.oracle.repo)
  dynsem.rev <<- getgitrev(dynsem.repo)
  graal.rev <<- gethgrev(graal.repo)
}

loadbenchmarks <- function() {
  benchmarks <<- read.csv(benchmarks.file)
}

loadmeasurements <- function() {
  measurements <<- read.csv(measurements.file)
}

writemeasurements <- function() {
  write.table(measurements, file=measurements.file, quote=F, append=F, row.names=F, col.names=T,  sep=",")
}

truncatedataall <- function(measurements) {
  return(apply(measurements, 1, truncatedatarow))
}

truncatedatarow <- function(row) {
  graaldata <- row["GRAALDATA"]
  jdkdata <- row["JDKDATA"]
  rmfile(jdkdata)
  rmfile(graaldata)

  row["GRAALDATA"] <- ""
  row["JDKDATA"] <- ""
  return(row)
}
