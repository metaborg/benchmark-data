
# Configuration file
config <- read.csv("configuration.csv")

sl.metaborg.repo = unlist(config[1, "METABORGSLREPO"])
sl.oracle.repo = unlist(config[1, "ORACLESLREPO"])
dynsem.repo = unlist(config[1, "DYNSEMREPO"])
graal.repo = unlist(config[1, "GRAALREPO"])

sl.metaborg.path = unlist(config[1, "METABORGSLPATH"])
sl.oracle.path = unlist(config[1, "ORACLESLPATH"])

getgitrev <- function(path) {
  gitdir <- paste("--git-dir=", path, sep="")
  rev = system2("git", args=c(gitdir, "rev-parse", "--short", "HEAD"), stdout=TRUE)
  return(rev)
}

gethgrev <- function(path) {
  rev = system2("hg", args=c("-R", paste(path), "id", "-i"), stdout=TRUE)
  return(rev)
}

sl.metaborg.rev <- getgitrev(sl.metaborg.repo)
sl.oracle.rev <- getgitrev(sl.oracle.repo)
dynsem.rev <- getgitrev(dynsem.repo)
graal.rev <- gethgrev(graal.repo)

# Benchmarks file
benchmarks <- read.csv("benchmarks.csv")

# Measurements file
measurements.file <- "measurements.csv"
measurements <- read.csv(measurements.file)

# Plots dir
plots.dir <- "plots"
