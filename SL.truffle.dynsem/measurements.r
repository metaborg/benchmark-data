source("common.r")

runmeasurement <- function(measurementrow) {
  time <- Sys.time()

  timestamp.datepart <- format(time, "%Y%m%d")
  timestamp.timepart <- format(time, "%H%M%s")
  datafile.graal.rel <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.jdk.rel <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
  datafile.graal <- paste(getwd(), "/", datafile.graal.rel, sep="")
  datafile.jdk <- paste(getwd(), "/", datafile.jdk.rel, sep="")

  inputarg <- paste("\"", benchmarks.path,"/", measurementrow["BENCHMARK"], "\"", sep="")
  graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
  jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")

  runres <- system2("./runner.sh", args=c(paste(getvariantpath(measurementrow["VARIANT"])), inputarg, graaloutarg, jdkoutarg))

  quitonfail(runres, paste("Error running",measurementrow["BENCHMARK"],"on language", measurementrow["VARIANT"]))
  
  measurementrow["GRAALDATA"] <- datafile.graal.rel
  measurementrow["JDKDATA"] <- datafile.jdk.rel

  return(measurementrow)
}

# runbenchmark <- function(benchmark, variant, variantrev, variantpath) {
#   print(paste("Running",benchmark[1], "from path", benchmark[2], "on language", variant))
#   time <- Sys.time()
#
#   timestamp.datepart <- format(time, "%Y%m%d")
#   timestamp.timepart <- format(time, "%H%M%s")
#
#   datafile.graal.rel <- paste("data/data_graal_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
#   datafile.jdk.rel <- paste("data/data_jdk_", timestamp.datepart, "_", timestamp.timepart, ".csv", sep="")
#
#   datafile.graal <- paste(getwd(), "/", datafile.graal.rel, sep="")
#   datafile.jdk <- paste(getwd(), "/", datafile.jdk.rel, sep="")
#
#   inputarg <- paste("\"", benchmark[2],"/", benchmark[1], "\"", sep="")
#   graaloutarg <- paste("\"", datafile.graal, "\"", sep="")
#   jdkoutarg <- paste("\"", datafile.jdk, "\"", sep="")
#
#   runres <- system2("./runner.sh", args=c(paste(variantpath), inputarg, graaloutarg, jdkoutarg))
#
#   if(runres != 0) {
#     print(paste("Error running",benchmark[1],"on language", variant))
#     quit("no", status=runres)
#   }
#
#   newrow <- data.frame(variant, variantrev, dynsem.rev, graal.rev, paste(benchmark[1]), datafile.graal.rel, datafile.jdk.rel)
#   write.table(newrow, file=measurements.file, quote=FALSE, append=T, row.names=F, col.names=F,  sep=",", na="42", )
#
# }

source("common.r")

aggregatedata <- function() {
  measurements <- read.csv(measurements.file)

  dataset <- data.frame(
    "RUNTYPE"=character(0),
    "STARTTIME"=integer(0),
    "ENDTIME"=integer(0),
    "DURATION"=integer(0),
    "SUCCESS"=integer(0),
    "FAILREASON"=character(0),
    "VARIANT"=character(0),
    "JVM"=character(0),
    "BENCHMARK"=character(0)
  )

  for(i in seq(1, nrow(measurements))) {
    m <- measurements[i,]
    # variantkey <- paste(unlist(m["VARIANT"]), unlist(m["VARIANTREV"]), unlist(m["DSREV"]), unlist(m["GRAALREV"]), sep="-")

    variantkey <- paste(unlist(m["VARIANT"]), unlist(m["VARIANTREV"]), unlist(m["DSREV"]), sep=".")
    benchmarkkey <- paste(unlist(m["BENCHMARK"]))
    graaldata <- read.csv(file=paste(unlist(m["GRAALDATA"])))
    jdkdata <- read.csv(file=paste(unlist(m["JDKDATA"])))

    graaldata["VARIANT"] <- rep(variantkey, nrow(graaldata))
    graaldata["JVM"] <- rep("GRAAL", nrow(graaldata))
    graaldata["BENCHMARK"] <- rep(benchmarkkey, nrow(graaldata))

    jdkdata["VARIANT"] <- rep(variantkey, nrow(jdkdata))
    jdkdata["JVM"] <- rep("JDK", nrow(jdkdata))
    jdkdata["BENCHMARK"] <- rep(benchmarkkey, nrow(jdkdata))

    graaldata["DURATION"] <- graaldata["DURATION"] / 1000000
    jdkdata["DURATION"] <- jdkdata["DURATION"] / 1000000
    dataset <- rbind(dataset, graaldata, jdkdata)
  }
  return(dataset)
}

createplots <- function(dataset, column, size=4, warmruns=TRUE) {
  types <- unique(dataset[column])
  types <- types[with(types, order(types)), ]
  ntypes <- length(types)
  nparts <- ceiling(ntypes / size)
  plots <- list()
  for(i in c(0:(nparts - 1))) {
    sidx <- i * size + 1
    eidx <- min((i+1) * size, ntypes)
    colvals <- types[c(sidx:eidx)]
    subset <- dataset[which(dataset[,column] %in% colvals), ]
    plot <- createplot(subset, warmruns)
    plots <- list(plots, plot)
  }
  return(plots)
}

createplot <- function(dataset, warmruns=TRUE) {
  require(ggplot2)
  if(warmruns) {
    runtype = "REPETITION"
  } else {
    runtype = "WARMUP"
  }
  subset <- dataset[dataset$RUNTYPE == runtype,]

  plot <- ggplot(data = subset, aes(x=JVM, y=DURATION))
  plot <- plot + geom_boxplot(aes(fill=VARIANT))
  plot <- plot + facet_wrap( ~ BENCHMARK, scales="free")
  plot <- plot + xlab("VM Type") + ylab("Time (ms)")
  plot <- plot + guides(fill=guide_legend(title=NULL))
  plot <- plot + theme(legend.position="top")

  if(warmruns){
    plot <- plot + ggtitle("Warm runs")
  }else {
    plot <- plot + ggtitle("Cold runs")
  }

  return(plot)
}

saveplot <- function(plot, filename="plot") {
  pdf(paste(plots.dir, "/", filename, ".pdf", sep=""))
  print(plot)
  dev.off()
}

runplots <- function() {
  data <- aggregatedata()
  plots.warm <- createplots(data, column="BENCHMARK", warmruns=TRUE)
  plots.cold <- createplots(data, column="BENCHMARK", warmruns=FALSE)
  saveplot(plots.warm, filename="plot_warm")
  saveplot(plots.cold, filename="plot_cold")
}

createbaseline <- function() {
  # run Oracle benchmarks
  apply(benchmarks, 1, runbenchmark, "Oracle", sl.oracle.rev, sl.oracle.path)
}

createmeasurement <- function() {
  apply(benchmarks, 1, runbenchmark, "DynSem", sl.metaborg.rev, sl.metaborg.path)
}
