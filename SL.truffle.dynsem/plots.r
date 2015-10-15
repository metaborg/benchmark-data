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

  plot <- ggplot(data = dataset[dataset$RUNTYPE == runtype,], aes(x=JVM, y=DURATION))
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
