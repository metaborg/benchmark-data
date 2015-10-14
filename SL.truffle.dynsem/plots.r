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

  if(warmruns){
    plot <- plot + ggtitle("Warm runs")
  }else {
    plot <- plot + ggtitle("Cold runs")
  }

  return(plot)
}

saveplots <- function () {
  data <- aggregatedata()
  plot.warm <- createplot(data, TRUE)
  plot.cold <- createplot(data, FALSE)
  pdf(paste(plots.dir, "/plot_warm.pdf", sep=""))
  print(plot.warm)
  dev.off()

  pdf(paste(plots.dir, "/plot_cold.pdf", sep=""))
  print(plot.cold)
  dev.off()
}
