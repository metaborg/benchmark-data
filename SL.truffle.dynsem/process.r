require(ggplot2)

oracle.graal.file <- "data/data_20151012_174807.csv"
oracle.jdk.file <- "data/data_20151012_180715.csv"
dynsem.graal.file <- "data/data_20151012_194445.csv"
dynsem.jdk.file <- "data/data_20151012_200057.csv"

oracle.graal.raw <- read.csv(oracle.graal.file)
oracle.jdk.raw <- read.csv(oracle.jdk.file)
dynsem.graal.raw <- read.csv(dynsem.graal.file)
dynsem.jdk.raw <- read.csv(dynsem.jdk.file)

oracle.graal.raw["DURATION"] <- oracle.graal.raw["DURATION"] / 1000000
oracle.jdk.raw["DURATION"] <- oracle.jdk.raw["DURATION"] / 1000000
dynsem.graal.raw["DURATION"] <- dynsem.graal.raw["DURATION"] / 1000000
dynsem.jdk.raw["DURATION"] <- dynsem.jdk.raw["DURATION"] / 1000000

oracle.graal.part <- data.frame(oracle.graal.raw["DURATION"], oracle.graal.raw["RUNTYPE"])
oracle.jdk.part <- data.frame(oracle.jdk.raw["DURATION"], oracle.jdk.raw["RUNTYPE"])
dynsem.graal.part <- data.frame(dynsem.graal.raw["DURATION"], dynsem.graal.raw["RUNTYPE"])
dynsem.jdk.part <- data.frame(dynsem.jdk.raw["DURATION"], dynsem.jdk.raw["RUNTYPE"])

oracle.graal.part$JVM <- rep("GRAAL", nrow(oracle.graal.part))
oracle.jdk.part$JVM <- rep("JDK", nrow(oracle.jdk.part))
dynsem.graal.part$JVM <- rep("GRAAL", nrow(dynsem.graal.part))
dynsem.jdk.part$JVM <- rep("JDK", nrow(dynsem.jdk.part))

oracle.graal.part$VARIANT <- rep("ORACLE", nrow(oracle.graal.part))
oracle.jdk.part$VARIANT <- rep("ORACLE", nrow(oracle.jdk.part))
dynsem.graal.part$VARIANT <- rep("DYNSEM", nrow(dynsem.graal.part))
dynsem.jdk.part$VARIANT <- rep("DYNSEM", nrow(dynsem.jdk.part))

cd <- rbind(oracle.graal.part, oracle.jdk.part, dynsem.graal.part, dynsem.jdk.part)

p.rep <- ggplot(data = cd[cd$RUNTYPE == "REPETITION",], aes(x=JVM, y=DURATION)) + geom_boxplot(aes(fill=VARIANT))
p.rep <- p.rep + facet_wrap( ~ JVM, scales="free")
p.rep <- p.rep + ggtitle("Binary-Trees")

pdf("plots/baseline_Binary-Trees_warm.pdf")
print(p.rep)
dev.off()

p.warm <- ggplot(data = cd[cd$RUNTYPE == "WARMUP",], aes(x=JVM, y=DURATION)) + geom_boxplot(aes(fill=VARIANT))
p.warm <- p.warm + facet_wrap( ~ JVM, scales="free")
p.warm <- p.warm + ggtitle("Binary-Trees")

pdf("plots/baseline_Binary-Trees_cold.pdf")
print(p.warm)
dev.off()
