run <- function(lib){
  .libPaths(c(lib,.libPaths())); suppressMessages(library(smatr))
  set.seed(11)
  mk <- function(n,e) data.frame(BodyMassToUse=10^rnorm(n,1,.3),
                                 AvgAll=10^rnorm(n,1,.3), Element=e)
  full <- do.call(rbind, lapply(c("aEmpty","femur","tibia","humerus"),
                                function(e) mk(8,e)))
  full$Element <- factor(full$Element)
  # 'ElementSubgroup': drop one level's rows but keep it as a factor level
  sub <- full[full$Element != "aEmpty", ]      # 'aEmpty' now empty but still a level
  cat("  levels carried in subset:", paste(levels(sub$Element),collapse=","), "\n")
  tryCatch({
    s <- sma(BodyMassToUse ~ AvgAll * Element, log="xy", data=sub,
             method="SMA", multcomp=TRUE, multcompmethod="adjust")
    cat("  multcomp=TRUE -> OK, pairwise rows =", nrow(s$multcompresult), "\n")
  }, error=function(e) cat("  multcomp=TRUE -> ERROR:", conditionMessage(e), "\n"))
}
cat("BASELINE:\n"); run("/tmp/lib_base")
