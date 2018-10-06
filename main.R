rm(list = ls())
setwd("~/Teknisk Fysik/AI/Project-3-Diagnostics")
#setwd("~/Teknisk Fysik/AI/Project-3-Diagnostics")
library(Diagnostics)
source("diagnose.R")
source("learn.R")
runDiagnostics(learn, diagnose, verbose = 2)










