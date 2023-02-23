rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "edu")
outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized","perwt")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

for (isoc in ISO) {
  print(isoc)
  setwd(inpath)
  dt <- data.table(haven::read_dta(sprintf("%s_edu.dta", isoc)))
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  dt <- dt[, .(year, serial, pernum, perwt)]
  setwd(outpath)
  saveRDS(dt, sprintf('perwt_%s.rds', isoc))
}