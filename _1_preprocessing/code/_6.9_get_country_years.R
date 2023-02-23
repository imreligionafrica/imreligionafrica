rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
outpath <- file.path(rootdir, "_1_preprocessing", "code")

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

setwd(inpath)

for (isoc in ISO) {
  
  print(isoc)
  
  dt <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  dt <- unique(dt, by=c('year'))
  dt <- dt[, c('year')]
  dt[, iso:=isoc]
  setcolorder(dt, 'iso')
  
  if (isoc == "BEN") {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
}

setwd(outpath)

DT <- DT[order(iso, year)]
DT[, new := 1]

DT_old <- fread('country_years_old.csv')
DT_old[, old := 1]

DT <- merge(DT, DT_old, by=c('iso', 'year'), all=TRUE)

fwrite(DT, 'country_years.csv')

