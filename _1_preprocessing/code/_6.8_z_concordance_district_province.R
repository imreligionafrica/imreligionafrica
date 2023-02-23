rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

code_dir <- file.path(rootdir, "_1_preprocessing", "code")

dist_dir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")

prov_dir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")

ISO <- c("BEN", "BWA", "BFA", "CMR", "EGY", 
         "ETH", "GHA", "GIN", "KEN", "LSO", 
         "LBR", "MWI", "MLI", "MUS", "MAR", 
         "MOZ", "NGA", "RWA", "SEN", "SLE", 
         "ZAF", "SSD", "SDN", "TZA", "TGO", 
         "UGA", "ZMB", "ZWE")

for (isoc in ISO) {
  
  print(isoc)
  setwd(dist_dir)
  dist <- data.table(readRDS(sprintf("districts_%s.rds", isoc)))
  setwd(prov_dir)
  prov <- data.table(readRDS(sprintf("provinces_%s.rds", isoc)))
  
  dt <- merge(dist, prov, by=c('year', 'serial', 'pernum'))
  dt <- unique(dt[ ,.(district, province)], by='district')
  dt[, iso:=isoc]
  setcolorder(dt, 'iso')
  
  if (isoc=='BEN') {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
  
}

setwd(code_dir)

fwrite(DT, 'conc_prov_dist.csv')
