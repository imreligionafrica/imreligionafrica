rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_eth <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "eth")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

ISOE <- c("BEN", "BFA", "BWA", "ETH",
          "GHA", "GIN", "LBR", "MAR", 
          "MLI", "MOZ", "MUS", "MWI", 
          "NGA", "RWA", "SEN", "SLE",
          "TGO", "UGA", "ZAF", "ZMB")

for (isoc in ISO) {
  
  print(sprintf('%s: loading data', isoc))
  
  setwd(indir_dis)
  dist <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  dist <- dist[!is.na(district)]
  dist <- unique(dist, by = c('year', 'district'))
  dist <- dist[, (ndist = .N), by = year]
  names(dist) <- c('year', 'ndist')
  
  if (isoc %in% ISOE ) {
    setwd(indir_eth)
    eth <- data.table(readRDS(sprintf('ethnicities_%s.rds', isoc)))
    eth <- eth[!is.na(eth_hrm)]
    eth <- unique(eth, by = c('year', 'eth_hrm'))
    eth <- eth[, (neth = .N), by = year]
    names(eth) <- c('year', 'neth')
    dt <- merge(dist, eth, by = 'year', all.x=TRUE, all.y=TRUE)
  } else {
    dt <- dist
    dt[, neth := NA]
    dt[, neth := as.integer(neth)]
  }
  
  dt[, iso:= isoc]
  setcolorder(dt, 'iso')
  
  if (isoc == 'BEN') {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
}

setwd(outdir)

fwrite(DT, '_ndist_eth.csv')