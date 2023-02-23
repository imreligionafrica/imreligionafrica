rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
inpath_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
inpath_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
inpath_eth <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "eth")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "ETH",
         "GHA", "LBR"       , "MLI",
         "MOZ", "MWI", "SEN", "SLE",
         "TGO", "UGA", "ZAF", "ZMB")

for (isoc in ISO) {
  
  print(sprintf('%s: loading data', isoc))
  
  setwd(inpath_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))[, .(year, serial, pernum, age)]
  setwd(inpath_eth)
  eth <- data.table(readRDS(sprintf('ethnicities_%s.rds', isoc)))
  setwd(inpath_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))[, .(year, serial, pernum, major_religion)]
  dt <- merge(eth, rel, by=c('year', 'serial', 'pernum'))
  dt <- merge(edu, dt, by=c('year', 'serial', 'pernum'))
  rm(edu, eth, rel)
  dt <- dt[!is.na(eth_hrm) & 
           !is.na(major_religion) & 
           major_religion != "" &
           !is.na(age), .(year, age, eth_hrm, major_religion)]
  dt[, byr := year-age]
  dt[, bd := 10*(byr %/% 10)]
  dt <- dt[, .(nobs = .N), by =  .(bd, eth_hrm, major_religion)]
  dt <- dt[order(bd, eth_hrm, major_religion)]
  
  dt[, iso:= isoc]
  setcolorder(dt, 'iso')
  
  if (isoc == 'BEN') {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
}

setwd(outpath)

fwrite(DT, '_nobs_bd_eth_rel.csv')