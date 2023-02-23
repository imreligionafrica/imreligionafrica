rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_pwt <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

setwd(outdir)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "UGA", "ZAF", "ZMB",
         "TGO")

iiso <- 1

for (iiso in 1:21) {
  
  isoc <- ISO[iiso]
  
  print(isoc)
  
  setwd(indir_edu)
  e <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'age')]
  setwd(indir_dis)
  d <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'district')]
  setwd(indir_rel)
  r <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'major_religion')]
  setwd(indir_pwt)
  w <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))
  
  dt <- merge(w, r, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, e, by = c('year', 'serial', 'pernum'))
  dt <- merge(dt, d, by = c('year', 'serial', 'pernum'))
  
  rm(r, w, e, d)
  
  dt[, bd := floor((year-age)/10)  * 10]
  dt[, c('year', 'serial', 'pernum', 'age') := NULL]
  dt <- dt[!is.na(bd) & !is.na(major_religion) & !is.na(district)]
  dt <- dt[, .(n = .N, w = sum(perwt)), by = c('bd', 'major_religion', 'district')]
  dt[, iso := isoc]
  dt <- dt[, c('iso', 'district', 'bd', 'major_religion', 'n', 'w')]
  dt[, `:=`(sn = n/sum(n), sw = w/sum(w)), by=c('bd', 'district')]
  
  if (iiso == 1) {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
}

rm(dt)

setwd(outdir)

fwrite(DT, '_pop_relshares_by_district_bd.csv')



