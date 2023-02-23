rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

for (iiso in 1:21) {
  
  iso <- ISO[iiso]
  
  print(sprintf('###################### %s ######################', iso))
  
  setwd(indir_edu)
  dt <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  dt <- dt[, c('year', 'serial', 'pernum', 'age', 'eckid', 'ysc')]
  dt[, lit := as.integer(eckid > 0)]
  dt[is.na(eckid), lit:= NA]
  dt[, eckid :=NULL]
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', iso)))
  dt <- merge(dt, rel, by=c('year', 'serial', 'pernum'), all=TRUE)
  
  dt <- dt[!is.na(major_religion) & major_religion != '' & (!is.na(lit) | !is.na(ysc))]
  
  dt[, byr := year-age]
  dt[, bd := 10 * (byr %/% 10)]

  ymr <- dt[, c('bd', 'age', 'major_religion', 'ysc')][
    !is.na(bd) & age>=25, .(ysc25 = mean(ysc, na.rm=TRUE)), by=.(bd, major_religion)]
  
  lmr <- dt[, c('bd', 'age', 'major_religion', 'lit')][
    !is.na(bd) & age>=14, .(lit14 = mean(lit, na.rm=TRUE)), by=.(bd, major_religion)]
  
  
  mr <- merge(ymr, lmr, by=c('bd', 'major_religion'), all=TRUE)

  mr[, iso:= ISO[iiso]]
  setcolorder(mr, 'iso')

  if (iiso == 1) {
    MR <- mr
  } else {
    MR <- rbind(MR, mr)
  }
  
}

setwd(outpath)
MR <- MR[order(iso, bd)]
fwrite(MR, '_mean_ysc_ec_by_bd_major_religion.csv')

