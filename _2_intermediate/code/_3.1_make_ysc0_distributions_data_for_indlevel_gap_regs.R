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

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "UGA", "ZAF", "ZMB",
         "TGO")

for (isoc in ISO) {
  
  print(sprintf("%s", isoc))
  
  setwd(indir_edu)
  e <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  setwd(indir_dis)
  d <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  setwd(indir_rel)
  r <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))
  setwd(indir_pwt)
  w <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))
  
  e <- e[, c('year', 'serial', 'pernum', 'age', 'ysc0')]
  e[, ysc0 := round(ysc0)]
  d <- d[, c('year', 'serial', 'pernum', 'district')]
  r <- r[, c('year', 'serial', 'pernum', 'major_religion')]
  r <- r[major_religion %in% c('Christian', 'Muslim', 'Traditional')]
  
  e[, bd:= 10 * floor((year-age)/10)]
  # e <- e[bd==1980 & !is.na(ysc0)]
  e <- e[!is.na(ysc0)]
  
  dt <- merge(e, d, by=c('year', 'serial', 'pernum'), all=FALSE)
  dt <- merge(dt, r, by=c('year', 'serial', 'pernum'), all=FALSE)
  dt <- merge(dt, w, by=c('year', 'serial', 'pernum'), all=FALSE)
  
  rm(e, d, r, w)
  
  dt1418 <- dt[age>=14 & age<=18]
  dt[, c('year', 'serial', 'pernum', 'age') := NULL]
  dt1418[, c('year', 'serial', 'pernum', 'age') := NULL]
  dt <- dt[, .(wy0 = sum(perwt)), by=.(district, ysc0, bd, major_religion)]
  dt1418 <- dt1418[, .(wy01418 = sum(perwt)), by=.(district, ysc0, bd, major_religion)]
  dt <- merge(dt, dt1418, by=c('district', 'ysc0', 'bd', 'major_religion'), all=TRUE)[order(district, major_religion, ysc0, bd)]
  dt[, iso := isoc]
  setcolorder(dt, 'iso')
  
  if (isoc=="BEN") {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }

}

setwd(outdir)

fwrite(DT, 'ysc0_distributions_by_district_bd.csv')









