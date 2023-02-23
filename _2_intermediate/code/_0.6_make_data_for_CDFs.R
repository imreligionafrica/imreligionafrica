rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

setwd(outdir)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

pop <- fread('pop_world.csv')

pop <- pop[iso %in% ISO & year == 1980]
pop[, shrpop := pop / sum(pop)]
pop <- pop[, c('iso', 'shrpop')]

for (iiso in 1:21) {
  
  isoc <- ISO[iiso]
  
  print(isoc)
  
  setwd(indir_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'age', 'ysc')]
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'major_religion')]
  
  dt <- merge(rel, edu, by = c('year', 'serial', 'pernum'))
  dt <- dt[age >= 25]

  rm(edu, rel)
  
  dt[, bd := floor((year-age)/10)  * 10]
  dt[, c('year', 'serial', 'pernum', 'age') := NULL]
  dt <- dt[!is.na(bd) & !is.na(major_religion) &  !is.na(ysc)]
  dt <- dt[bd >= 1950 & bd <= 1980]
  
  dt <- dt[, .(n = .N), by = c('bd', 'major_religion', 'ysc')]
  dt[, iso := isoc]
  
  if (iiso == 1) {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
}

rm(dt)


setcolorder(DT, 'iso')
DT <- DT[order(iso, bd, major_religion, ysc)]

DT[, nobsc := sum(n), by = iso]
DT[, shrobs := nobsc / sum(n)]
DT[, nobsc := NULL]
DT <- merge(DT, pop, by='iso')
DT[, wt := shrpop / shrobs]
DT[, nwt := n * wt]
DT <- DT[, .(iso, bd, major_religion, ysc, n, nwt)]
DT <- DT[, .(n = sum(n), nwt = sum(nwt)), by=.(bd, major_religion, ysc)]

setwd(outdir)

fwrite(DT, '_data_for_CDFs.csv')



