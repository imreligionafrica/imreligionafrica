rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE) 


ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

iiso <- 1

for (iiso in 1:21) {
  
  isoc <- ISO[iiso]
  print(sprintf('#################### %s ####################', isoc))
  
  setwd(indir_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))
  setwd(indir_bas)
  bas <- data.table(readRDS(sprintf('basedat_%s.rds', isoc)))
  
  dt <- merge(edu[, c('year', 'serial', 'pernum', 'age', 'ysc', 'eckid', 'ec0')],
              rel[, c('year', 'serial', 'pernum', 'major_religion')],
              by=c('year', 'serial', 'pernum'))
  dt <- merge(dt, bas[, c('year', 'serial', 'pernum', 'male')])
  rm(edu, rel, bas)
  dt <- dt[!is.na(major_religion) & age >= 14 & age <= 18 & !is.na(eckid) & !is.na(ec0) & !is.na(male)]
  dt[, byr := year-age]
  for (i in seq(1870, 2010, by = 10)) {
    ipl9 = i+9
    dt[byr >= i & byr <= ipl9, bch10 := i]
  }
  imupg <- dt[ec0 == 0, .(imup_1418g = mean(eckid >= 1, na.rm = TRUE),
                          n_imup_1418g = .N), by = c('bch10', 'major_religion', 'male')]
  imdng <- dt[ec0 >= 1, .(imdn_1418g = mean(eckid == 0, na.rm = TRUE),
                          n_imdn_1418g = .N), by = c('bch10', 'major_religion', 'male')]
  imupc <- dt[ec0 == 0, .(imup_1418c = mean(eckid >= 1, na.rm = TRUE),
                          n_imup_1418c = .N), by = c('bch10', 'male')]
  imdnc <- dt[ec0 >= 1, .(imdn_1418c = mean(eckid == 0, na.rm = TRUE),
                          n_imdn_1418c = .N), by = c('bch10', 'male')]
  avgeg <- dt[, .(myscg = mean(ysc), mecg = mean(eckid)), by = c('bch10', 'major_religion', 'male')]
  avgec <- dt[, .(myscc = mean(ysc), mecc = mean(eckid)), by = c('bch10', 'male')]
  
  
  out <- merge(imupg, imdng, by=c('bch10', 'major_religion', 'male'), all=TRUE)
  out <- merge(out, imupc, by=c('bch10', 'male'), all=TRUE)
  out <- merge(out, imdnc, by=c('bch10', 'male'), all=TRUE)
  out <- merge(out, avgeg, by=c('bch10', 'major_religion', 'male'), all=TRUE)
  out <- merge(out, avgec, by=c('bch10', 'male'), all=TRUE)
  
  out[, iso:=isoc]
  setcolorder(out, c('iso'))
  
  if (iiso==1) {
    OUT <- out
  } else {
    OUT <- rbind(OUT, out)
  }
  
}

setwd(outpath)
fwrite(OUT, 'imupdown_1418_by_bchmajrel_gender.csv')