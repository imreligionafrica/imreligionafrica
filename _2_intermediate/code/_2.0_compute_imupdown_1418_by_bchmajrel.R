rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE) 

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MWI", "NGA", "RWA", "SEN",
         "SLE", "UGA", "ZAF", "ZMB", "TGO","MUS")

setwd(outpath)
master<-fread('religions__mastertab.csv')


for (iiso in 1:21) {
  
  isoc <- ISO[iiso]
  print(sprintf('#################### %s ####################', isoc))
  setwd(indir_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))
  
  dt <- merge(edu[, c('year', 'serial', 'pernum', 'age', 'ysc', 'eckid', 'ec0')],
              rel[, c('year', 'serial', 'pernum', 'major_religion')],
              by=c('year', 'serial', 'pernum'))
  rm(edu, rel)
  
  dt <- dt[!is.na(major_religion) & major_religion!='' & age >= 14 & age <= 18 & !is.na(eckid) & !is.na(ec0)]

  dt[,bch10:=floor((year-age)/10)*10]
  
  imupg <- dt[ec0 == 0, .(imup_1418g = mean(eckid >= 1, na.rm = TRUE),
                          n_imup_1418g = .N), by = c('bch10', 'major_religion')]
  imdng <- dt[ec0 >= 1, .(imdn_1418g = mean(eckid == 0, na.rm = TRUE),
                          n_imdn_1418g = .N), by = c('bch10', 'major_religion')]
  imupc <- dt[ec0 == 0, .(imup_1418c = mean(eckid >= 1, na.rm = TRUE),
                          n_imup_1418c = .N), by = c('bch10')]
  imdnc <- dt[ec0 >= 1, .(imdn_1418c = mean(eckid == 0, na.rm = TRUE),
                          n_imdn_1418c = .N), by = c('bch10')]
  avgeg <- dt[, .(myscg = mean(ysc), mecg = mean(eckid)), by = c('bch10', 'major_religion')]
  avgec <- dt[, .(myscc = mean(ysc), mecc = mean(eckid)), by = c('bch10')]
  

  out <- merge(imupg, imdng, by=c('bch10', 'major_religion'), all=TRUE)
  out <- merge(out, imupc, by=c('bch10'), all=TRUE)
  out <- merge(out, imdnc, by=c('bch10'), all=TRUE)
  out <- merge(out, avgeg, by=c('bch10', 'major_religion'), all=TRUE)
  out <- merge(out, avgec, by=c('bch10'), all=TRUE)

  masteri <- unique(master[iso==isoc], by='major_religion')[,.(major_religion,shre)]
  out <- merge(out, masteri, by=c('major_religion'), all=TRUE)
  out[, iso:=isoc]
  setcolorder(out, c('iso'))
  
  if (iiso==1) {
    OUT <- out
  } else {
    OUT <- rbind(OUT, out)
  }

}

setwd(outpath)
fwrite(OUT, 'imupdown_1418_by_bchmajor_religion.csv')