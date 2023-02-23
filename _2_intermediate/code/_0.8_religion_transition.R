rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_or <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "old_rel")
indir_ed <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_pw <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "UGA", "ZAF", "ZMB",
         "TGO")

for (isoc in ISO) {
  
  print(isoc)
  
  setwd(indir_ed)
  ed <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))
  setwd(indir_or)
  or <- data.table(readRDS(sprintf('old_rel_%s.rds', isoc)))
  ed <- ed[, .(year, serial, pernum, age, eckid, ec0, ecmom, ecpop)]
  setwd(indir_pw)
  pw <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))
  
  dt <- merge(or, ed, by=c('year', 'serial', 'pernum'))
  dt <- merge(dt, pw, by=c('year', 'serial', 'pernum'))
  
  rm(or, ed, pw)
  
  dt[, bd := floor((year-age)/10)  * 10]
  dt[, c('serial', 'pernum') := NULL]
  
  x0all <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion0),
              .(w0 = sum(perwt)),
              by=c('bd', 'year', 'major_religion', 'major_religion0', 'age')][
                order(bd, year, major_religion, major_religion0, age)
              ]
  
  xmomall <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion_mom),
              .(wmom = sum(perwt)),
              by=c('bd', 'year', 'major_religion', 'major_religion_mom', 'age')][
                order(bd, year, major_religion, major_religion_mom, age)
              ]
  
  xpopall <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion_pop),
                .(wpop = sum(perwt)),
                by=c('bd', 'year', 'major_religion', 'major_religion_pop', 'age')][
                  order(bd, year, major_religion, major_religion_pop, age)
                ]
  
  x0ed <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion0) & !is.na(eckid) & !is.na(ec0),
              .(w0 = sum(perwt)),
              by=c('bd', 'year', 'major_religion', 'major_religion0', 'age', 'ec0', 'eckid')][
                order(bd, year, major_religion, major_religion0, age, ec0, eckid)
              ]
  
  xmomed <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion_mom) & !is.na(eckid) & !is.na(ecmom),
                .(wmom = sum(perwt)),
                by=c('bd', 'year', 'major_religion', 'major_religion_mom', 'age', 'ecmom', 'eckid')][
                  order(bd, year, major_religion, major_religion_mom, age, ecmom, eckid)
                ]
  
  xpoped <- dt[!is.na(major_religion) & !is.na(bd) & !is.na(major_religion_pop) & !is.na(eckid) & !is.na(ecpop),
                .(wpop = sum(perwt)),
                by=c('bd', 'year', 'major_religion', 'major_religion_pop', 'age', 'ecpop', 'eckid')][
                  order(bd, year, major_religion, major_religion_pop, age, ecpop, eckid)
                ]
  
  names(xmomall) <- c(names(x0all)[1:5], 'wmom')
  names(xpopall) <- c(names(x0all)[1:5], 'wpop')
  names(xmomed) <- c(names(x0ed)[1:7], 'wmom')
  names(xpoped) <- c(names(x0ed)[1:7], 'wpop')
  
  all <- merge(x0all, xmomall, by=c('bd', 'year', 'major_religion', 'major_religion0', 'age'), all=TRUE)
  all <- merge(all, xpopall, by=c('bd', 'year', 'major_religion', 'major_religion0', 'age'), all=TRUE)
  
  
  ed <- merge(x0ed, xmomed, by=c('bd', 'year', 'major_religion', 'major_religion0', 'age', 'ec0', 'eckid'), all=TRUE)
  ed <- merge(ed, xpoped, by=c('bd', 'year', 'major_religion', 'major_religion0', 'age', 'ec0', 'eckid'), all=TRUE)
  
  all[, iso:=isoc]
  ed[, iso:=isoc]
  
  all[is.na(all)] = 0
  ed[is.na(ed)] = 0
  
  setcolorder(all, 'iso')
  setcolorder(ed, 'iso')

  if (isoc == "BEN") {
    ALL <- all
    ED <- ed
  } else {
    ALL <- rbind(ALL, all)
    ED <- rbind(ED, ed)
  }
  
}

setwd(outdir)

fwrite(ALL, 'religion_transition_data_all.csv')
fwrite(ED, 'religion_transition_data_ed.csv')