rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_pw <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MWI", "NGA", "RWA", "SEN",
         "SLE", "TGO", "UGA", "ZAF", "ZMB",
         "MUS")


for (isoc in ISO) {
  
  print(isoc)
  
  setwd(indir_mig)
  m <- data.table(readRDS(sprintf("migdists_%s.rds", isoc)))
  setwd(indir_edu)
  e <- data.table(readRDS(sprintf("aey_old_mompop_unc_%s.rds", isoc)))
  setwd(indir_rel)
  r <- data.table(readRDS(sprintf("religions_%s.rds", isoc)))  
  setwd(indir_pw)
  p <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))

  if ('migdistr_bplmatch' %in% names(m)) {
    
    m <- m[, .(year, serial, pernum, migdistr, migdistr_bplmatch)]
    e <- e[, .(year, serial, pernum, age, eckid, ec0)]
    r <- r[, .(year, serial, pernum, major_religion)]
    
    e[, bd := floor((year-age)/10)  * 10]
    e[!is.na(eckid), lit := as.integer(eckid>=1)]
    e[!is.na(ec0), lit0 := as.integer(ec0>=1)]
    e <- e[, .(year, serial, pernum, age, bd, lit, lit0)]
    
    m[, migrant := as.integer(migdistr != migdistr_bplmatch)]
    m[is.na(migdistr) | is.na(migdistr_bplmatch), migrant := NA]
    m[, `:=`(org = migdistr_bplmatch, dst = migdistr)][, c('migdistr', 'migdistr_bplmatch'):=NULL]
    m <- m[, .(year, serial, pernum, org, dst, migrant)]
    
    dt <- merge(p, e, by=c('year', 'serial', 'pernum'))
    dt <- merge(dt, r, by=c('year', 'serial', 'pernum'))
    dt <- merge(dt, m, by=c('year', 'serial', 'pernum'))
    rm(m, e, r, p)
    
    xall <- dt[!is.na(major_religion) & major_religion != "" & !is.na(bd) & !is.na(migrant),
               .(w = sum(perwt)),
               by=c('year', 'bd', 'age', 'major_religion', 'org', 'migrant')][
                 order(year, bd, age, major_religion, org, migrant)
               ]
    
    xed <- dt[!is.na(major_religion) & major_religion != "" & !is.na(bd) & !is.na(migrant) & !is.na(lit) & !is.na(lit0),
              .(w = sum(perwt)),
              by=c('year', 'bd', 'age', 'major_religion', 'org',  'lit0',  'lit', 'migrant')][
                order(year, bd, age, major_religion, org, lit0, lit, migrant)
              ]
    
    xall[, iso:= isoc]
    xed[, iso:= isoc]
    
    setcolorder(xall, 'iso')
    setcolorder(xed, 'iso')
    
    if (isoc == 'BEN') {
      ALL <- xall
      ED <- xed
    } else {
      ALL <- rbind(ALL, xall)
      ED <- rbind(ED, xed) 
    }
    
  }
  

}

setwd(outdir)

fwrite(ALL, 'migrant_stock_data_all.csv')
fwrite(ED, 'migrant_stock_data_ed.csv')


