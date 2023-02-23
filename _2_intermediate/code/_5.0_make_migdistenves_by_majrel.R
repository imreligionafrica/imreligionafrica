rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE) 

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY", 
         "ETH", "GHA", "GIN", "LBR", "MWI", 
         "MLI", "MUS", "MOZ", "RWA", "SEN", 
         "SLE", "TGO", "UGA", "ZAF", "ZMB")


niso <- length(ISO)

for (iiso in 1:niso) {

  iso <- ISO[iiso]
  
  print(sprintf('###################### %s ######################', iso))
  
  setwd(indir_edu)
  dat <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  dat <- dat[, c('year', 'serial', 'pernum', 'age', 'eckid', 'ec0')]
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', iso)))
  rel <- rel[, c('year', 'serial', 'pernum', 'major_religion')]
  dat <- merge(dat, rel, by=c('year', 'serial', 'pernum'), all=TRUE)
  rm(rel)
  
  dat <- dat[!is.na(age)]
  dat <- dat[!is.na(eckid)]
  dat <- dat[!is.na(ec0)]
  dat <- dat[age >= 14]
  
  dat$ctry <- iiso
  
  setcolorder(dat, c('ctry', 'year', 'serial', 'pernum'))
  
  setwd(indir_mig)
  migdists <- data.table(readRDS(sprintf('migdists_%s.rds', iso)))
  
  DT <- merge(dat, migdists, by = c('year', 'serial', 'pernum'))
  DT[,c('serial', 'pernum'):=NULL]
  
  rm(dat, migdists)
  
  DT <- DT[!is.na(migdistr) & migdistr == migdistr_bplmatch]
  DT[,'migdistr_bplmatch':= NULL]
  

  ###########################################################################
  ###########################################################################  
  
  DT$byr <- DT$year-DT$age
  DT$bdy <- NA
  DT$bdy <- as.numeric(DT$bdy)
  
  k <- 1
  for (i in seq(1870, 2010, by = 10)) {
    DT[byr >= i & byr <= i+9, bdy := k]
    k <- k+1
  }
  
  DT[,c('byr') := NULL]
  
  
  DT[eckid > ec0, mobo :=1]
  DT[eckid <= ec0, mobo :=0]
  DT[ec0 != 0, mobo := NA]

  
  DT[,c('eckid', 'ec0') := NULL]
  
  ###########################################################################
  ###########################################################################  
  
  e1418 <- DT[age <= 18,
              .(yo_1418 = mean(mobo, na.rm = TRUE)),                  
              by = c('migdistr', 'bdy')]
  
  e1425 <- DT[age <= 25 & age >= 14,
              .(yo_1425 = mean(mobo, na.rm = TRUE)),                  
                by = c('migdistr', 'bdy')]
  
  e14100 <- DT[,
                .(yo_14100 = mean(mobo, na.rm = TRUE)),                  
                by = c('migdistr', 'bdy')]
  
  
  er1418 <- DT[!is.na(major_religion) & age <= 18,
              .(yor_1418 = mean(mobo, na.rm = TRUE)),                  
              by = c('major_religion', 'migdistr', 'bdy')]
  
  er1425 <- DT[!is.na(major_religion) & age <= 25 & age >= 14,
              .(yor_1425 = mean(mobo, na.rm = TRUE)),                  
              by = c('major_religion', 'migdistr', 'bdy')]
  
  er14100 <- DT[!is.na(major_religion),
               .(yor_14100 = mean(mobo, na.rm = TRUE)),                  
               by = c('major_religion', 'migdistr', 'bdy')]
  
  
  isoout <- merge(e1418, e1425, by = c('migdistr', 'bdy'), all.x = TRUE, all.y = TRUE)
  isoout <- merge(isoout, e14100, by = c('migdistr', 'bdy'), all.x = TRUE, all.y = TRUE)
  isooutr <- merge(er1418, er1425, by = c('migdistr', 'bdy', 'major_religion'), all.x = TRUE, all.y = TRUE)
  isooutr <- merge(isooutr, er14100, by = c('migdistr', 'bdy', 'major_religion'), all.x = TRUE, all.y = TRUE)

  isoout$iso <- iso
  setcolorder(isoout, 'iso')
  isooutr$iso <- iso
  setcolorder(isooutr, 'iso')
  
  
  if (iiso == 1) {
    out <- isoout
    outr <- isooutr
  } else {
    out <- rbind(out, isoout)
    outr <- rbind(outr, isooutr)
  }
  
}

######################################################################
######################################################################

for (iiso in 1:niso) {
  
  iso <- ISO[iiso]
  
  print(sprintf('###################### %s ######################', iso))
  
  setwd(indir_edu)
  dat <- as.data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  dat <- dat[, c('year', 'serial', 'pernum', 'age')]
  setwd(indir_rel)
  rel <- as.data.table(readRDS(sprintf('religions_%s.rds', iso)))
  rel <- rel[, c('year', 'serial', 'pernum', 'major_religion')]
  dat <- merge(dat, rel, by=c('year', 'serial', 'pernum'), all=TRUE)
  rm(rel)
  
  dat <- dat[!is.na(age)]
  dat <- dat[!is.na(major_religion)]
  
  setcolorder(dat, c('year', 'serial', 'pernum'))
  
  setwd(indir_mig)
  migdists <- data.table(readRDS(sprintf('migdists_%s.rds', iso)))

  DT <- merge(dat, migdists, by = c('year', 'serial', 'pernum'))
  DT[,c('serial', 'pernum'):=NULL]
  
  rm(dat, migdists)
  
  DT <- DT[!is.na(migdistr) & migdistr == migdistr_bplmatch]
  DT[,'migdistr_bplmatch':= NULL]
  

  DT$byr <- DT$year-DT$age
  DT$bdy <- NA
  DT$bdy <- as.numeric(DT$bdy)
  
  k <- 1
  for (i in seq(1870, 2010, by = 10)) {
    DT[byr >= i & byr <= i+9, bdy := k]
    k <- k+1
  }
  
  DT[,c('byr') := NULL]
  
  
  ######################################################################
  ######################################################################
  
  nr0100 <- DT[,
               .(nr0100 = .N),                  
               by = c('major_religion', 'migdistr', 'bdy')]
  
  nr14100 <- DT[age >=14,
               .(nr14100 = .N),                  
               by = c('major_religion', 'migdistr', 'bdy')]
  
  nr1425 <- DT[age >=14 & age <= 25,
                .(nr1425 = .N),                  
                by = c('major_religion', 'migdistr', 'bdy')]
  
  nr1418 <- DT[age >=14 & age <= 18,
                .(nr1418 = .N),                  
                by = c('major_religion', 'migdistr', 'bdy')]
  
  
  isooutr <- merge(nr0100, nr14100, by = c('migdistr', 'bdy', 'major_religion'), all.x = TRUE, all.y = TRUE)
  isooutr <- merge(isooutr, nr1425, by = c('migdistr', 'bdy', 'major_religion'), all.x = TRUE, all.y = TRUE)
  isooutr <- merge(isooutr, nr1418, by = c('migdistr', 'bdy', 'major_religion'), all.x = TRUE, all.y = TRUE)
  
  isooutr$iso <- iso
  setcolorder(isooutr, 'iso')
  
  if (iiso == 1) {
    outrn <- isooutr
  } else {
    outrn <- rbind(outrn, isooutr)
  }
  
}


######################################################################
######################################################################
######################################################################

setwd(outpath)

outr <- merge(outr, outrn, by=c('iso', 'migdistr', 'bdy', 'major_religion'), all.x=TRUE, all.y=TRUE)

outr[,`:=`(x0100 = sum(nr0100, na.rm=TRUE),
           x14100 = sum(nr14100, na.rm=TRUE),
           x1425 = sum(nr1425, na.rm=TRUE),
           x1418 = sum(nr1418, na.rm=TRUE)), by=c('iso', 'migdistr', 'bdy')]

outr[major_religion=='Christian' | major_religion=='Muslim' | major_religion=='Traditional',
     `:=`(x0100_3 = sum(nr0100, na.rm=TRUE),
          x14100_3 = sum(nr14100, na.rm=TRUE),
          x1425_3 = sum(nr1425, na.rm=TRUE),
          x1418_3 = sum(nr1418, na.rm=TRUE)), by=c('iso', 'migdistr', 'bdy')]


outr[, `:=`(sr0100_5 = nr0100 / x0100,
            sr14100_5 = nr14100 / x14100,
            sr1425_5 = nr1425 / x1425,
            sr1418_5 = nr1418 / x1418,
            sr0100_3 = nr0100 / x0100_3,
            sr14100_3 = nr14100 / x14100_3,
            sr1425_3 = nr1425 / x1425_3,
            sr1418_3 = nr1418 / x1418_3)]


a <- outr[, c('iso', 'migdistr', 'bdy', 'major_religion', 'nr0100', 'nr14100', 'nr1425', 'nr1418')]
a[major_religion == 'Muslim', `:=`(m0100 = nr0100,
                                   m14100 = nr14100,
                                   m1425 = nr1425,
                                   m1418 = nr1418)]
a[is.na(m0100), m0100 := 0]
a[is.na(m14100), m14100 := 0]
a[is.na(m1425), m1425 := 0]
a[is.na(m1418), m1418 := 0]

a[, `:=`(m0100 = sum(m0100, na.rm=TRUE),
         m14100 = sum(m14100, na.rm=TRUE), 
         m1425 = sum(m1425, na.rm=TRUE), 
         m1418 = sum(m1418, na.rm=TRUE)), by=c('iso', 'migdistr', 'bdy')]

a[, `:=`(n0100_5 = sum(nr0100, na.rm=TRUE),
         n14100_5 = sum(nr14100, na.rm=TRUE), 
         n1425_5 = sum(nr1425, na.rm=TRUE), 
         n1418_5 = sum(nr1418, na.rm=TRUE)), by=c('iso', 'migdistr', 'bdy')]

a[major_religion=='Christian' | major_religion=='Muslim' | major_religion=='Traditional',
    `:=`(n0100_3 = sum(nr0100, na.rm=TRUE),
         n14100_3 = sum(nr14100, na.rm=TRUE), 
         n1425_3 = sum(nr1425, na.rm=TRUE), 
         n1418_3 = sum(nr1418, na.rm=TRUE)), by=c('iso', 'migdistr', 'bdy')]

a[is.na(n0100_5), n0100_5 := 0]
a[is.na(n14100_5), n14100_5 := 0]
a[is.na(n1425_5), n1425_5 := 0]
a[is.na(n1418_5), n1418_5 := 0]

a[is.na(n0100_3), n0100_3 := 0]
a[is.na(n14100_3), n14100_3 := 0]
a[is.na(n1425_3), n1425_3 := 0]
a[is.na(n1418_3), n1418_3 := 0]


a[, `:=`(smus0100_5 = m0100 / n0100_5, 
         smus14100_5 = m14100 / n14100_5, 
         smus1425_5 = m1425 / n1425_5, 
         smus1418_5 = m1418 / n1418_5,
         smus0100_3 = m0100 / n0100_3, 
         smus14100_3 = m14100 / n14100_3, 
         smus1425_3 = m1425 / n1425_3, 
         smus1418_3 = m1418 / n1418_3)
         ]


a <- a[, c('iso', 'migdistr', 'bdy',
           'smus0100_5', 'smus14100_5', 'smus1425_5', 'smus1418_5', 
           'smus0100_3', 'smus14100_3', 'smus1425_3', 'smus1418_3')]


outr[, c('nr0100', 'nr14100', 'nr1425', 'nr1418',
         'x0100', 'x14100', 'x1425', 'x1418', 
         'x0100_3', 'x14100_3', 'x1425_3', 'x1418_3'):=NULL]


summary(outr$sr0100_5)
summary(outr$sr14100_5)
summary(outr$sr1425_5)
summary(outr$sr1418_5)

summary(outr$sr0100_3)
summary(outr$sr14100_3)
summary(outr$sr1425_3)
summary(outr$sr1418_3)

a <- unique(a, by = c('iso', 'migdistr', 'bdy'))

out <- merge(out, a, by = c('iso', 'migdistr', 'bdy'), all.x=TRUE, all.y=TRUE)

saveRDS(out, 'migdist__envs_bybd_newec0.rds')
saveRDS(outr, 'migdist__envs_bybdrel_newec0.rds')











