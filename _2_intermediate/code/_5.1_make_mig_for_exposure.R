rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(foreign)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
indir_mys <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migyrs")
indir_bas <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")
outpath <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "CMR", "EGY",
         "ETH", "GHA", "GIN",
         "MLI", "MWI", "RWA",
         "UGA", "ZAF", "ZMB",
         "TGO")

iiso <- 1

for (iiso in 1:13) {
  
  iso <- ISO[iiso]
  
  print(sprintf('###################### %s ######################', iso))
  
  setwd(indir_bas)
  dat <- data.table(readRDS(sprintf('basedat_%s.rds', iso)))
  setwd(indir_edu)
  ne <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', iso)))
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', iso)))
  rel[, c('harmonized_religion', 'religiond', 'religiond_str'):=NULL]
  
  ne[, c('related', 'momloc', 'poploc', 'famcode', 
         'aunc', 'ysc0', 'yscmom', 'yscpop', 
         'ecunc', 'yscunc', 'ysc'):=NULL]
  setcolorder(ne, c('year', 'serial', 'pernum'))
  

  dat <- merge(dat, ne, by = c('year', 'serial', 'pernum'))
  dat <- merge(dat, rel, by = c('year', 'serial', 'pernum'))
  rm(rel, ne)
  
  dat <- dat[!is.na(age)]
  dat <- dat[!is.na(eckid)]
  dat <- dat[age >= 14]
  dat <- dat[major_religion!='']
  dat <- dat[!is.na(ec0) & !is.na(a0)]
  dat[, apar := NA]
  dat[, apar := as.numeric(apar)]
  dat[, ecpar := NA]
  dat[, ecpar := as.numeric(ecpar)]
  dat[!is.na(amom) & is.na(apop), apar := amom]
  dat[!is.na(apop) & is.na(amom), apar := apop]
  dat[!is.na(apop) & !is.na(amom), apar := (amom+apop)/2]
  dat[!is.na(ecmom) & is.na(ecpop), ecpar := ecmom]
  dat[!is.na(ecpop) & is.na(ecmom), ecpar := ecpop]
  dat[!is.na(ecpop) & !is.na(ecmom), ecpar := (ecmom+ecpop)/2]

  dat$iso3 <- iso
  
  dat <- dat[,list(iso3, year, serial, pernum, age, major_religion, male, urban,
                   a0, apar, eckid, ec0, ecpar, gencode)]
  dat[!is.na(ec0) & age <= 25, ngen0 := uniqueN(gencode), by=.(year, serial)]
  dat[!is.na(ecpar) & age <= 25, ngenpar := uniqueN(gencode), by=.(year, serial)]
  dat[, `:=`(ngen0 = max(ngen0, na.rm=TRUE),
             ngenpar = max(ngenpar, na.rm=TRUE)),
      by=.(year,serial)]
  dat[, `:=`(mg01425 = as.integer(ngen0 > 1),
             mgpar1425 = as.integer(ngenpar > 1))]
  dat[, c('gencode', 'ngen0', 'ngenpar'):=NULL]
  
  
  setcolorder(dat, c('iso3', 'year', 'serial', 'pernum'))
  
  setwd(indir_mig)
  migdists <- data.table(readRDS(sprintf('migdists_%s.rds', iso)))
  DT <- merge(dat, migdists, by = c('year', 'serial', 'pernum'))
  
  rm(dat, migdists)

  setwd(indir_mys)
  migyrs <- data.table(readRDS(sprintf('%s_migyrs.rds', iso)))
  
  DT <- merge(DT, migyrs, by = c('year', 'serial', 'pernum'))
  rm(migyrs)
  
  DT <- DT[!is.na(migdistr) &  !is.na(migdistr_bplmatch)]

  DT[migyrs > 95, migyrs := NA]
  
  ###########################################################################
  ###########################################################################   
  
  DT <- DT[migdistr != migdistr_bplmatch]
  DT[,ageatmig := age - migyrs]
  DT[,migyrs := NULL]
  DT <- DT[!is.na(ageatmig) & ageatmig != 0]
  
  ###########################################################################
  ###########################################################################  
  
  DT$byr <- DT$year-DT$age
  DT$byro <- DT$year-DT$a0
  DT$byrpar <- DT$year-DT$apar
  DT$bdy <- NA
  DT$bdo <- NA
  DT$bdp <- NA
  DT$bdy <- as.numeric(DT$bdy)
  DT$bdo <- as.numeric(DT$bdo)
  DT$bdp <- as.numeric(DT$bdp)

  k <- 1
  for (i in seq(1870, 2010, by = 10)) {

    DT[byr >= i & byr <= i+9, bdy := k]
    DT[byro >= i & byro <= i+9, bdo := k]
    DT[byrpar >= i & byrpar <= i+9, bdp := k]
    
    k <- k+1
  }
  
  DT[,c('byr', 'a0', 'byro', 'apar', 'byrpar') := NULL]
  
  
  DT[eckid > ec0, mobo :=1]
  DT[eckid <= ec0, mobo :=0]
  DT[ec0 != 0, mobo := NA]
  
  DT[eckid > ecpar, mobpar :=1]
  DT[eckid <= ecpar, mobpar :=0]
  DT[ecpar != 0, mobpar := NA]
  
  DT[,c('eckid', 'ec0', 'ecpar') := NULL]
  
  ###########################################################################
  ###########################################################################  
  
  setwd(outpath)
  
  migenvs <- data.table(readRDS('migdist__envs_bybd_newec0.rds'))
  migenvs[, c('iso', 'smus14100_5', 'smus1418_5', 'smus14100_3', 'smus1418_3') := NULL]
  migenvsr <- data.table(readRDS('migdist__envs_bybdrel_newec0.rds'))
  migenvsr[, c('iso', 'sr14100_5', 'sr1418_5', 'sr14100_3', 'sr1418_3') := NULL]
  DT <- merge(DT, migenvs, by = c("migdistr", "bdy"))
  DT <- merge(DT, migenvsr, by = c("migdistr", "bdy", "major_religion"))
  
  ns <- c('yo', 'yor')
  ns_all <- c('yo')
  ns_rel <- c('yor')
  ags <- c('1418', '1425', '14100')
  
  for (ag in ags) {
    for (n in ns_all) {
      setnames(migenvs, sprintf('%s_%s', n, ag), sprintf('%s_%s_bp', n, ag))
    } 
    for (n in ns_rel) {
      setnames(migenvsr, sprintf('%s_%s', n, ag), sprintf('%s_%s_bp', n, ag))
    } 
  }
  
  ns <- c('smus', 'sr')
  ns_all <- c('smus')
  ns_rel <- c('sr')
  ags <- c('0100_5', '1425_5', '0100_3', '1425_3')

  
  for (ag in ags) {
    for (n in ns_all) {
      setnames(migenvs, sprintf('%s%s', n, ag), sprintf('%s%s_bp', n, ag))
    } 
    for (n in ns_rel) {
      setnames(migenvsr, sprintf('%s%s', n, ag), sprintf('%s%s_bp', n, ag))
    } 
  }
  
  setnames(migenvs, 'migdistr', 'migdistr_bplmatch')
  setnames(migenvsr, 'migdistr', 'migdistr_bplmatch')
  DT <- merge(DT, migenvs, by = c("migdistr_bplmatch", "bdy"))
  DT <- merge(DT, migenvsr, by = c("migdistr_bplmatch", "bdy", "major_religion"))
  
  
  ns <- c('yo', 'yor')
  ags <- c('1418', '1425', '14100')
  
  for (n in ns) {
    for (ag in ags) {
      DT[, (sprintf('d_%s_%s', n, ag)) :=
           get(sprintf('%s_%s', n, ag)) - get(sprintf('%s_%s_bp', n, ag)) ]
      
      # DT[,sprintf('%s_%s', n, ag) := NULL]    
      # DT[,sprintf('%s_%s_bp', n, ag) := NULL]    
    } 
  }

  ns <- c('smus', 'sr')
  ags <- c('0100_5', '1425_5', '0100_3', '1425_3')
  
  for (n in ns) {
    for (ag in ags) {
      DT[, (sprintf('d_%s_%s', n, ag)) :=
           get(sprintf('%s%s', n, ag)) - get(sprintf('%s%s_bp', n, ag)) ]
      
      # DT[,sprintf('%s_%s', n, ag) := NULL]    
      # DT[,sprintf('%s_%s_bp', n, ag) := NULL]    
    } 
  }
  
  
  # setcolorder(DT, c('iso3', 'year', 'serial', 'pernum',
  #                   'migdistr', 'migdistr_bplmatch', 'age', 'male', 'urban',
  #                   'bdy', 'bdo', 'bdp', 'mobo', 'mobo_sc', 'mobp', 'mobp_sc'))
  
  rm(migenvs, migenvsr)
  
  
  setcolorder(DT, c('iso3', 'year', 'serial', 'pernum', 'major_religion',
                    'migdistr', 'migdistr_bplmatch', 'age', 'male', 'urban',
                    'bdy', 'bdo', 'bdp', 'mobo', 'mobpar', 'mg01425', 'mgpar1425', 'ageatmig'))
  
  if (iiso == 1) {
    outDT <- DT
  } else {
    outDT <- rbind(outDT, DT)   
  }
  

}

######################################################################
######################################################################
######################################################################

rm(DT)

outDT[, serial_new := as.numeric(as.factor(paste0(iso3, "_",
                                                  as.character(year), "_",
                                                  as.character(serial))))]
summary(outDT$serial_new)
outDT[,uniqueN(serial_new)]
# outDT[, serial_new:=NULL]
setcolorder(outDT, c('iso3', 'year', 'serial', 'pernum', 'serial_new', 'major_religion',
                       'migdistr', 'migdistr_bplmatch', 'age', 'male', 'urban',
                       'bdy', 'bdo', 'bdp', 'mobo', 'mobpar', 'mg01425', 'mgpar1425', 'ageatmig'))

outDT <- outDT[!is.na(mobo)]
saveRDS(outDT, '_mig_for_exposure_newecold.rds')


