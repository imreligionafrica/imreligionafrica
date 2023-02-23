rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA",
         "SEN", "SLE", "TGO", "UGA", "ZAF",
         "ZMB")

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
indir_coh <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "cohabtypes")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

relstrs <- c("Head", "Spouse/partner", "Spouse", "Child",
             "Biological child", "Adopted child", "Stepchild",
             "Other relative", "Grandchild", "Grandchild or great grandchild", 
             "Parent/parent-in-law", "Parent", "Parent-in-law", 
             "Child-in-law", "Sibling/sibling-in-law", 
             "Sibling", "Sibling-in-law", "Grandparent",
             "Parent/grandparent/ascendant", "Aunt/uncle", 
             "Nephew/niece", "Cousin", "Other relative, not elsewhere classified", 
             "Non-relative", "Friend/guest/visitor/partner", "Visitor", 
             "Employee", "Domestic employee", "Foster child",
             "Group quarters", "Non-relative, n.e.c.", "Other relative or non-relative",
             "Unknown", "Missing Data")


relstrsabb <- c('Head', 'Sps/Prtnr', 'Spouse', 'Child',
                'BioCh', 'AdopCh', 'StepCh', 'Othrel',
                'Gdchild', 'Gch/Ggch', 'Prnt/PIL', 'Parent', 'PinL', 
                'CinL', 'Sibl/SIL', 'Sibl', 'SinL', 'Grandp', 'P/Gp/Asc', 
                'Aunt/Unc', 'Nep/Nie', 'Cousin', 'OR nec', 'Nonrel', 'Fnd/Gue/Vis',
                'Visit', 'Empl', 'Domemp', 'FosCh', 'qrpq', 'NR nec', 'OR/NR', 'unknwn', 'miss')

relcodes <- c(1000, 2000, 2100, 3000, 3100, 3200, 3300, 4000,
              4100, 4110, 4200, 4210, 4220, 4300, 4400,
              4410, 4430, 4500, 4600, 4700, 4810, 4820,
              4900, 5000, 5100, 5120, 5200, 5210, 5330,
              5600, 5900, 6000, 9999, NA)

gencodes <- c(0, 0, 0, 1, 1, 1, 1, NA, 2, 2, -1, -1, -1, 1, 0, 0 , 0, -2, -1, -1 , 1, 0 , NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA)

famcodes <- c(1, 1, 1, 1,
              1, 1, 1, 
              1, 1, 1,
              1, 1, 1, 
              1, 1, 
              1, 1, 1,
              1, 1,
              1, 1, 1,
              0, 0, 0,
              0, 0, 1,
              0, 0, 0,
              0, 0)

codes <- data.table(cbind(relstrs, relstrsabb, relcodes, gencodes, famcodes))
codes[, relcodes := as.integer(relcodes)]
codes[, gencodes := as.integer(gencodes)]
codes[, famcodes := as.integer(famcodes)]


codes <- codes[, c('relcodes', 'famcodes')]
names(codes) <- c('related', 'family')

for (iiso in 1:21) {
  
  isoc <- ISO[iiso]
  
  print(sprintf('############# %s #############', isoc))
  
  setwd(indir_coh)
  cohab <- data.table(readRDS(sprintf('cohabtypes_%s.rds', isoc)))
  setwd(indir_rel)
  rel <- data.table(readRDS(sprintf('religions_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'major_religion')]
  setwd(indir_edu)
  edu <- data.table(readRDS(sprintf('aey_old_mompop_unc_%s.rds', isoc)))[, c('year', 'serial', 'pernum', 'age')]
  setwd(indir_dis)
  dist <- data.table(readRDS(sprintf('districts_%s.rds', isoc)))
  dt <- merge(cohab, codes, by='related', all.x=TRUE)
  rm(cohab)
  dt[, unrecorded:=NULL]
  dt[, cohab_old := momonly+poponly+mompoponly+momothers+popothers+mompopothers+othersonly]
  dt[, nfammbr := sum(family), by=c('year', 'serial')]
  dt[, cohab := as.integer(nfammbr > 1)]
  dt[cohab_old > 0, cohab := 1]
  
  dt <- merge(dt, rel, by=c('year', 'serial', 'pernum'))
  dt <- merge(dt, edu, by=c('year', 'serial', 'pernum'))
  dt <- merge(dt, dist, by=c('year', 'serial', 'pernum'))
  
  rm(dist, rel, edu)
  
  dt[, bd := 10*((year-age)%/%10)]
  
  cohab_1418 <- dt[!is.na(major_religion) & age >= 14 & age <= 18,
                   .(all_1418 = .N,
                     cohab_any_1418 = sum(cohab),
                     cohab_old_1418 = sum(cohab_old)),
                   by=.(bd, year, major_religion)]
  
  cohab_1425 <- dt[!is.na(major_religion) & age >= 14 & age <= 25,
                   .(all_1425 = .N,
                     cohab_any_1425 = sum(cohab),
                     cohab_old_1425 = sum(cohab_old)),
                   by=.(bd, year, major_religion)]
  
  cohab_cbyr <- merge(cohab_1418, cohab_1425, by=c('bd', 'year', 'major_religion'), all=TRUE)
  
  
  cohab_1418 <- dt[!is.na(major_religion) & age >= 14 & age <= 18,
                   .(all_1418 = .N,
                     cohab_any_1418 = sum(cohab),
                     cohab_old_1418 = sum(cohab_old)),
                   by=.(bd, year, district, major_religion)]
  
  cohab_1425 <- dt[!is.na(major_religion) & age >= 14 & age <= 25,
                   .(all_1425 = .N,
                     cohab_any_1425 = sum(cohab),
                     cohab_old_1425 = sum(cohab_old)),
                   by=.(bd, year, district, major_religion)]
  
  cohab_dbyr <- merge(cohab_1418, cohab_1425, by=c('bd', 'year', 'district', 'major_religion'), all=TRUE)
  
  cohabtypes_1418 <- dt[!is.na(major_religion) & age >= 14 & age <= 18,
                        .(momonly_1418 = sum(momonly),
                          poponly_1418 = sum(poponly),
                          mompoponly_1418 = sum(mompoponly),
                          momothers_1418 = sum(momothers),
                          popothers_1418 = sum(popothers),
                          mompopothers_1418 = sum(mompopothers),
                          othersonly_1418 = sum(othersonly)),
                        by=major_religion]
  
  cohabtypes_1425 <- dt[!is.na(major_religion) & age >= 14 & age <= 25,
                        .(momonly_1425 = sum(momonly),
                          poponly_1425 = sum(poponly),
                          mompoponly_1425 = sum(mompoponly),
                          momothers_1425 = sum(momothers),
                          popothers_1425 = sum(popothers),
                          mompopothers_1425 = sum(mompopothers),
                          othersonly_1425 = sum(othersonly)),
                        by=major_religion]
  
  cohabtypes <- merge(cohabtypes_1418, cohabtypes_1425, by='major_religion')
  
  
  cohab_cbyr[, iso := isoc]
  cohab_dbyr[, iso := isoc]
  cohabtypes[, iso := isoc]
  
  setcolorder(cohab_cbyr, 'iso')
  setcolorder(cohab_dbyr, 'iso')
  setcolorder(cohabtypes, 'iso')
  
  
  if (isoc == 'BEN') {
    COHAB_CBYR <- cohab_cbyr
    COHAB_DBYR <- cohab_dbyr
    COHABTYPES <- cohabtypes
  } else {
    COHAB_CBYR <- rbind(COHAB_CBYR, cohab_cbyr)
    COHAB_DBYR <- rbind(COHAB_DBYR, cohab_dbyr)
    COHABTYPES <- rbind(COHABTYPES, cohabtypes)
  }

}

setwd(outdir)
fwrite(COHAB_CBYR, '_cohab_cbyr.csv')
fwrite(COHAB_DBYR, '_cohab_dbyr.csv')
fwrite(COHABTYPES, '_cohabtypes.csv')




