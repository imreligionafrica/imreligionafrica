rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

code_dir <- file.path(rootdir, "_1_preprocessing", "code")

dist_dir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")

prov_dir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")

ISO <- c("BEN", "BWA", "BFA", "CMR", "EGY", 
         "ETH", "GHA", "GIN", "KEN", "LSO", 
         "LBR", "MWI", "MLI", "MUS", "MAR", 
         "MOZ", "NGA", "RWA", "SEN", "SLE", 
         "ZAF", "SSD", "SDN", "TZA", "TGO", 
         "UGA", "ZMB", "ZWE")

setwd(dist_dir)
dist_concordance <- fread('__district_correspondence_fromgeocorr_toreindexed.csv')

setwd(code_dir)
whichvar_tab <- fread('for_conc_prov_corrdist_whichvar.csv')

for (isoc in ISO) {
  
  print(isoc)
  setwd(dist_dir)
  dist <- data.table(readRDS(sprintf("districts_%s.rds", isoc)))
  setwd(prov_dir)
  prov <- data.table(readRDS(sprintf("provinces_%s.rds", isoc)))
  
  dt <- merge(dist, prov, by=c('year', 'serial', 'pernum'))
  
  distconc_iso <- dist_concordance[iso==isoc]
  distconc_iso_check <- distconc_iso[!is.na(correlates_district) & correlates_district != district]
  whichvar_iso <- whichvar_tab[iso==isoc, whichvar]
  
  
  if ('correlates_district' %in% names(dt)) {
    dt <- dt[, c('year', 'serial', 'pernum', 'district', 'correlates_district', 'province')]
    dt <- unique(dt[ ,.(correlates_district, district, province)], by='correlates_district')
    dt[, old_district_id := NA]
    dt[, old_district_id := as.integer(old_district_id)]
  } else {
    distconc_iso <- distconc_iso[, .(district, correlates_district, old_district_id)]
    dt <- unique(dt[ ,.(district, province)], by='district')
    dt <- merge(dt, distconc_iso, by='district', all.x=TRUE)
  }
  
  dt[, iso:=isoc]
  
  if (whichvar_iso == 'old') {
    dt[, correlates_district := old_district_id]
  }
  dt <- dt[!is.na(correlates_district), .(iso, district, correlates_district, province)]

  if (isoc=='BEN') {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
}

setwd(code_dir)

fwrite(DT, 'conc_prov_corrdist.csv')