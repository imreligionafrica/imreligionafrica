rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "other")

outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")

codepath <- file.path(rootdir, "_1_preprocessing", "code")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

setwd(codepath)

conc <- fread('religion_harmonization_concordance_v1_2.csv')

for (iiso in 1:length(ISO)) {
  
  # iiso <- 3
  
  setwd(inpath)
  
  isoc <- ISO[iiso]
  print(isoc)
  
  dt <- data.table(haven::read_dta(sprintf('%s_other.dta', isoc)))
  conc_iso <- conc[iso==isoc]
  conc_iso[, iso:=NULL]
  
  if ('religiond' %in% names(dt)) {
    dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
    r0 <- dim(dt)[1]
    dt <- dt[, c('year', 'serial', 'pernum', 'religiond')]
    dt <- merge(dt, conc_iso, by='religiond')
    r1 <- dim(dt)[1]
    print(r0)
    print(r1)
    setcolorder(dt, c('year', 'serial', 'pernum'))
    dt <- dt[order(year, serial, pernum)]
    
    setwd(outpath)
    
    saveRDS(dt, sprintf('religions_%s.rds', isoc))
    
  } else {
    
    rm(dt)
    
  } 
  print('#######################################')
  
}

