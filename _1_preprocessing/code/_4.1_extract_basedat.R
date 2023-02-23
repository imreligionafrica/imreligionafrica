rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "other")

outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "basedat")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

iiso <- 1

for (iiso in 1:28) {
  isoc <- ISO[iiso]
  
  print('#############################')
  print(isoc)
  setwd(inpath)
  
  dt <- data.table(haven::read_dta(sprintf('%s_other.dta', isoc)))
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  if ("urban" %in% names(dt)) {
    dt <- dt[, .(year, serial, pernum, sex, urban)]
    
    dt[urban==9, urban:=NA]
    dt[urban==1, urban:=0]
    dt[urban==2, urban:=1]
    print('URBAN')
    print(table(dt$urban))
    
  } else {
    dt <- dt[, .(year, serial, pernum, sex)]
  }
  
  print('SEX')
  print(table(dt$sex))
  dt[, male:=1]
  dt[sex==2, male:=0]
  dt[sex==9, male:=NA]
  dt[, sex:=NULL]
  print('MALE')
  print(table(dt$male))
  
  setwd(outpath)
  
  saveRDS(dt, sprintf("basedat_%s.rds", isoc))

}

# ls_sex <- data.table(attributes(dt$sex)$labels)
# ls_sex[, V2:= names(attributes(dt$sex)$labels)]
# ls_urban <- data.table(attributes(dt$urban)$labels)
# ls_urban[, V2:= names(attributes(dt$urban)$labels)]
# ls_sex[] <- lapply(ls_sex, function(x) { attributes(x) <- NULL; x })
# ls_urban[] <- lapply(ls_urban, function(x) { attributes(x) <- NULL; x })