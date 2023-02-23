rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "edu")
outpath <- file.path(rootdir, "_1_preprocessing", "code")

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "KEN", "LBR",
         "LSO", "MAR", "MLI", "MOZ", "MUS",
         "MWI", "NGA", "RWA", "SDN", "SEN", 
         "SLE", "SSD", "TGO", "TZA", "UGA", 
         "ZAF", "ZMB", "ZWE")

setwd(inpath)

for (iiso in 1:length(ISO)) {
  
  isoc <- ISO[iiso]
  print(isoc)
  
  dt <- data.table(haven::read_dta(sprintf('%s_edu.dta', isoc)))
  
  ls <- data.table(attributes(dt$related)$labels)
  ls[, V2:= names(attributes(dt$related)$labels)]
  dt <- dt[, .(related)]
  dt <- unique(dt, by='related')
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  ls[] <- lapply(ls, function(x) { attributes(x) <- NULL; x })
  names(ls) <- c('related', 'related_str')
  dt <- merge(dt, ls, by='related', all.x=TRUE, all.y=FALSE)
  
  dt[, iso := isoc]
  dt <- dt[, c('iso', 'related', 'related_str')]
  
  if (iiso == 1) {
    DT <- dt
  } else {
    DT <- rbind(DT, dt)
  }
  
}

DT <- DT[order(iso, related, related_str)]

setwd(outpath)

fwrite(DT, "related_codebook_new.csv")


