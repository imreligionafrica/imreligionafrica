rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "other")

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
  
  dt <- data.table(haven::read_dta(sprintf('%s_other.dta', isoc)))
  
  if ('religiond' %in% names(dt)) {
    
    ls <- data.table(attributes(dt$religiond)$labels)
    ls[, V2:= names(attributes(dt$religiond)$labels)]
    dt <- dt[, .(religiond)]
    dt <- unique(dt, by='religiond')
    dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
    ls[] <- lapply(ls, function(x) { attributes(x) <- NULL; x })
    names(ls) <- c('religiond', 'religiond_str')
    dt <- merge(dt, ls, by='religiond', all.x=TRUE, all.y=FALSE)
    
    dt[, iso := isoc]
    dt <- dt[, c('iso', 'religiond', 'religiond_str')]
  
    if (iiso == 1) {
      DT <- dt
    } else {
      DT <- rbind(DT, dt)
    }
    
      
  } else {
    
    rm(dt)
    
  } 
  
}

DT <- DT[order(iso, religiond, religiond_str)]

setwd(outpath)

fwrite(DT, "religion_codebook_new.csv")


