rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "eth")

outpath <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "eth")

dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

setwd(inpath)
fs <- dir(inpath)[5:length(dir(inpath))]
conc <- fread("all_iso_eth_correspondence.csv")
conc[, eth_low:=as.numeric(eth_low)]

for (f in fs){
  # f <- fs[1]
  isoc <- substr(f,1,3)
  print(isoc)
  
  setwd(inpath)
  dt <- data.table(haven::read_dta(f))
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  conc_i <- conc[iso==isoc]
  keep <- c('year', 'serial', 'pernum')
  keep <- c(keep, c(unique(conc_i$ethlow_varname)))
  dt <- dt[, keep, with=FALSE]
  dt[, eth_low:=NA]
  dt[, eth_low:=as.numeric(eth_low)]
  for (yr in unique(conc_i$year)) {
    conc_i_y <- conc_i[year==yr]
    ethvar <- conc_i_y$ethlow_varname[1]
    dt[year==yr, eth_low:=get(ethvar)]
  }
  dt <- merge(dt, conc_i[, .(year, eth_low, eth_hrm)], 
              by=c('year', 'eth_low'), all.x=TRUE, all.y=FALSE)
  dt <- dt[order(year, serial, pernum), .(year, serial, pernum, eth_hrm)]
  
  setwd(outpath)
  saveRDS(dt, sprintf("ethnicities_%s.rds", isoc))
  print(NROW(dt[!is.na(eth_hrm)]))
}