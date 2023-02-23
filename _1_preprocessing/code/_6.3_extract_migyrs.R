rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "raw", "migyrs")

outdir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migyrs")

dir.create(outdir_mig, showWarnings = FALSE, recursive = TRUE)

fs <- dir(indir_mig)

for (f in fs) {

  setwd(indir_mig)
  
  dt <- data.table(haven::read_dta(f))
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  isoc <- substr(f, 1,3)
  fout <- paste0(substr(f, 1, 10), '.rds')
  print(isoc)
  if (isoc == 'ZAF') {
    dt <- dt[, .(year, serial, pernum, migyrs2)]
  } else {
    dt <- dt[, .(year, serial, pernum, migyrs1)]
  }
  names(dt) <- c('year', 'serial', 'pernum', 'migyrs')
  print(names(dt))

  setwd(outdir_mig)
  saveRDS(dt, fout)
  
}
  
