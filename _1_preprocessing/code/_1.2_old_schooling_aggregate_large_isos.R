rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

# check _3.1_find_ind_occ_aggregate_large_isos
# there everything (including folder creation and moving files
# is automated)

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

path <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")

setwd(path)

large_isos <- c('EGY', 'ETH', 'ZAF', 'GHA', 'KEN', 'MAR', 'MWI', 'TZA', 'UGA')
large_files <- list()
for (iso in large_isos) {
  large_files[[iso]] <- c()
}

for (f in dir(path)) {
  for (iso in large_isos) {
    if (grepl(paste0('aey_old_mompop_unc_', iso, '[0-9][0-9][0-9][0-9]'), f)) {
      large_files[[iso]] <- c(large_files[[iso]], f)
    }
  }
}

for (iso in large_isos) {
  i <- 1
  for (f in large_files[[iso]]) {
    print(f)
    dt <- data.table(readRDS(f))
    if (i == 1) {
      DT <- dt
    } else {
      DT <- rbind(DT, dt)
    }
    i <- i+1
    if (file.exists(f)) {
      file.remove(f)
    }
  }
  saveRDS(DT, sprintf('aey_old_mompop_unc_%s.rds', iso))
}