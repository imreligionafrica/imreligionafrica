rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

path <- file.path(rootdir, "_1_preprocessing", "data", "standardized","ind_occ")

comppath <- file.path(path, "year_components_large_isos")

dir.create(comppath, showWarnings = FALSE, recursive = TRUE)

setwd(path)

large_isos <- c('EGY', 'ETH', 'ZAF', 'GHA', 'KEN', 'MAR', 'MWI', 'TZA', 'UGA')
large_files <- list()
for (iso in large_isos) {
  large_files[[iso]] <- c()
}

for (f in dir(path)) {
  for (iso in large_isos) {
    if (grepl(paste0('new_ind_occ_', iso), f)) {
      large_files[[iso]] <- c(large_files[[iso]], f)
    }
  }
}

for (iso in large_isos) {
  i <- 1
  for (f in large_files[[iso]]) {
    print(f)
    dt <- data.table(readRDS(f))
    from_file <- file.path(path, f)
    to_file <- file.path(comppath, f)
    file.rename(from = from_file, to = to_file)
    if (i == 1) {
      DT <- dt
    } else {
      DT <- rbind(DT, dt)
    }
    i <- i+1
  }
  saveRDS(DT, sprintf('new_ind_occ_%s.rds', iso))
}

