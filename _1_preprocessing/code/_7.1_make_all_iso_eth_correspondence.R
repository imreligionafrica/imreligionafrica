rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the console

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

inpath <- file.path(rootdir, "_1_preprocessing", "code")

outpath <- file.path(rootdir, "_1_preprocessing", "data", "raw", "eth")

setwd(inpath)

sheets <- readxl::excel_sheets('ethnicity_correspondence.xlsx')


for (sheet in sheets) {
  if (grepl("hrm", sheet) == FALSE) {
    dt <- data.table(readxl::read_excel('ethnicity_correspondence.xlsx', sheet=sheet))
    isoc <- substr(sheet, 1, 3)
    yr <- as.integer(substr(sheet, 4, 7))

    names(dt) <- sapply(names(dt), tolower)
    elvn <- names(dt)[1]
    names(dt) <- c('eth_low', 'eth_low_name', 'eth_hrm', 'eth_hrm_name')
    dt[, `:=`(iso = isoc, year = yr, ethlow_varname = elvn)]
    setcolorder(dt, c('iso', 'year', 'ethlow_varname'))
    dt[eth_hrm=='.', eth_hrm:=NA]
    dt[eth_hrm_name=='Missing', eth_hrm_name:=NA]
    dt[, eth_hrm := as.numeric(eth_hrm)]
    if (sheet == "BEN1979") {
      DT <- dt
    } else {
      DT <- rbind(DT, dt)
    }
  }
}

setwd(outpath)

fwrite(DT, 'all_iso_eth_correspondence.csv')