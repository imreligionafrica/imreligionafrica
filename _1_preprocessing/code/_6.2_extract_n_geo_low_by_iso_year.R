rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

dir <- file.path(rootdir, "_1_preprocessing", "code")

indir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "geo_correspondences")

setwd(indir)

fs <- c()

for (f in dir(indir)) {
  if (grepl("corr_", f)) {
    isoc <- substr(f, 6, 8)
    yr <- as.integer(substr(f, 9, 12))
    print(substr(f, 6, 12))
    dt <- data.table(haven::read_dta(f))
    dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
    dt <- unique(dt, by='geo_low')
    dt <- dt[, .(iso=isoc, year=yr, n_dist=.N)]
    if (substr(f, 6, 12)=="BEN1979") {
      DT <- dt
    } else {
      DT <- rbind(DT, dt)
    }
  }
}

setwd(dir)

fwrite(DT, "n_geo_low_by_iso_year.csv")