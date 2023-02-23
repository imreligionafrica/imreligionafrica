rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

codedir <- file.path(rootdir, "_1_preprocessing", "code")

indir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts", "before_reindex")

outdir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

setwd(codedir)
approaches <- fread("geo_hrm_approaches.csv")

fs <- dir(indir)

add <- 0
for (f in fs) {
  
  # f <- fs[7]
  isoc <- substr(f, 11, 13)
  print(isoc)
  approach <- approaches[iso==isoc, geo][1]
  setwd(indir)
  dt <- data.table(readRDS(f))
  setnames(dt, 'district', 'old_district_id')
  dists <- dt[, .(old_district_id)]
  dists <- unique(dists, by='old_district_id')
  dists <- dists[order(old_district_id)]
  dists[, district := .I + add]
  dists[is.na(old_district_id), district:=NA]
  dt <- merge(dt, dists, by='old_district_id', all.x=TRUE)
  setcolorder(dt, c('year', 'serial', 'pernum', 'district'))
  have_cd <- FALSE
  if ('correlates_district' %in% names(dt)) {
    have_cd <- TRUE
  } else {
    dt[, correlates_district:=NA]
    dt[, correlates_district:=as.integer(correlates_district)]
  }
  dists <- unique(dt, by=c('district', 'correlates_district', 'old_district_id'))
  if (have_cd == FALSE) {
    dt[, correlates_district:=NULL]
  }
  dt[, 'old_district_id':=NULL]
  dists[, 'old_district_type' := approach]
  dists[, iso:=isoc]
  dists <- dists[, .(iso, district, old_district_id, correlates_district, old_district_type)]
  add <- add + NROW(unique(dists[!is.na(district)], by='district'))
  
  if (isoc=='BEN') {
    DISTS <- dists
  } else {
    DISTS <- rbind(DISTS, dists)
  }
  
  setwd(outdir)
  saveRDS(dt, sprintf('districts_%s.rds', isoc))
  
}

fwrite(DISTS, '__district_correspondence_fromgeocorr_toreindexed.csv')
