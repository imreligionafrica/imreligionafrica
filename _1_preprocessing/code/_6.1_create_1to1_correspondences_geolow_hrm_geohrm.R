rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

dir <- file.path(rootdir, "_1_preprocessing", "data")

indir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "geo_correspondences")

outdir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "geo_correspondences")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

setwd(indir)

force_1_1_correspondence <- function(dt, hrm_col, low_col, random) {
  wt_col <- paste0(hrm_col, '_wt')
  
  # getting rid of attributes
  dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
  n1 <- NROW(dt)
  
  # largest weight within a low_col
  dt[, maxwt := max(get(wt_col)), by=c('country', 'year', low_col)]
  dt <- dt[get(wt_col) == maxwt]
  n2 <- NROW(dt)
  
  # breaking ties
  dt[, n_geolow := .N, by=c('country', 'year', low_col)]
  if (random==TRUE) {
    # may want to do this randomly
    # as in the code below
    dt[, rand := sample(.N,.N, replace=FALSE), by=c('country', 'year', low_col)]
    dt[, maxrand := max(rand), by=c('country', 'year', low_col)]
    dt <- dt[rand == maxrand]  
  } else {
    # alternatively, to make the results reproducible, use the
    # minimum hrm_col [ID] within a low_col
    dt[, maxid := min(get(hrm_col)), by=c('country', 'year', low_col)]
    dt <- dt[get(hrm_col) == maxid]
  }
  n3 <- NROW(dt)
  
  print(sprintf("removed %s rows for < max area share", n1-n2))
  print(sprintf("removed %s rows by breaking ties", n2-n3))
  dt <- dt[, c('country', 'year', low_col, hrm_col), with=FALSE]
  names(dt) <- c('iso', 'year', low_col, hrm_col)
  
  return(dt)
  
}

####################
# geo_low to geo_hrm
####################
dt <- data.table(haven::read_dta("geo_hrm2geo_corr.dta"))
hrm_col <- 'geo_hrm'
low_col <- 'geo_low'
# dt1 <- force_1_1_correspondence(dt, hrm_col, low_col, FALSE)
# dt2 <- force_1_1_correspondence(dt, hrm_col, low_col, TRUE)
# names(dt1) <- c('iso', 'year', 'geo_low', 'geo_hrm_min')
# names(dt2) <- c('iso', 'year', 'geo_low', 'geo_hrm_rand')
# dtx <- merge(dt1, dt2, by=c('iso', 'year', 'geo_low'))
# dtx <- dtx[geo_hrm_min != geo_hrm_rand] 
corr_geo_low2geo_hrm <- force_1_1_correspondence(dt, hrm_col, low_col, FALSE)

####################
# geo_low to hrm
####################
dt <- data.table(haven::read_dta("hrm2geo_corr.dta"))
hrm_col <- 'hrm'
low_col <- 'geo_low'
corr_geo_low2hrm <- force_1_1_correspondence(dt, hrm_col, low_col, FALSE) 

####################
# pr_low to hrm
####################
dt <- data.table(haven::read_dta("hrm2pr_corr.dta"))
hrm_col <- 'hrm'
low_col <- 'pr_low'
corr_pr_low2hrm <- force_1_1_correspondence(dt, hrm_col, low_col, FALSE) 

####################
# pr_low to hrm
####################
dt <- data.table(haven::read_dta("hrm2bpl_corr.dta"))
hrm_col <- 'hrm'
low_col <- 'bpl_low'
corr_bpl_low2hrm <- force_1_1_correspondence(dt, hrm_col, low_col, FALSE) 


####################
# geo_low to gadm_1
####################
dt <- data.table(haven::read_dta("gadm1_hrm2geo_low.dta"))
print(NROW(dt) == NROW(unique(dt, by='geo_low')))
dt[, year := as.integer(year)]
corr_geo_low2gadm_1 <- dt[order(geo_low, year), .(geo_low, gadm1_hrm, year)]

setwd(outdir)

saveRDS(corr_geo_low2geo_hrm, "corr_geo_low2geo_hrm.RDS")
saveRDS(corr_geo_low2hrm, "corr_geo_low2hrm.RDS")
saveRDS(corr_pr_low2hrm, "corr_pr_low2hrm.RDS")
saveRDS(corr_bpl_low2hrm, "ccorr_bpl_low2hrm.RDS")
saveRDS(corr_geo_low2gadm_1, "corr_geo_low2gadm_1.RDS")
