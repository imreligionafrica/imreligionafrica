rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)

library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))

indir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "geo_correspondences")
altindir <- file.path(rootdir, "_1_preprocessing", "data", "raw", "georaw")
codedir <- file.path(rootdir, "_1_preprocessing", "code")
geoconcdir <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "geo_correspondences")
outdir_dists <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts", "before_reindex")
outdir_provs <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "provinces")
outdir_migdists <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")

dir.create(outdir_dists, showWarnings = FALSE, recursive = TRUE)
dir.create(outdir_provs, showWarnings = FALSE, recursive = TRUE)
dir.create(outdir_migdists, showWarnings = FALSE, recursive = TRUE)

setwd(codedir)
approaches <- fread("geo_hrm_approaches.csv")
setwd(geoconcdir)

corr_geo_low2gadm_1 <- data.table(readRDS("corr_geo_low2gadm_1.RDS"))
corr_pr_low2hrm <- data.table(readRDS("corr_pr_low2hrm.RDS"))
corr_bpl_low2hrm <- data.table(readRDS("ccorr_bpl_low2hrm.RDS"))
corr_geo_low2hrm <- data.table(readRDS("corr_geo_low2hrm.RDS"))     
corr_geo_low2geo_hrm <- data.table(readRDS("corr_geo_low2geo_hrm.RDS"))

ISO <- c("BEN", "BWA", "BFA", "CMR", "EGY", 
         "ETH", "GHA", "GIN", "KEN", "LSO", 
         "LBR", "MWI", "MLI", "MUS", "MAR", 
         "MOZ", "NGA", "RWA", "SEN", "SLE", 
         "ZAF", "SSD", "SDN", "TZA", "TGO", 
         "UGA", "ZMB", "ZWE")

setwd(indir)

fs <- dir()
idx <- grepl('corr_', fs)
fs <- fs[idx]

for (iiso in 1:28) {

  # iiso <- 8
  isoc <- ISO[iiso]
  print('#######################################')
  print(isoc)

  if (isoc == "GHA") {
    setwd(altindir)
    dt <- data.table(readRDS('correspondence_GHA_allyears_geo_low2000.rds'))
    dt <- dt[, .(year, serial, pernum, geo_low_2000)]
    names(dt) <- c('cyr', 'serial', 'pernum', 'geo_low')
    dt[, year := 2000]
    
    setwd(indir)
    
    dt_corr84 <- data.table(haven::read_dta("corr_GHA1984.dta"))
    dt_corr00 <- data.table(haven::read_dta("corr_GHA2000.dta"))
    dt_corr10 <- data.table(haven::read_dta("corr_GHA2010.dta"))
    dt_corr84[] <- lapply(dt_corr84, function(x) { attributes(x) <- NULL; x })
    dt_corr00[] <- lapply(dt_corr00, function(x) { attributes(x) <- NULL; x })
    dt_corr10[] <- lapply(dt_corr10, function(x) { attributes(x) <- NULL; x })
    dt_corr84[, geo_low:=NULL]
    dt_corr00[, geo_low:=NULL]
    dt_corr10[, geo_low:=NULL]
    
    
    # subsetting the correspondences for the country-year
    corr_bpl_low2hrm_84 <- corr_bpl_low2hrm[year==1984 & iso==isoc]
    corr_bpl_low2hrm_00 <- corr_bpl_low2hrm[year==2000 & iso==isoc]
    corr_bpl_low2hrm_10 <- corr_bpl_low2hrm[year==2010 & iso==isoc]
    corr_pr_low2hrm_00 <- corr_pr_low2hrm[year==2000 & iso==isoc]
    corr_bpl_low2hrm_84[, c('iso', 'year'):=NULL]
    corr_bpl_low2hrm_00[, c('iso', 'year'):=NULL]
    corr_bpl_low2hrm_10[, c('iso', 'year'):=NULL]
    corr_pr_low2hrm_00[, c('iso', 'year'):=NULL]
    # even though we use geo_low_2000 for the districts, we will need
    # geo_hrm for the covariates
    corr_geo_low2geo_hrm_00 <- corr_geo_low2geo_hrm[year==2000 & iso==isoc]
    corr_geo_low2geo_hrm_00[, iso:=NULL]
    # finally, we need the current location hrm match for 2000 (for migration)
    corr_geo_low2hrm_00 <- corr_geo_low2hrm[year==2000 & iso==isoc]
    corr_geo_low2hrm_00[, iso:=NULL]
    
    
    # add geo_hrm (for covariates) to full dataset
    dt <- merge(dt, corr_geo_low2geo_hrm_00, by=c('year', 'geo_low'), all.x=TRUE, all.y=FALSE)
    dt <- merge(dt, corr_geo_low2gadm_1, by=c('geo_low', 'year'), all.x=TRUE, all.y=FALSE)
    dt <- merge(dt, corr_geo_low2hrm_00, by=c('geo_low', 'year'), all.x=TRUE, all.y=FALSE)
    dt <- dt[, .(cyr, serial, pernum, geo_low, geo_hrm, gadm1_hrm, hrm)]
    setnames(dt, 'cyr', 'year')
    
    dists <- dt[, .(year, serial, pernum, geo_low, geo_hrm)]
    provs <- dt[, .(year, serial, pernum, gadm1_hrm)]
    migdists <- dt[, .(year, serial, pernum, hrm)]
    names(dists) <- c('year', 'serial', 'pernum', 'district', 'correlates_district')
    names(provs) <- c('year', 'serial', 'pernum', 'province')
    names(migdists) <- c('year', 'serial', 'pernum', 'migdistr')
    rm(dt)
    
    # birth places / previous residences
    dt_corr84 <- merge(dt_corr84, corr_bpl_low2hrm_84, by='bpl_low', all.x=TRUE, all.y=FALSE)
    setnames(dt_corr84, 'hrm', 'migdistr_bplmatch')
    dt_corr84[, bpl_low:=NULL]
    dt_corr00 <- merge(dt_corr00, corr_bpl_low2hrm_00, by='bpl_low', all.x=TRUE, all.y=FALSE)
    setnames(dt_corr00, 'hrm', 'migdistr_bplmatch')
    setnames(dt_corr00, 'pr_low_5', 'pr_low')
    dt_corr00 <- merge(dt_corr00, corr_pr_low2hrm_00, by='pr_low', all.x=TRUE, all.y=FALSE)
    dt_corr00[is.na(migdistr_bplmatch), migdistr_bplmatch:=hrm]
    dt_corr00[, c('bpl_low', 'pr_low', 'hrm'):=NULL]
    dt_corr10 <- merge(dt_corr10, corr_bpl_low2hrm_10, by='bpl_low', all.x=TRUE, all.y=FALSE)
    setnames(dt_corr10, 'hrm', 'migdistr_bplmatch')
    dt_corr10[, bpl_low:=NULL]
    dt_corr84[, year:=1984]
    dt_corr00[, year:=2000]
    dt_corr10[, year:=2010]
    dt_corr84 <- dt_corr84[order(serial, pernum), .(year, serial, pernum, migdistr_bplmatch)]
    dt_corr00 <- dt_corr00[order(serial, pernum), .(year, serial, pernum, migdistr_bplmatch)]
    dt_corr10 <- dt_corr10[order(serial, pernum), .(year, serial, pernum, migdistr_bplmatch)]
    dt_corr <- rbind(dt_corr84, dt_corr00, dt_corr10)
    migdists <- merge(migdists, dt_corr, by=c('year', 'serial', 'pernum'), all.x=FALSE, all.y=FALSE)
    
    rm(corr_bpl_low2hrm_00, corr_bpl_low2hrm_10, corr_bpl_low2hrm_84,
       corr_pr_low2hrm_00, corr_geo_low2geo_hrm_00,
       corr_geo_low2hrm_00, dt_corr, dt_corr00, dt_corr10, dt_corr84)    
    
    
  } else if (isoc == "TGO") {
    setwd(altindir)
    dt <- data.table(readRDS('correspondence_TGO_allyears_geo_low2010.rds'))
    dt <- dt[, .(year, serial, pernum, geo_low_2010)]
    names(dt) <- c('cyr', 'serial', 'pernum', 'geo_low')
    dt[, year := 2010]
    
    setwd(indir)
    dt_corr10 <- data.table(haven::read_dta("corr_TGO2010.dta"))
    dt_corr10[] <- lapply(dt_corr10, function(x) { attributes(x) <- NULL; x })
    dt_corr10[, geo_low:=NULL]
    
    # subsetting the correspondences for the country-year
    corr_pr_low2hrm_10 <- corr_pr_low2hrm[year==2010 & iso==isoc]
    corr_pr_low2hrm_10[, c('iso', 'year'):=NULL]
    # even though we use geo_low_2010 for the districts, we will need
    # geo_hrm for the covariates
    corr_geo_low2geo_hrm_10 <- corr_geo_low2geo_hrm[year==2010 & iso==isoc]
    corr_geo_low2geo_hrm_10[, iso:=NULL]
    # finally, we need the current location hrm match for 2000 (for migration)
    corr_geo_low2hrm_10 <- corr_geo_low2hrm[year==2010 & iso==isoc]
    corr_geo_low2hrm_10[, iso:=NULL]
    
    # add geo_hrm (for covariates) to full dataset
    dt <- merge(dt, corr_geo_low2geo_hrm_10, by=c('year', 'geo_low'), all.x=TRUE, all.y=FALSE)
    dt <- merge(dt, corr_geo_low2gadm_1, by=c('geo_low', 'year'), all.x=TRUE, all.y=FALSE)
    dt <- merge(dt, corr_geo_low2hrm_10, by=c('geo_low', 'year'), all.x=TRUE, all.y=FALSE)
    dt <- dt[, .(cyr, serial, pernum, geo_low, geo_hrm, gadm1_hrm, hrm)]
    setnames(dt, 'cyr', 'year')
    
    dists <- dt[, .(year, serial, pernum, geo_low, geo_hrm)]
    provs <- dt[, .(year, serial, pernum, gadm1_hrm)]
    migdists <- dt[, .(year, serial, pernum, hrm)]
    names(dists) <- c('year', 'serial', 'pernum', 'district', 'correlates_district')
    names(provs) <- c('year', 'serial', 'pernum', 'province')
    names(migdists) <- c('year', 'serial', 'pernum', 'migdistr')
    rm(dt)
    
    setnames(dt_corr10, 'pr_low_p', 'pr_low')
    dt_corr10 <- merge(dt_corr10, corr_pr_low2hrm_10, by='pr_low', all.x=TRUE, all.y=FALSE)
    setnames(dt_corr10, 'hrm', 'migdistr_bplmatch')
    dt_corr10[, c('pr_low'):=NULL]
    dt_corr10[, year:=2010]
    migdists <- merge(migdists, dt_corr10, by=c('year', 'serial', 'pernum'), all.x=TRUE, all.y=TRUE)
    
    rm(corr_geo_low2geo_hrm_10, corr_geo_low2hrm_10, corr_pr_low2hrm_10, dt_corr10)

  } else {
    setwd(indir)
    idx <- grepl(paste0('corr_', isoc), fs)
    fsi <- fs[idx]
    
    isoi <- 1
    # iterating over the year-files for the given country
    for (f in fsi) {
      
      # f <- fsi[1]
      yr <- as.integer(substr(f, 9, 12))
      
      # subsetting the correspondences for the country-year
      corr_bpl_low2hrm_i <- corr_bpl_low2hrm[year==yr & iso==isoc]
      corr_pr_low2hrm_i <- corr_pr_low2hrm[year==yr & iso==isoc]
      corr_geo_low2hrm_i <- corr_geo_low2hrm[year==yr & iso==isoc]
      corr_geo_low2geo_hrm_i <- corr_geo_low2geo_hrm[year==yr & iso==isoc]
      
      dt <- data.table(haven::read_dta(f))
      dt[] <- lapply(dt, function(x) { attributes(x) <- NULL; x })
      if ('_merge' %in% names(dt)) {
        dt[, '_merge':=NULL]
      }
      dt[, `:=`(year=yr, iso=isoc)]
      
      # getting the approach for this country-year
      approach_i <- approaches[year==yr & iso==isoc]$geo
      
      # merging districts, provinces and migdistricts by location
      dt <- merge(dt, corr_geo_low2geo_hrm_i, by=c('iso', 'geo_low', 'year'), all.x=TRUE, all.y=FALSE)
      if (approach_i == "hrm")  {
        dt <- merge(dt, corr_geo_low2hrm_i, by=c('iso', 'geo_low', 'year'), all.x=TRUE, all.y=FALSE)
        setnames(dt, 'geo_hrm', 'geo_hrm_correlates')
        setnames(dt, 'hrm', 'geo_hrm')
      }
      dt <- merge(dt, corr_geo_low2gadm_1, by=c('geo_low', 'year'), all.x=TRUE, all.y=FALSE)
      dt <- merge(dt, corr_geo_low2hrm_i, by=c('iso', 'geo_low', 'year'), all.x=TRUE, all.y=FALSE)
      
      # defining sub-files
      if ('geo_hrm_correlates' %in% names(dt)) {
        dists_i <- dt[, .(year, serial, pernum, geo_hrm, geo_hrm_correlates)]  
      } else {
        dists_i <- dt[, .(year, serial, pernum, geo_hrm)]
      }
      provs_i <- dt[, .(year, serial, pernum, gadm1_hrm)]
      migdists_i <- dt[, .(year, serial, pernum, hrm)]
      # naming columns
      if ('geo_hrm_correlates' %in% names(dt)) {
        names(dists_i) <- c('year', 'serial', 'pernum', 'district', 'correlates_district')
      } else {
        names(dists_i) <- c('year', 'serial', 'pernum', 'district')
      }
      names(provs_i) <- c('year', 'serial', 'pernum', 'province')
      names(migdists_i) <- c('year', 'serial', 'pernum', 'migdistr')
      # deleting some superfluous columns
      dt[, c('geo_low', 'geo_hrm', 'gadm1_hrm', 'hrm'):=NULL]
      if ('geo_hrm_correlates' %in% names(dt)) {
        dt[, 'geo_hrm_correlates':=NULL]
      }
      
      # getting the columns that define birthplaces / previous residences
      cols <- names(dt)
      bp_pr_cols <- cols[! cols %in% c('iso', 'year', 'serial', 'pernum')]
      
      # if have such columns
      if (length(bp_pr_cols) > 0) {
        # define vector for previous residence types
        yrs_pr <- c()
        # iterate over birthplace / previous residence columns
        for (col in bp_pr_cols) {
          print(col)
          # if the column contains the string 'pr_low'
          if (grepl('pr_low', col)) {
            # rename the column of the correspondence dataframe
            names(corr_pr_low2hrm_i) <- c('iso', 'year', col, 'hrm')
            # get the type of previous residence: '1, 5, 10, or p'
            yr_pr <- gsub("pr_low_(.+)", "\\1", col)
            # merge
            dt <- merge(dt, corr_pr_low2hrm_i, by=c('iso', 'year', col), all.x=TRUE, all.y=FALSE)
            # change column name of merged column to reflect previous residence type
            setnames(dt, 'hrm', paste0('pr_hrm_', yr_pr))
            # add previous residence type to vector of previous residence types 
            yrs_pr <- c(yrs_pr, yr_pr)  
          } else {
            # if column doesnt contain 'pr_low', it is a 'bpl_low' and the merge is easy
            dt <- merge(dt, corr_bpl_low2hrm_i, by=c('iso', 'year', 'bpl_low'), all.x=TRUE, all.y=FALSE)
            # change the column name of harmonized column
            setnames(dt, 'hrm', 'bpl_hrm')
          }
          # after each merge, drop the column on which we merged from merged dataframe
          dt[, (col):=NULL]
        } 
        # sort the yrs_pr from oldest to newest (starting with 'p' if there)
        yrs_pr <- sort(yrs_pr, decreasing=TRUE)
        # if birthplaces is available, make that migdistr_bplmatch
        if ('bpl_hrm' %in% names(dt)) {
          dt[, migdistr_bplmatch := bpl_hrm]
          dt[, bpl_hrm:=NULL]
        # otherwise initialize it as NA
        } else {
          dt[, migdistr_bplmatch := NA]
        }
        dt[, migdistr_bplmatch := as.numeric(migdistr_bplmatch)]
        # loop over the pr_ columns from oldest to newest and replace
        # missing migdistr_bplmatch with available data
        while (length(yrs_pr) > 0) {
          yr <- yrs_pr[1]
          bpcol <- paste0('pr_hrm_', yr)
          dt[is.na(migdistr_bplmatch), migdistr_bplmatch := get(bpcol)]
          yrs_pr <- yrs_pr[! yrs_pr %in% c(yr)]
          dt[, (bpcol):=NULL]
        }
        # merge birth places with migdists for that country-year
        migdists_i <- merge(migdists_i, dt, by=c('year', 'serial', 'pernum'))
        migdists_i <- migdists_i[, .(year, serial, pernum, migdistr, migdistr_bplmatch)]
      }
      
      # build up the country-level data from country-year level data
      if (isoi == 1) {
        dists <- dists_i
        provs <- provs_i
        migdists <- migdists_i
      } else {
        dists <- rbind(dists, dists_i, fill=TRUE)
        provs <- rbind(provs, provs_i, fill=TRUE)
        migdists <- rbind(migdists, migdists_i, fill=TRUE)
      }
      
      isoi <- isoi + 1
      
    }

  }
  
  setwd(outdir_dists)
  dists[] <- lapply(dists, function(x) { attributes(x) <- NULL; x })  # write the data  
  saveRDS(dists, sprintf("districts_%s.rds", isoc))

  setwd(outdir_provs)
  provs[] <- lapply(provs, function(x) { attributes(x) <- NULL; x })  # write the data  
  saveRDS(provs, sprintf("provinces_%s.rds", isoc))
  
  setwd(outdir_migdists)
  migdists[] <- lapply(migdists, function(x) { attributes(x) <- NULL; x })  # write the data  
  saveRDS(migdists, sprintf("migdists_%s.rds", isoc))
  
}