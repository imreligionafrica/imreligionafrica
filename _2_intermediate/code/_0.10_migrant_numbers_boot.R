rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
indir_rel <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "rel")
indir_edu <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "edu")
indir_pw <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "perwt")
indir_mig <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "migdists")
outdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MWI", "NGA", "RWA", "SEN",
         "SLE", "TGO", "UGA", "ZAF", "ZMB",
         "MUS")

nboot = 1000

for (isoc in ISO) {

  print('########################################')
  print(isoc)
  print('########################################')
  
  setwd(indir_mig)
  m <- data.table(readRDS(sprintf("migdists_%s.rds", isoc)))
  setwd(indir_edu)
  e <- data.table(readRDS(sprintf("aey_old_mompop_unc_%s.rds", isoc)))
  setwd(indir_rel)
  r <- data.table(readRDS(sprintf("religions_%s.rds", isoc)))  
  setwd(indir_pw)
  p <- data.table(readRDS(sprintf('perwt_%s.rds', isoc)))
  
  if ('migdistr_bplmatch' %in% names(m)) {
    
    m <- m[, .(year, serial, pernum, migdistr, migdistr_bplmatch)]
    e <- e[, .(year, serial, pernum, age)]
    r <- r[, .(year, serial, pernum, major_religion)]
    
    e[, bd := floor((year-age)/10)  * 10]
    e <- e[bd==1980, .(year, serial, pernum)]
    
    m[, migrant := as.integer(migdistr != migdistr_bplmatch)]
    m[is.na(migdistr) | is.na(migdistr_bplmatch), migrant := NA]
    m[, `:=`(org = migdistr_bplmatch, dst = migdistr)][, c('migdistr', 'migdistr_bplmatch'):=NULL]
    m <- m[, .(year, serial, pernum, org, dst, migrant)]
    
    dt <- merge(p, e, by=c('year', 'serial', 'pernum'))
    dt <- merge(dt, r, by=c('year', 'serial', 'pernum'))
    dt <- merge(dt, m, by=c('year', 'serial', 'pernum'))
    rm(m, e, r, p)
    
    dt <- dt[major_religion %in% c('Christian', 'Muslim', 'Traditional', 'Other', 'No Religion') & !is.na(migrant)]
    
    for (ib in 1:nboot){
      
      if ( ib %% 100 == 0) {
        print(ib)
      }
      
      dtboot <- dt[sample(nrow(dt), nrow(dt), replace=TRUE)]
      xall <- dtboot[, .(w = sum(perwt)), by=.(year, major_religion, org, migrant)]
      x <- dcast(xall, year + org + major_religion ~ migrant, value.var='w', fill=0)
      names(x) = c('year', 'org', 'major_religion', 'w0', 'w1')
      
      x[, `:=`(tot = w0 + w1, shr = w1 / (w0 + w1))][
        , c('w0', 'w1') := NULL][
          , totorg := sum(tot), by=.(year, org)][
            , wshr := shr * tot / totorg][
              , total_org_migshr := sum(wshr), by=.(year, org)][
                , shr_dm := shr - total_org_migshr][
                  , c('totorg', 'wshr', 'total_org_migshr') := NULL][
                    , totrel := sum(tot), by=.(year, major_religion)][
                      , `:=`(wshr = shr * tot / totrel, wshr_dm = shr_dm * tot / totrel )]
      x <- x[, .(wshr = sum(wshr), wshr_dm = sum(wshr_dm)), by=.(year, major_religion)]
      x <- x[, .(wshr = mean(wshr), wshr_dm = mean(wshr_dm)), by=.(major_religion)]
      x[, iboot := ib]
      
      if ( ib == 1) {
        X <- x
      } else {
        X <- rbind(X, x)
      }
    }
  
    X[, iso := isoc]

    if (isoc == 'BEN') {
      OUT <- X
    } else {
      OUT <- rbind(OUT, X)
    }

  }
  
}

setwd(outdir)

setcolorder(OUT, c('iso', 'major_religion', 'iboot'))

# x <- dcast(OUT[iso=='BEN', c('major_religion', 'iboot', 'wshr')], iboot ~ major_religion, value.var='wshr')
# x[, diff := Christian - Muslim]
# hist(x[, diff])

fwrite(OUT, 'migrant_stock_shares_boot_1980.csv')



