rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")
indir_cor <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "gis_covariates")
indir_dis <- file.path(rootdir, "_1_preprocessing", "data", "standardized", "districts")
code_dir <- file.path(rootdir, "_1_preprocessing", "code")

setwd(wdir)

dt <- fread('_F_dist_religion_bch10.csv')
dt <- dt[, c('iso',
             'bch10',
             'major_religion',
             'district',
             'litomg_18',
             'n_litomg_18',
             'immg_18',
             'n_immg_18',
             'imdwmg_18',
             'n_imdwmg_18')]

dt[, `:=`(xlo = litomg_18 * n_litomg_18, xim = immg_18 * n_immg_18, ximdw = imdwmg_18 * n_imdwmg_18)]
dt[, bch10:=NULL]
dt <- dt[, .(lito = sum(xlo, na.rm=TRUE),
             im = sum(xim, na.rm=TRUE),
             imdw = sum(ximdw, na.rm=TRUE),
             nlito = sum(n_litomg_18, na.rm=TRUE),
             nim = sum(n_immg_18, na.rm=TRUE),
             nimdw = sum(n_imdwmg_18, na.rm=TRUE)),
         by=.(iso, district, major_religion)]
dt[, `:=`(lito = sum(lito), nlito = sum(nlito)), by=.(iso, district)]
dt[, `:=`(lito = lito/nlito, im = im/nim, imdw = imdw/nimdw)]
summary(dt[, im])
summary(dt[, imdw])
summary(dt[, lito])
dt[, 'nlito':=NULL]
dt <- dt[major_religion != 'Other' & major_religion != 'No Religion']

#####################################################################
# religion and industry shares

relshares <- fread("relshares_relfrag_replpol_district.csv")
indshares <- fread("indshares_district_cohort_1960_and_older.csv")[, c("smin", "n"):=NULL]
relshares <- relshares[, c("district", "major_religion", "shr_n", "frag_n")]
relshares_wide <- dcast(relshares[, c("district", "major_religion", "shr_n")],
                   district ~ major_religion, value.var = "shr_n", fun.aggregate = mean
                   )
relshares_wide <- relshares_wide[!is.na(district)]
relshares_wide <- relshares_wide[, c("district", "Christian", "Muslim", "Traditional")]
names(relshares_wide) <- c("district", "popshare_c", "popshare_m", "popshare_t")
relshares_wide <- merge(relshares_wide, 
                        unique(relshares[, c("district", "frag_n")], by="district"),
                        by="district")
names(indshares) <- c("district", "iso", "sagr1960", "sman1960", "sser1960", "surb1960")

shares_rel_ind <- merge(indshares, relshares_wide, by="district", all=TRUE)
shares_rel_ind <- shares_rel_ind[!is.na(district)]
shares_rel_ind[is.na(shares_rel_ind)] <- 0 

#####################################################################



setwd(indir_dis)
dist_concordance <- fread('__district_correspondence_fromgeocorr_toreindexed.csv')

setwd(code_dir)
whichvar_tab <- fread('for_conc_prov_corrdist_whichvar.csv')

dist_concordance[is.na(correlates_district), correlates_district:=99999]
dist_concordance[,aux := min(correlates_district, na.rm=TRUE), by=district]
dist_concordance <- dist_concordance[correlates_district==aux][, aux:=NULL]

dt <- merge(dt, dist_concordance, by=c('iso', 'district'), all.x = TRUE, all.y = FALSE)
dt <- merge(dt, whichvar_tab, by='iso', all.x = TRUE, all.y = FALSE)
dt[whichvar=='old', correlates_district:=old_district_id]
dt[, c('whichvar', 'old_district_id'):=NULL]


setwd(indir_cor)

setwd(indir_cor)
correlates <- data.table(haven::read_dta("gis_covariates.dta"))
correlates[] <- lapply(correlates, function(x) { attributes(x) <- NULL; x })


# 50% of the minimum value
correlates[pdHYDE50==0, pdHYDE50:=0.000025]
correlates[,ln_pdHYDE50:=log(pdHYDE50)]

correlates[iso3=='EGY'|iso3=='MAR'|iso3=='ZAF', intRRD:=NA]
correlates[iso3=='EGY'|iso3=='MAR', intRDS:=NA]
correlates[iso3=='EGY'|iso3=='MAR', intIMP:=NA]
correlates[iso3=='EGY'|iso3=='MAR', intPAV:=NA]

correlates[iso3=='EGY'|iso3=='MAR'|iso3=='ZAF', distRRD:=NA]
correlates[iso3=='EGY'|iso3=='MAR', distRDS60:=NA]
correlates[iso3=='EGY'|iso3=='MAR', distIMP60:=NA]
correlates[iso3=='EGY'|iso3=='MAR', distPAV60:=NA]

correlates[iso3=='EGY'|iso3=='MAR'|iso3=='ZAF', rrdlen:=NA]
correlates[iso3=='EGY'|iso3=='MAR', rdlen_any:=NA]
correlates[iso3=='EGY'|iso3=='MAR', rdlen_imp:=NA]
correlates[iso3=='EGY'|iso3=='MAR', rdlen_pav:=NA]

log_vars <- c('distCAP', 'distBORD', 'distCOAST',
              'distRRD', 'distRDS60', 'distIMP60',
              'distPAV60', 'distMIC', 'distMIP',
              'distEBRQC', 'distEMURC')

for (lv in log_vars) {
  correlates[, paste0('ln_', lv) := log(get(lv))]
}

correlates[, zsSUIT := log(1+zsSUIT)]
correlates[, zsMAL := log(1+zsMAL)]
correlates[, zsTR := log(zsTR)]

correlates <- correlates[, c('iso3',
                             'district',
                             'intOIL',
                             'intDIA',
                             'ln_distCAP',
                             'ln_distBORD',
                             'ln_distCOAST',
                             'zsSUIT',
                             'zsMAL',
                             'zsTR',
                             'ln_pdHYDE50',
                             'ln_distRRD',
                             'ln_distIMP60',
                             'ln_distMIC',
                             'ln_distMIP',
                             'ln_distEBRQC',
                             'ln_distEMURC')]


setnames(correlates, 'iso3', 'iso')
setnames(correlates, 'district', 'correlates_district')

dt <- merge(dt, correlates, by=c('iso', 'correlates_district'), all.x=TRUE, all.y=FALSE)

dt <- dt[!is.na(district)]

setwd(code_dir)

conc <- fread('conc_prov_dist.csv')
setwd(wdir)
dt <- merge(dt, conc, by=c('iso', 'district'), all.x=TRUE)
dt <- merge(dt, shares_rel_ind, by=c('iso', 'district'), all.x=TRUE)


fwrite(dt, '_data_for_distlevel_corrregs.csv')



