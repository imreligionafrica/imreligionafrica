rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE)

setwd(wdir)

dt <- fread('_F_migdist_religion_bch10.csv')
dt <- dt[!is.na(migdistr)]
# conc <- unique(fread('conc_prov_dist.csv'), by='district')
# conc[, iso:=NULL]
# dt <- merge(dt, conc, by='district', all.x=TRUE, all.y=FALSE)
dt <- dt[bch10 == 1980 | bch10 == 1990, 
         c('iso', 'migdistr', 'bch10', 'major_religion', 
           'n', 'n_litomg_18')]
setnames(dt, 'n_litomg_18', 'nl')

migshares <- fread('migrant_shares_per_origin_for_regs.csv')
setnames(migshares, 'bd', 'bch10')
setnames(migshares, 'org', 'migdistr')
dt <- merge(dt, migshares, by = c('iso', 'migdistr', 'bch10', 'major_religion'))
rm(migshares)

dt[, `:=`(tot = sum(n, na.rm = TRUE),
          totl = sum(nl, na.rm = TRUE)),
   by=c('iso', 'migdistr', 'bch10')]
dt[, `:=`(shr = n/tot,
          shrl = nl/totl)]

dt[, c('n', 'nl', 'tot', 'totl'):=NULL]

dt <- dt[, .(relshr = mean(shr, na.rm=TRUE),
             relshrl = mean(shrl, na.rm=TRUE),
             migshr_nofe = mean(shr_nofe, na.rm=TRUE),
             migshr_uwfe = mean(shr_uwfe, na.rm=TRUE),
             migshr_wfe = mean(shr_wfe, na.rm=TRUE)
             ),
         by = c('iso', 'migdistr', 'major_religion')]

dt <- dt[order(iso, migdistr, major_religion)]

# dt[, cbd:= as.integer(as.factor(paste0(iso, '_', as.character(bd))))]
dt[, niso := as.integer(as.factor(iso))]

diso <- dt[, .(ndist = uniqueN(migdistr)), by=iso]
diso[, distshr := ndist / sum(ndist)]
diso[, ndist := NULL]


ISO <- c("BEN", "BFA", "BWA", "CMR", "EGY",
         "ETH", "GHA", "GIN", "LBR", "MLI",
         "MOZ", "MUS", "MWI", "NGA", "RWA", 
         "SEN", "SLE", "TGO", "UGA", "ZAF", 
         "ZMB")

pop <- fread('pop_world.csv')

pop <- pop[iso %in% ISO & year == 1980]
pop[, shrpop := pop / sum(pop)]
pop <- pop[, c('iso', 'shrpop')]

weights <- merge(pop, diso, by='iso')
weights[, wt := shrpop / distshr]
weights[, c('shrpop', 'distshr'):=NULL]

dt <- merge(dt, weights, by='iso')

rm(pop, diso, weights)


# 3 religions
# 2 country / province FE
# 2 up / down
# 3 share lit old
# 2 weighted/ unweighted


OUT <- data.table(matrix(NA, 3*2*2*3, 7))
names(OUT) <- c('weighted', 'relshr', 'migshr_fe', 'major_religion', 'b', 'se', 'n')
OUT[, weighted := as.character(weighted)]
OUT[, relshr := as.character(relshr)]
OUT[, migshr_fe := as.character(migshr_fe)]
OUT[, major_religion := as.character(major_religion)]
OUT[, b := as.numeric(b)]
OUT[, se := as.numeric(se)]
OUT[, n := as.integer(n)]

OUT[, weighted := c(rep('yes', 3*3*2), rep('no', 3*3*2))]
OUT[, relshr := rep(c(rep('basic', 3*3), rep('lito', 3*3)), 2)]
OUT[, migshr_fe := rep(rep(rep(c(rep('none', 3), rep('unweighted', 3), rep('weighted', 3)) ,2), 2))]
OUT[, major_religion := rep(c('christian', 'muslim', 'traditional'), 3*2*2)]

for (i in 1:NROW(OUT)) {
  
  wgt <- OUT[i, weighted]
  relsh <- OUT[i, relshr]
  msfe <- OUT[i, migshr_fe]
  mr <- OUT[i, major_religion]
  
  if (relsh  == 'basic') {
    rhs <- 'relshr'
  } else {
    rhs <- 'relshrl' 
  }
  
  if (mr == 'christian') {
    dtest <- dt[major_religion == 'Christian']
  } else if (mr == 'muslim') {
    dtest <- dt[major_religion == 'Muslim']
  } else if (mr == 'traditional') {
    dtest <- dt[major_religion == 'Traditional']
  }
  
  
  if (msfe == 'none') {
    f <- paste0('migshr_nofe ~ ', rhs)
  } else if (msfe == 'unweighted') {
    f <- paste0('migshr_uwfe ~ ', rhs)
  } else if (msfe == 'weighted') {
    f <- paste0('migshr_wfe ~ ', rhs)
  }
  
  f <- paste0(f, ' | niso | 0 | migdistr')
  
  # if (feff == 'country') {
  #   f <- paste0(f, ' | niso | 0 | province')
  # } else if (feff == 'province') {
  #   f <- paste0(f, ' | province | 0 | province')
  # }
  
  if (wgt == 'no') {
    m <- felm(as.formula(f), data=dtest)
  } else {
    m <- felm(as.formula(f), data=dtest, weights=dtest[, wt])
  }
  
  
  OUT[weighted == wgt & relshr == relsh & migshr_fe == msfe & major_religion == mr, 
      `:=`(b=m$coefficients[1],
           se=sqrt(m$clustervcv[1,1]),
           n=m$N
      )
  ]
  
  
}


fwrite(OUT, '_distlevel_mig_relshare_regressions.csv')

#############################################################
#############################################################
# residualizing for example scatter plots

# unweighted

dt[, wgt := wt]
dt[, wt := NULL]

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_nofe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_nofe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_nofe <- data.table(mx$residuals, my$residuals)
names(c_nofe) <- c('xc_nofe', 'yc_nofe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_nofe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_nofe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m_nofe <- data.table(mx$residuals, my$residuals)
names(m_nofe) <- c('xm_nofe', 'ym_nofe')

c_nofe[, n := 1:NROW(c_nofe)]
m_nofe[, n := 1:NROW(m_nofe)]

######

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_uwfe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_uwfe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_uwfe <- data.table(mx$residuals, my$residuals)
names(c_uwfe) <- c('xc_uwfe', 'yc_uwfe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_uwfe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_uwfe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m_uwfe <- data.table(mx$residuals, my$residuals)
names(m_uwfe) <- c('xm_uwfe', 'ym_uwfe')

c_uwfe[, n := 1:NROW(c_uwfe)]
m_uwfe[, n := 1:NROW(m_uwfe)]

#####

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_wfe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_wfe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_wfe <- data.table(mx$residuals, my$residuals)
names(c_wfe) <- c('xc_wfe', 'yc_wfe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_wfe)]
fx <- 'relshr ~ 1 | niso | 0 | 0' 
fy <- 'migshr_wfe ~ 1 | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m_wfe <- data.table(mx$residuals, my$residuals)
names(m_wfe) <- c('xm_wfe', 'ym_wfe')

c_wfe[, n := 1:NROW(c_wfe)]
m_wfe[, n := 1:NROW(m_wfe)]

OUT <- merge(c_nofe, m_nofe, by = 'n', all = TRUE)
OUT <- merge(OUT, c_uwfe, by = 'n', all = TRUE)
OUT <- merge(OUT, m_uwfe, by = 'n', all = TRUE)
OUT <- merge(OUT, c_wfe, by = 'n', all = TRUE)
OUT <- merge(OUT, m_wfe, by = 'n', all = TRUE)

OUT[, n:= NULL]

fwrite(OUT, '_distlevel_mig_relshare_regressions_residuals_for_scatter_cm_level.csv')

##########################################################################################################

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_nofe', 'relshr')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_nofe', 'relshr')]
names(c) <- c('niso', 'district', 'migc', 'shrc')
names(m) <- c('district', 'migm', 'shrm')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm)][, c('niso', 'gap', 'shrm', 'shrgap')]

fx <- 'shrm ~ 1 | niso | 0 | niso' 
fxg <- 'shrgap ~ 1 | niso | 0 | niso' 
fy <- 'gap ~ 1 | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
mxg <- felm(as.formula(fxg), data=dtest)
my <- felm(as.formula(fy), data=dtest)
nofe <- data.table(mx$residuals, mxg$residuals, my$residuals)
names(nofe) <- c('xnofe', 'xgnofe', 'ynofe')

nofe[, n := 1:NROW(nofe)]

######

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_uwfe', 'relshr')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_uwfe', 'relshr')]
names(c) <- c('niso', 'district', 'migc', 'shrc')
names(m) <- c('district', 'migm', 'shrm')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm)][, c('niso', 'gap', 'shrm', 'shrgap')]

fx <- 'shrm ~ 1 | niso | 0 | niso' 
fxg <- 'shrgap ~ 1 | niso | 0 | niso' 
fy <- 'gap ~ 1 | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
mxg <- felm(as.formula(fxg), data=dtest)
my <- felm(as.formula(fy), data=dtest)
uwfe <- data.table(mx$residuals, mxg$residuals, my$residuals)
names(uwfe) <- c('xuwfe', 'xguwfe', 'yuwfe')

uwfe[, n := 1:NROW(uwfe)]

######

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_wfe', 'relshr')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_wfe', 'relshr')]
names(c) <- c('niso', 'district', 'migc', 'shrc')
names(m) <- c('district', 'migm', 'shrm')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm)][, c('niso', 'gap', 'shrm', 'shrgap')]

fx <- 'shrm ~ 1 | niso | 0 | niso' 
fxg <- 'shrgap ~ 1 | niso | 0 | niso' 
fy <- 'gap ~ 1 | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
mxg <- felm(as.formula(fxg), data=dtest)
my <- felm(as.formula(fy), data=dtest)
wfe <- data.table(mx$residuals, mxg$residuals, my$residuals)
names(wfe) <- c('xwfe', 'xgwfe', 'ywfe')

wfe[, n := 1:NROW(wfe)]

######

OUT <- merge(nofe, uwfe, by = 'n', all = TRUE)
OUT <- merge(OUT, wfe, by = 'n', all = TRUE)
OUT[, n:= NULL]

fwrite(OUT, '_distlevel_mig_relshare_regressions_residuals_for_scatter_cm_gap.csv')
























