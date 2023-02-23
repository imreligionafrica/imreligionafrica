rm(list=ls()) # this clears all variables from the workspace
cat("\014")   # this clears the consol

library(data.table)
library(lfe)
library(rstudioapi)  

rootdir <- dirname(dirname(dirname(getActiveDocumentContext()$path)))
wdir <- file.path(rootdir, "_2_intermediate", "data")
dir <- file.path(rootdir, "_1_preprocessing", "code")

dir.create(wdir, showWarnings = FALSE, recursive = TRUE)

setwd(dir)
conc <- unique(fread('conc_prov_dist.csv'), by='district')
conc[, iso:=NULL]

setwd(wdir)
dt <- fread('_F_dist_religion_bch10.csv')
dt <- dt[!is.na(district)]
dt <- merge(dt, conc, by='district', all.x=TRUE, all.y=FALSE)
dt <- dt[bch10 == 1980 | bch10 == 1990, 
         c('iso', 'province', 'district', 'bch10', 'major_religion', 
           'n_immg_18', 'immg_18', 
           'n_imdwmg_18', 'imdwmg_18',  
           'n_litomg_18', 'litomg_18')]

dt[, `:=`(xu = n_immg_18 * immg_18,
          xd = n_imdwmg_18 * imdwmg_18,
          xl = n_litomg_18 * litomg_18)]


dt[, c('immg_18', 'imdwmg_18', 'litomg_18', 'bch10'):=NULL]
dt <- dt[, .(xu = sum(xu, na.rm=TRUE),
             xd = sum(xd, na.rm=TRUE),
             xl = sum(xl, na.rm=TRUE),
             nu = sum(n_immg_18, na.rm=TRUE),
             nd = sum(n_imdwmg_18, na.rm=TRUE),
             nl = sum(n_litomg_18, na.rm=TRUE)),
             by = c('iso', 'province', 'district', 'major_religion')]

dt <- dt[nl != 0]
dt[, `:=`(up = xu / nu,
          dn = xd / nd,
          lt = xl / nl)]
dt[, `:=`(n = sum(nl),
          xxl = sum(xl)), by=c('iso', 'province', 'district')]
dt[, shr := nl/n]
dt[, ltd := xxl/n]
dt [, c('xu', 'xd', 'nu', 'nd', 'n', 'nl', 'xl', 'xxl') := NULL]
dt <- dt[order(iso, province, district, major_religion)]

# dt[, cbd:= as.integer(as.factor(paste0(iso, '_', as.character(bd))))]
dt[, niso := as.integer(as.factor(iso))]

diso <- dt[, .(ndist = uniqueN(district)), by=iso]
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


OUT <- data.table(matrix(NA, 3*2*2*3*2, 8))
names(OUT) <- c('weighted', 'direction', 'fe', 'share_lit_old', 'major_religion', 'b', 'se', 'n')
OUT[, weighted := as.character(weighted)]
OUT[, direction := as.character(direction)]
OUT[, fe := as.character(fe)]
OUT[, share_lit_old := as.character(share_lit_old)]
OUT[, major_religion := as.character(major_religion)]
OUT[, b := as.numeric(b)]
OUT[, se := as.numeric(se)]
OUT[, n := as.integer(n)]

OUT[, weighted := c(rep('yes', 3*3*2*2), rep('no', 3*3*2*2))]
OUT[, direction := rep(c(rep('up', 3*3*2), rep('down', 3*3*2)), 2)]
OUT[, fe := rep(rep(c(rep('country', 3*3), rep('province', 3*3)), 2), 2)]
OUT[, share_lit_old := rep(rep(rep(c(rep('none', 3), rep('overall', 3), rep('own', 3)) ,2), 2), 2)]
OUT[, major_religion := rep(c('christian', 'muslim', 'traditional'), 3*2*2*2)]

for (i in 1:NROW(OUT)) {
  
  wgt <- OUT[i, weighted]
  updn <- OUT[i, direction]
  feff <- OUT[i, fe]
  slo <- OUT[i, share_lit_old]
  mr <- OUT[i, major_religion]
  
  if (updn  == 'up') {
    lhs <- 'up'
  } else {
    lhs <- 'dn' 
  }
  
  if (mr == 'christian') {
    dtest <- dt[major_religion == 'Christian']
  } else if (mr == 'muslim') {
    dtest <- dt[major_religion == 'Muslim']
  } else if (mr == 'traditional') {
    dtest <- dt[major_religion == 'Traditional']
  }
  

  if (slo == 'none') {
    f <- paste0(lhs, ' ~ shr')
  } else if (slo == 'overall') {
    f <- paste0(lhs, ' ~ shr + ltd')
  } else if (slo == 'own') {
    f <- paste0(lhs, ' ~ shr + lt')
  }
  
  if (feff == 'country') {
    f <- paste0(f, ' | niso | 0 | province')
  } else if (feff == 'province') {
    f <- paste0(f, ' | province | 0 | province')
  }
  
  if (wgt == 'no') {
    m <- felm(as.formula(f), data=dtest)
  } else {
    m <- felm(as.formula(f), data=dtest, weights=dtest[, wt])
  }
  
  
  OUT[weighted == wgt & direction == updn & fe == feff & share_lit_old == slo & major_religion == mr, 
       `:=`(b=m$coefficients[1],
            se=sqrt(m$clustervcv[1,1]),
            n=m$N
            )
       ]
  
  
  
  
}

fwrite(OUT, '_distlevel_im_relshare_regressions.csv')

#############################################################
#############################################################
# residualizing for example scatter plots

# unweighted

# IM LEVELS ON OWN RELIGION SHARE - RESIDUALIZING OVERALL (ACROSS ALL RELIGIONS)
# SHARE LITERATE OLD IN THE DISTRICT

dt[, wgt := wt]
dt[, wt := NULL]

dtest <- dt[major_religion == 'Christian' & !is.na(shr) & !is.na(up) & !is.na(ltd)]
fx <- 'shr ~ ltd | niso | 0 | province' 
fy <- 'up ~ ltd | niso | 0 | province' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c <- data.table(mx$residuals, my$residuals)
names(c) <- c('xc', 'yc')

dtest <- dt[major_religion == 'Muslim' & !is.na(shr) & !is.na(up) & !is.na(ltd)]
fx <- 'shr ~ ltd | niso | 0 | province' 
fy <- 'up ~ ltd | niso | 0 | province' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m <- data.table(mx$residuals, my$residuals)
names(m) <- c('xm', 'ym')

c[, n := 1:NROW(c)]
m[, n := 1:NROW(m)]

OUT <- merge(c, m, by = 'n', all = TRUE)
OUT[, n:= NULL]

fwrite(OUT, '_distlevel_im_relshare_regressions_residuals_for_scatter_cm_level.csv')

##########################################################################################################

# IM LEVELS ON OWN RELIGION SHARE: NOT CONTROLLING FOR SHARE LITERATE OLD

dtest <- dt[major_religion == 'Christian' & !is.na(shr) & !is.na(up)]
fx <- 'shr ~ 1 | niso | 0 | province' 
fy <- 'up ~ 1 | niso | 0 | province' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c <- data.table(mx$residuals, my$residuals)
names(c) <- c('xc', 'yc')

dtest <- dt[major_religion == 'Muslim' & !is.na(shr) & !is.na(up)]
fx <- 'shr ~ 1 | niso | 0 | province' 
fy <- 'up ~ 1 | niso | 0 | province' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m <- data.table(mx$residuals, my$residuals)
names(m) <- c('xm', 'ym')

c[, n := 1:NROW(c)]
m[, n := 1:NROW(m)]

OUT <- merge(c, m, by = 'n', all = TRUE)
OUT[, n:= NULL]

fwrite(OUT, '_distlevel_im_relshare_regressions_residuals_for_scatter_cm_level_no_litcontrol.csv')

##########################################################################################################

# IM GAPS ON MUSLIM SHARE - RESIDUALIZING OVERALL SHARE LITERATE OLD AND
# THEN RELIGION SPECIFIC SHARE LITERATE OLD

c <-  dt[major_religion == 'Christian'][, c('niso', 'district', 'up', 'ltd', 'lt')]
m <-  dt[major_religion == 'Muslim'][, c('district', 'up', 'ltd', 'lt', 'shr')]
names(c) <- c('niso', 'district', 'upc', 'ltdc', 'ltc')
names(m) <- c('district', 'upm', 'ltdm', 'ltm', 'shrm')
dtest <- merge(c, m, by='district')
dtest[, gap := upc-upm]
dtest[, ltd := ltdc]
dtest <- dtest[!is.na(gap) & !is.na(district)][, c('niso', 'gap', 'ltdc', 'ltd', 'ltc', 'ltm', 'shrm')]
#dtest[, ltdc:= NULL]


fx <- 'shrm ~ ltd | niso | 0 | niso' 
fy <- 'gap ~ ltd | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
overall <- data.table(mx$residuals, my$residuals)
names(overall) <- c('xov', 'yov')


fx <- 'shrm ~ ltc + ltm | niso | 0 | niso' 
fy <- 'gap ~ ltc + ltm  | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
relspec <- data.table(mx$residuals, my$residuals)
names(relspec) <- c('xre', 'yre')


overall[, n := 1:NROW(overall)]
relspec[, n := 1:NROW(relspec)]

OUT <- merge(overall, relspec, by = 'n', all = TRUE)
OUT[, n:= NULL]

fwrite(OUT, '_distlevel_im_relshare_regressions_residuals_for_scatter_cm_gap.csv')

##########################################################################################################

# IM GAPS ON MUSLIM SHARE - NOT CONTROLLING FOR SHARE LITERATE OLD

c <-  dt[major_religion == 'Christian'][, c('niso', 'district', 'up')]
m <-  dt[major_religion == 'Muslim'][, c('district', 'up', 'shr')]
names(c) <- c('niso', 'district', 'upc')
names(m) <- c('district', 'upm', 'shrm')
dtest <- merge(c, m, by='district')
dtest[, gap := upc-upm]
dtest <- dtest[!is.na(gap) & !is.na(district)][, c('niso', 'gap', 'shrm')]
#dtest[, ltdc:= NULL]

fx <- 'shrm ~ 1 | niso | 0 | niso' 
fy <- 'gap ~ 1 | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
OUT <- data.table(mx$residuals, my$residuals)
names(OUT) <- c('x', 'y')

fwrite(OUT, '_distlevel_im_relshare_regressions_residuals_for_scatter_cm_gap_no_litcontrol.csv')

