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
           'n', 'n_litomg_18', 'litomg_18')]
setnames(dt, 'n_litomg_18', 'nl')

migshares <- fread('migrant_shares_per_origin_for_regs.csv')
setnames(migshares, 'bd', 'bch10')
setnames(migshares, 'org', 'migdistr')
dt <- merge(dt, migshares, by = c('iso', 'migdistr', 'bch10', 'major_religion'))
rm(migshares)

dt[, xl := nl * litomg_18]

dt[, `:=`(tot = sum(n, na.rm = TRUE),
          totl = sum(nl, na.rm = TRUE)),
   by=c('iso', 'migdistr', 'bch10')]
dt[, `:=`(shr = n/tot,
          shrl = nl/totl)]

# dt[, c('n', 'nl', 'tot', 'totl'):=NULL]

dt <- dt[, .(relshr = mean(shr, na.rm=TRUE),
             relshrl = mean(shrl, na.rm=TRUE),
             migshr_nofe = mean(shr_nofe, na.rm=TRUE),
             migshr_uwfe = mean(shr_uwfe, na.rm=TRUE),
             migshr_wfe = mean(shr_wfe, na.rm=TRUE),
             nl = sum(nl, na.rm=TRUE),
             xl = sum(xl, na.rm=TRUE)),
         by = c('iso', 'migdistr', 'major_religion')]

dt <- dt[order(iso, migdistr, major_religion)]
dt[, lt := xl/nl]

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


#############################################################
#############################################################
# residualizing for example scatter plots

# unweighted

dt[, wgt := wt]
dt[, wt := NULL]

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_nofe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_nofe ~ lt | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_nofe <- data.table(mx$residuals, my$residuals)
names(c_nofe) <- c('xc_nofe', 'yc_nofe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_nofe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_nofe ~ lt | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m_nofe <- data.table(mx$residuals, my$residuals)
names(m_nofe) <- c('xm_nofe', 'ym_nofe')

c_nofe[, n := 1:NROW(c_nofe)]
m_nofe[, n := 1:NROW(m_nofe)]

######

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_uwfe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_uwfe ~ lt | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_uwfe <- data.table(mx$residuals, my$residuals)
names(c_uwfe) <- c('xc_uwfe', 'yc_uwfe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_uwfe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_uwfe ~ lt | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
m_uwfe <- data.table(mx$residuals, my$residuals)
names(m_uwfe) <- c('xm_uwfe', 'ym_uwfe')

c_uwfe[, n := 1:NROW(c_uwfe)]
m_uwfe[, n := 1:NROW(m_uwfe)]

#####

dtest <- dt[major_religion == 'Christian' & !is.na(relshr) & !is.na(migshr_wfe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_wfe ~ lt | niso | 0 | 0' 
mx <- felm(as.formula(fx), data=dtest)
my <- felm(as.formula(fy), data=dtest)
c_wfe <- data.table(mx$residuals, my$residuals)
names(c_wfe) <- c('xc_wfe', 'yc_wfe')

dtest <- dt[major_religion == 'Muslim' & !is.na(relshr) & !is.na(migshr_wfe) & !is.na(lt)]
fx <- 'relshr ~ lt | niso | 0 | 0' 
fy <- 'migshr_wfe ~ lt | niso | 0 | 0' 
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

fwrite(OUT, '_distlevel_mig_relshare_regressions_residuals_for_scatter_cm_level_litocontrol.csv')

##########################################################################################################

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_nofe', 'relshr', 'lt')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_nofe', 'relshr', 'lt')]
names(c) <- c('niso', 'district', 'migc', 'shrc', 'litoc')
names(m) <- c('district', 'migm', 'shrm', 'litom')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm) & !is.na(litoc) & !is.na(litom)][, c('niso', 'gap', 'shrm', 'shrgap', 'litoc', 'litom')]

fx <- 'shrm ~ litoc + litom | niso | 0 | niso' 
fxg <- 'shrgap ~ litoc + litom | niso | 0 | niso' 
fy <- 'gap ~ litoc + litom | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
mxg <- felm(as.formula(fxg), data=dtest)
my <- felm(as.formula(fy), data=dtest)
nofe <- data.table(mx$residuals, mxg$residuals, my$residuals)
names(nofe) <- c('xnofe', 'xgnofe', 'ynofe')

nofe[, n := 1:NROW(nofe)]

######

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_uwfe', 'relshr', 'lt')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_uwfe', 'relshr', 'lt')]
names(c) <- c('niso', 'district', 'migc', 'shrc', 'litoc')
names(m) <- c('district', 'migm', 'shrm', 'litom')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm) & !is.na(litoc) & !is.na(litom)][, c('niso', 'gap', 'shrm', 'shrgap', 'litoc', 'litom')]

fx <- 'shrm ~ litoc + litom | niso | 0 | niso' 
fxg <- 'shrgap ~ litoc + litom | niso | 0 | niso' 
fy <- 'gap ~ litoc + litom | niso | 0 | niso' 
mx <- felm(as.formula(fx), data=dtest)
mxg <- felm(as.formula(fxg), data=dtest)
my <- felm(as.formula(fy), data=dtest)
uwfe <- data.table(mx$residuals, mxg$residuals, my$residuals)
names(uwfe) <- c('xuwfe', 'xguwfe', 'yuwfe')

uwfe[, n := 1:NROW(uwfe)]

######

c <-  dt[major_religion == 'Christian'][, c('niso', 'migdistr', 'migshr_wfe', 'relshr', 'lt')]
m <-  dt[major_religion == 'Muslim'][, c('migdistr', 'migshr_wfe', 'relshr', 'lt')]
names(c) <- c('niso', 'district', 'migc', 'shrc', 'litoc')
names(m) <- c('district', 'migm', 'shrm', 'litom')
dtest <- merge(c, m, by='district')
dtest[, gap := migc-migm]
dtest[, shrgap := shrc-shrm]
dtest <- dtest[!is.na(gap) & !is.na(shrgap) & !is.na(shrm) & !is.na(litoc) & !is.na(litom)][, c('niso', 'gap', 'shrm', 'shrgap', 'litoc', 'litom')]

fx <- 'shrm ~ litoc + litom | niso | 0 | niso' 
fxg <- 'shrgap ~ litoc + litom | niso | 0 | niso' 
fy <- 'gap ~ litoc + litom | niso | 0 | niso' 
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

fwrite(OUT, '_distlevel_mig_relshare_regressions_residuals_for_scatter_cm_gap_litocontrol.csv')

